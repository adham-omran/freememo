import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.PrintStream;
import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.security.KeyFactory;
import java.security.MessageDigest;
import java.security.PublicKey;
import java.security.Signature;
import java.security.spec.X509EncodedKeySpec;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Base64;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The bootstrap of a Local Install (ticket 19, plans/local-install/issues/19-app-update.md).
 *
 * It ships in the installer and an App Update never replaces it. It is the
 * jpackage main class, so the user's arguments (such as --no-tab) reach it.
 * It has no dependency but the JDK.
 *
 * It does this, in a loop:
 *   1. Pick the App Update jars in {data dir}/app that are newer than the
 *      bundled jar, carry a valid Ed25519 signature, and have no .failed
 *      marker. Newest first. The bundled jar is the last one.
 *   2. Start the app (freememo.local_install) as a child process with the
 *      bundled runtime, and wait for FREEMEMO_READY or FREEMEMO_SIGNIN_READY
 *      on its stdout. Every child line goes on to this process's stdout.
 *   3. When the child exits before READY, or READY does not come in time,
 *      count a failed start for that jar (file freememo-<v>.jar.starts), log
 *      why, and start the next jar. The second failed start in a row (crash or
 *      timeout) writes the .failed marker. A READY clears the count. One slow
 *      start (a cold disk, a first PostgreSQL initdb) so never rejects a good
 *      jar for good. When even the last jar fails, remove the markers and
 *      counts of this run: the cause is then not the jar.
 *   4. When the child exits with code 75 after READY (the tray's "Restart to
 *      update"), go back to step 1. Any other exit code ends this process.
 *
 * Log: every child line (stdout and stderr) and every bootstrap line also go
 * to {data dir}/freememo.log. A Windows launcher has no console, so without
 * the file the app's output is lost. At start, a log over 5 MB moves to
 * freememo.log.1, so two files at most stay.
 *
 * A launcher that finds the data directory locked starts one child with the
 * best jar and no fallback, so that a second start never marks a good jar.
 *
 * Signature: Ed25519 over the text
 *   "freememo-app-jar\n" + version + "\n" + sha256-hex(jar) + "\n"
 * with the public key (X.509 DER, base64) in the system property
 * freememo.update-public-key. Without the key no App Update jar is picked.
 *
 * System properties (tools/installer/build.mjs sets them; tests set them too):
 *   freememo.boot.appdir            directory of freememo.jar, postgres-binaries.jar
 *   freememo.boot.bundled-version   Release Version of the bundled jar
 *   freememo.update-public-key      see above
 *   freememo.boot.main-class        default freememo.local_install
 *   freememo.boot.ready-timeout-s   default 240. The app prints READY only after
 *                                   PostgreSQL started, and a first start runs
 *                                   initdb. Two failed starts are needed to
 *                                   mark a jar, so 240 s stays enough.
 *   freememo.boot.java              default java.home/bin/java
 */
public final class FreememoBoot {
  static final int RESTART_CODE = 75;
  static final String STATEMENT_HEAD = "freememo-app-jar\n";
  private static final Pattern JAR = Pattern.compile("freememo-(\\d+(?:\\.\\d+)*)\\.jar");

  private final Path appDir;
  private final String bundledVersion;
  private final PublicKey key;
  private final String mainClass;
  private final long readyTimeoutS;
  private final String java;
  private final Path updates;
  private final Path dataDir;
  private final List<String> args;
  private final PrintStream out;
  private volatile Process child;

  FreememoBoot(Path appDir, String bundledVersion, PublicKey key, String mainClass, long readyTimeoutS,
               String java, Path dataDir, List<String> args, PrintStream out) {
    this.appDir = appDir;
    this.bundledVersion = bundledVersion;
    this.key = key;
    this.mainClass = mainClass;
    this.readyTimeoutS = readyTimeoutS;
    this.java = java;
    this.dataDir = dataDir;
    this.updates = dataDir.resolve("app");
    this.args = args;
    this.out = out;
  }

  // ── Pure parts ───────────────────────────────────────────────────

  static String sha256(Path file) throws Exception {
    MessageDigest md = MessageDigest.getInstance("SHA-256");
    try (InputStream in = Files.newInputStream(file)) {
      byte[] buf = new byte[1 << 16];
      for (int n; (n = in.read(buf)) > 0; ) md.update(buf, 0, n);
    }
    StringBuilder sb = new StringBuilder();
    for (byte b : md.digest()) sb.append(String.format("%02x", b));
    return sb.toString();
  }

  static PublicKey parseKey(String base64) throws Exception {
    byte[] der = Base64.getMimeDecoder().decode(base64.trim());
    return KeyFactory.getInstance("Ed25519").generatePublic(new X509EncodedKeySpec(der));
  }

  /** True when `sig` is the signature of this jar as release `version`. Never throws. */
  static boolean verify(Path jar, Path sig, PublicKey key, String version) {
    try {
      if (key == null || !Files.isRegularFile(jar) || !Files.isRegularFile(sig)) return false;
      String statement = STATEMENT_HEAD + version + "\n" + sha256(jar) + "\n";
      Signature s = Signature.getInstance("Ed25519");
      s.initVerify(key);
      s.update(statement.getBytes(StandardCharsets.UTF_8));
      return s.verify(Files.readAllBytes(sig));
    } catch (Exception e) {
      return false;
    }
  }

  /** Negative, zero or positive, comparing dotted numbers; a missing part is 0. */
  static int compareVersions(String a, String b) {
    String[] x = a.split("\\."), y = b.split("\\.");
    for (int i = 0; i < Math.max(x.length, y.length); i++) {
      long p = i < x.length ? Long.parseLong(x[i]) : 0, q = i < y.length ? Long.parseLong(y[i]) : 0;
      if (p != q) return Long.compare(p, q);
    }
    return 0;
  }

  static Path sigOf(Path jar) { return jar.resolveSibling(jar.getFileName() + ".sig"); }
  static Path failedOf(Path jar) { return jar.resolveSibling(jar.getFileName() + ".failed"); }
  static Path startsOf(Path jar) { return jar.resolveSibling(jar.getFileName() + ".starts"); }

  /** Count one more failed start in a row for `jar`. Post: the new count. */
  static int countFailedStart(Path jar) throws IOException {
    int n = 0;
    try { n = Integer.parseInt(Files.readString(startsOf(jar)).trim()); } catch (Exception ignored) { }
    n++;
    Files.writeString(startsOf(jar), n + "\n");
    return n;
  }

  static String versionOf(Path jar) {
    Matcher m = JAR.matcher(jar.getFileName().toString());
    return m.matches() ? m.group(1) : null;
  }

  static Path defaultDataDir() {
    String home = System.getProperty("user.home");
    String os = System.getProperty("os.name", "").toLowerCase(Locale.ROOT);
    if (os.contains("mac")) return Paths.get(home, "Library", "Application Support", "FreeMemo Local");
    if (os.contains("win")) {
      String appdata = System.getenv("APPDATA");
      return Paths.get(appdata != null ? appdata : home, "FreeMemo Local");
    }
    String xdg = System.getenv("XDG_DATA_HOME");
    return Paths.get(xdg != null ? xdg : home + "/.local/share", "FreeMemo Local");
  }

  /**
   * The verified jars of {data dir}/app, newest first. A jar with a bad or
   * missing signature is deleted, with one log line. A jar that is not newer
   * than the bundled one, or that has a .failed marker, is left alone and not picked.
   */
  List<Path> pick() {
    List<Path> good = new ArrayList<>();
    if (key == null || !Files.isDirectory(updates)) return good;
    try (var stream = Files.list(updates)) {
      for (Path jar : (Iterable<Path>) stream::iterator) {
        String v = versionOf(jar);
        if (v == null || compareVersions(v, bundledVersion) <= 0 || Files.exists(failedOf(jar))) continue;
        if (verify(jar, sigOf(jar), key, v)) good.add(jar);
        else {
          log("deleting " + jar.getFileName() + ": the signature is not valid");
          try { Files.deleteIfExists(jar); Files.deleteIfExists(sigOf(jar)); } catch (IOException ignored) { }
        }
      }
    } catch (IOException e) {
      log("cannot read " + updates + ": " + e.getMessage());
    }
    good.sort((a, b) -> compareVersions(versionOf(b), versionOf(a)));
    return good;
  }

  // ── Running ──────────────────────────────────────────────────────

  void log(String line) {
    out.println("[boot] " + Instant.now() + " " + line);
    out.flush();
  }

  static final long LOG_MAX_BYTES = 5L << 20;

  /**
   * Post: a stream that writes to `console` and appends to {dataDir}/freememo.log.
   * A log over LOG_MAX_BYTES first moves to freememo.log.1. Without a writable
   * log, `console` alone, with one line that says why.
   */
  static PrintStream withLogFile(Path dataDir, PrintStream console) {
    try {
      Files.createDirectories(dataDir);
      Path log = dataDir.resolve("freememo.log");
      if (Files.exists(log) && Files.size(log) > LOG_MAX_BYTES) {
        Files.move(log, dataDir.resolve("freememo.log.1"), StandardCopyOption.REPLACE_EXISTING);
      }
      OutputStream file = Files.newOutputStream(log, StandardOpenOption.CREATE, StandardOpenOption.APPEND);
      OutputStream both = new OutputStream() {
        @Override public void write(int b) throws IOException { console.write(b); file.write(b); }
        @Override public void write(byte[] b, int off, int len) throws IOException { console.write(b, off, len); file.write(b, off, len); }
        @Override public void flush() throws IOException { console.flush(); file.flush(); }
        @Override public void close() throws IOException { flush(); file.close(); }
      };
      return new PrintStream(both, true, StandardCharsets.UTF_8);
    } catch (IOException e) {
      console.println("[boot] no log file in " + dataDir + ": " + e.getMessage());
      return console;
    }
  }

  private boolean dataDirLocked() {
    try {
      Files.createDirectories(dataDir);
      try (FileChannel ch = FileChannel.open(dataDir.resolve("launcher.lock"), StandardOpenOption.CREATE, StandardOpenOption.WRITE)) {
        FileLock lock;
        try { lock = ch.tryLock(); } catch (java.nio.channels.OverlappingFileLockException e) { return true; }
        if (lock == null) return true;
        lock.release();
        return false;
      }
    } catch (Exception e) {
      return false;
    }
  }

  private List<String> command(Path jar) throws IOException {
    List<String> cmd = new ArrayList<>();
    cmd.add(java);
    cmd.add("-Xss4m");
    cmd.add("-Dfreememo.bootstrap=true");
    for (String name : System.getProperties().stringPropertyNames()) {
      if ((name.startsWith("freememo.") && !name.equals("freememo.bootstrap")) || name.equals("jpackage.app-path")
          || name.equals("apple.awt.enableTemplateImages")) {
        cmd.add("-D" + name + "=" + System.getProperty(name));
      }
    }
    StringBuilder cp = new StringBuilder(jar.toString());
    try (var stream = Files.list(appDir)) {
      List<Path> others = new ArrayList<>();
      stream.forEach(p -> {
        String n = p.getFileName().toString();
        if (n.endsWith(".jar") && !n.equals("freememo.jar") && !n.equals("freememo-boot.jar")) others.add(p);
      });
      others.sort(null);
      for (Path p : others) cp.append(File.pathSeparator).append(p);
    }
    cmd.add("-cp");
    cmd.add(cp.toString());
    cmd.add(mainClass);
    cmd.addAll(args);
    return cmd;
  }

  /** What one child run came to. */
  static final class Outcome {
    volatile boolean ready;
    int exit = -1;
    String why;
  }

  private Outcome start(Path jar) throws Exception {
    Outcome o = new Outcome();
    ProcessBuilder pb = new ProcessBuilder(command(jar));
    // stderr joins stdout, so it reaches the log file too.
    pb.redirectErrorStream(true);
    Process p = pb.start();
    child = p;
    CountDownLatch done = new CountDownLatch(1);
    Thread pump = new Thread(() -> {
      try (BufferedReader r = new BufferedReader(new InputStreamReader(p.getInputStream(), StandardCharsets.UTF_8))) {
        for (String line; (line = r.readLine()) != null; ) {
          out.println(line);
          out.flush();
          if (line.startsWith("FREEMEMO_READY") || line.startsWith("FREEMEMO_SIGNIN_READY")) {
            o.ready = true;
            done.countDown();
          }
        }
      } catch (IOException ignored) {
      } finally {
        done.countDown();
      }
    }, "boot-stdout");
    pump.setDaemon(true);
    pump.start();
    boolean signalled = done.await(readyTimeoutS, TimeUnit.SECONDS);
    if (!o.ready) {
      if (signalled) {
        o.exit = p.waitFor();
        o.why = "the app exited with code " + o.exit + " before READY";
      } else {
        o.why = "no READY after " + readyTimeoutS + " s";
        p.destroy();
        if (!p.waitFor(20, TimeUnit.SECONDS)) p.destroyForcibly();
        o.exit = p.waitFor();
      }
      return o;
    }
    o.exit = p.waitFor();
    pump.join(2000);
    return o;
  }

  /** Post: the exit code of this process. */
  int run() throws Exception {
    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      Process p = child;
      if (p != null && p.isAlive()) {
        p.destroy();
        try { p.waitFor(30, TimeUnit.SECONDS); } catch (InterruptedException ignored) { }
      }
    }, "boot-quit"));
    Path bundled = appDir.resolve("freememo.jar");
    while (true) {
      boolean locked = dataDirLocked();
      List<Path> jars = pick();
      jars.add(bundled);
      if (locked) jars = jars.subList(0, 1);
      List<Path> marked = new ArrayList<>();
      Outcome o = null;
      boolean restart = false;
      for (int i = 0; i < jars.size(); i++) {
        Path jar = jars.get(i);
        boolean last = i == jars.size() - 1;
        if (!jar.equals(bundled)) log("starting " + jar.getFileName());
        o = start(jar);
        if (o.ready) {
          if (!jar.equals(bundled)) Files.deleteIfExists(startsOf(jar));
          restart = o.exit == RESTART_CODE;
          break;
        }
        if (locked) break;
        if (!jar.equals(bundled)) {
          int n = countFailedStart(jar);
          marked.add(startsOf(jar));
          if (n >= 2) {
            log(jar.getFileName() + " failed " + n + " starts in a row, so it is marked: " + o.why
                + (last ? "" : "; starting the previous jar"));
            Files.writeString(failedOf(jar), o.why + "\n");
            marked.add(failedOf(jar));
          } else {
            log(jar.getFileName() + " failed: " + o.why + "; it gets one more start" + (last ? "" : ", after the previous jar"));
          }
        } else {
          log("the bundled jar failed: " + o.why);
        }
        if (last) {
          for (Path m : marked) Files.deleteIfExists(m);
          if (!marked.isEmpty()) log("every jar failed, so the markers are removed");
        }
      }
      if (restart) { log("restarting to apply an update"); continue; }
      return o.exit;
    }
  }

  public static void main(String[] argv) throws Exception {
    List<String> args = List.of(argv);
    Path dataDir = null;
    for (int i = 0; i + 1 < args.size(); i++) if (args.get(i).equals("--data-dir")) dataDir = Paths.get(args.get(i + 1));
    if (dataDir == null) dataDir = defaultDataDir();
    String appdir = System.getProperty("freememo.boot.appdir");
    if (appdir == null) { System.err.println("freememo.boot.appdir is not set"); System.exit(2); }
    String k = System.getProperty("freememo.update-public-key");
    PublicKey key = null;
    String bundled = System.getProperty("freememo.boot.bundled-version", "0");
    try { if (k != null && !k.isBlank()) key = parseKey(k); } catch (Exception e) {
      System.out.println("[boot] the update public key is not valid, so App Updates are off: " + e.getMessage());
    }
    FreememoBoot boot = new FreememoBoot(Paths.get(appdir), bundled, key,
        System.getProperty("freememo.boot.main-class", "freememo.local_install"),
        Long.parseLong(System.getProperty("freememo.boot.ready-timeout-s", "240")),
        System.getProperty("freememo.boot.java", Paths.get(System.getProperty("java.home"), "bin", "java").toString()),
        dataDir, args, withLogFile(dataDir, System.out));
    boot.log("FreeMemo " + bundled + " starting (" + System.getProperty("os.name") + ", Java " + System.getProperty("java.version") + ")");
    System.exit(boot.run());
  }
}
