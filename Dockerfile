FROM clojure:temurin-17-tools-deps-1.12.0.1501 AS build
WORKDIR /app

# The build's heap, CHOSEN rather than inherited. Nothing set one before, so the
# JVM took its default MaxRAMPercentage=25 — a quarter of whatever the host has.
# On the 7.9 GB deploy box that is 1982 MB (measured with PrintFlagsFinal), and
# the `:prod` ClojureScript compile has grown past it: three deploys died with
# `OutOfMemoryError: Java heap space` inside `uberjar` at ~176 s, while the same
# commits built fine on a 16 GB machine whose quarter is 3996 MB. A build that
# fails or passes according to the builder's RAM.
#
# 3g sits above the measured need — 2560 MB produces a complete 64.6 MB jar,
# 1982 MB does not — and inside the deploy box's headroom: 7.9 GB total, less
# the live color's 3 GiB mem_limit and ~0.3 GB of postgres/ontop/caddy.
# RAISING THIS WITHOUT RAISING THAT HEADROOM risks the kernel OOM killer
# choosing the LIVE container, which is an outage rather than a failed build.
#
# Absolute, not MaxRAMPercentage. docker-compose.prod.yml uses a percentage for
# the RUNTIME and is right to: that container has a `mem_limit` for the
# percentage to be a percentage OF. This stage has no limit, so a percentage is
# a share of the host — the accident being fixed. What the compile needs is a
# property of the codebase, not of the machine that happens to build it.
ENV JAVA_TOOL_OPTIONS="-Xmx3g"

COPY deps.edn deps.edn
ARG VERSION
ENV VERSION=$VERSION
ARG GIT_COMMIT
ENV GIT_COMMIT=$GIT_COMMIT
RUN clojure -A:prod -M -e ::ok       # preload – rebuilds if deps or commit version changes
RUN clojure -A:build:prod -M -e ::ok # preload

# Third-party client assets (PDF.js, Quill, Konva, KaTeX, …) are NOT in git.
# They are downloaded here and every byte is checked against the committed
# vendor-lock.sha256, so a compromised or drifted upstream fails the build
# instead of shipping. JDK-only — no curl, node or python in this stage.
#
# This runs BEFORE `COPY src` so editing a namespace does not re-download 3.4 MB;
# the layer is keyed on the lockfile and the fetcher alone.
COPY vendor-lock.sha256 vendor-lock.sha256
COPY src-build src-build
COPY resources/public/freememo/open-sans.css resources/public/freememo/open-sans.css
RUN clojure -X:build vendor/fetch!

COPY shadow-cljs.edn shadow-cljs.edn
COPY src src
COPY src-prod src-prod
COPY resources resources

# `COPY resources` above merges into the directory the fetch already populated.
# If that assumption ever breaks, the vendor tree would vanish and we would ship
# a jar whose every asset 404s — silently. This fails the build instead.
RUN clojure -X:build vendor/verify! && clojure -X:build vendor/check-refs!

RUN clojure -X:prod:build uberjar :version "\"$VERSION\"" :git-commit "\"$GIT_COMMIT\"" :build/jar-name "app.jar"

FROM eclipse-temurin:17-jre AS app
WORKDIR /app
# ImageMagick + libheif provide HEIC/HEIF decode for Live Document photo
# uploads — the JVM's ImageIO has no HEIC codec, so /api/heic-preview shells
# out to `magick`. (Base switched from amazoncorretto:17 / Amazon Linux 2,
# whose ImageMagick package lacks a reliable HEIF delegate.)
# curl backs the compose healthcheck. graphviz provides `sfdp`, which lays out
# the knowledge-graph view server-side (freememo.kg-graph). On Ubuntu the
# `graphviz` package ships the sfdp BINARY but not its layout engine — that
# lives in libgvplugin-neato-layout8 (neato/fdp/sfdp family). Without it sfdp
# exits with "no layout engine support for sfdp", so it MUST be installed
# explicitly (it is not a graphviz dependency or recommend).
# ffmpeg (which also provides ffprobe) backs the video pipeline: probe duration,
# extract a 32 kbps mono MP3, and split it into Whisper-sized chunks
# (freememo.ffmpeg / plans/incremental-video.md §4.4). Without it a video still
# uploads and plays; it just gets no duration, waveform, or transcript.
RUN apt-get update \
 && apt-get install -y --no-install-recommends imagemagick libheif1 curl graphviz libgvplugin-neato-layout8 ffmpeg \
 && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/target/app.jar app.jar

EXPOSE 8080
CMD java -cp app.jar clojure.main -m prod