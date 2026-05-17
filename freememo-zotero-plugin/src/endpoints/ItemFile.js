// GET /freememo/items/:key/file
// Pre:   Origin in allowlist; :key is a PDF attachment in the user library.
// Post:  200 {filename, content_type, size, base64}.
//        size is the raw byte length; base64 is the file contents base64-encoded.
// Blame: 413 {error:"too-large",size,cap} when size exceeds PER_FILE_MAX_BYTES.
//        404 {error:"not-attachment"|"no-file"} when :key is not a PDF attachment
//        or its file is missing from disk.
//        403 on bad origin (caller).
//
// Binary-over-text rationale: Zotero's response writer goes through
// nsIConverterOutputStream.writeString (UTF-8) which mangles raw bytes.
// base64 wrapping is the cheapest way to round-trip a Uint8Array via the
// existing endpoint contract. ~33% bandwidth overhead is acceptable
// because each PDF is fetched exactly once per import.

FreeMemoEndpoints = FreeMemoEndpoints || {};

// Match server-side freememo.quota/per-file-max-bytes default (100 MB).
// Server re-enforces this cap on upload, so the plugin value is a defence-
// in-depth ceiling, not the trust boundary.
const PER_FILE_MAX_BYTES = 104857600;

function binaryStringToBase64(s) {
  // btoa() on 100 MB strings is workable on modern engines, but chunking
  // in multiples of 3 bytes keeps engines happy on smaller stacks and
  // keeps memory predictable. Each chunk produces a self-contained base64
  // segment with no padding except the final one.
  const chunkSize = 65532; // 65535 - (65535 % 3) === 65535... let's use a multiple of 3 directly
  if (s.length <= chunkSize) return btoa(s);
  let out = "";
  let i = 0;
  while (i + chunkSize < s.length) {
    out += btoa(s.substr(i, chunkSize));
    i += chunkSize;
  }
  out += btoa(s.substr(i));
  return out;
}

FreeMemoEndpoints.ItemFile = class {
  constructor() {
    this.supportedMethods = ["GET"];
    this.supportedDataTypes = ["application/json"];
    this.permitBookmarklet = false;
    this.allowRequestsFromUnsafeWebContent = true;
  }

  async init(request) {
    const origin = FreeMemoPlugin.originOf(request);
    if (!FreeMemoPlugin.originAllowed(origin)) {
      return FreeMemoPlugin.jsonResponse(403, origin, { error: "origin-not-allowed" });
    }

    const key = FreeMemoPlugin.pathParamOf(request, "key");
    if (!key) {
      return FreeMemoPlugin.jsonResponse(400, origin, { error: "missing-key" });
    }

    const libraryID = Zotero.Libraries.userLibraryID;
    const item = await Zotero.Items.getByLibraryAndKeyAsync(libraryID, key);
    if (!item || !item.isAttachment()) {
      return FreeMemoPlugin.jsonResponse(404, origin, { error: "not-attachment" });
    }
    if (item.attachmentContentType !== "application/pdf") {
      return FreeMemoPlugin.jsonResponse(404, origin, { error: "not-pdf" });
    }

    let path;
    try {
      path = await item.getFilePathAsync();
    } catch (e) {
      Zotero.debug("FreeMemo file: getFilePathAsync failed " + e);
    }
    if (!path) {
      return FreeMemoPlugin.jsonResponse(404, origin, { error: "no-file" });
    }

    // getBinaryContentsAsync returns a binary string (each char is a byte 0-255).
    let binary;
    try {
      binary = await Zotero.File.getBinaryContentsAsync(path);
    } catch (e) {
      Zotero.debug("FreeMemo file: read failed " + e);
      return FreeMemoPlugin.jsonResponse(500, origin, { error: "read-failed" });
    }

    const size = binary.length;
    if (size > PER_FILE_MAX_BYTES) {
      return FreeMemoPlugin.jsonResponse(413, origin, {
        error: "too-large", size: size, cap: PER_FILE_MAX_BYTES
      });
    }

    const base64 = binaryStringToBase64(binary);
    return FreeMemoPlugin.jsonResponse(200, origin, {
      filename: item.attachmentFilename || "attachment.pdf",
      content_type: "application/pdf",
      size: size,
      base64: base64
    });
  }
};
