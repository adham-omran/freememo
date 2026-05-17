// GET /freememo/items/:key/children/pdfs
// Pre:   Origin in allowlist; :key resolves to a parent item.
// Post:  200 {pdfs: [{key, filename, link_mode}]}.
//        Filters strictly on itemType=attachment AND attachmentContentType=application/pdf.
// Blame: 200 {pdfs:[]} when key resolves to nothing or has no PDF children
//        (matches existing freememo.zotero/list-pdf-attachments contract).

FreeMemoEndpoints = FreeMemoEndpoints || {};

FreeMemoEndpoints.ItemPdfChildren = class {
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
    const parent = await Zotero.Items.getByLibraryAndKeyAsync(libraryID, key);
    if (!parent) {
      return FreeMemoPlugin.jsonResponse(200, origin, { pdfs: [] });
    }
    const attachmentIDs = parent.getAttachments();
    const attachments = await Zotero.Items.getAsync(attachmentIDs);
    const pdfs = [];
    for (const a of attachments) {
      if (a.attachmentContentType === "application/pdf") {
        pdfs.push({
          key: a.key,
          filename: a.attachmentFilename || "attachment.pdf",
          link_mode: Zotero.Attachments.linkModeToName(a.attachmentLinkMode)
        });
      }
    }
    return FreeMemoPlugin.jsonResponse(200, origin, { pdfs: pdfs });
  }
};
