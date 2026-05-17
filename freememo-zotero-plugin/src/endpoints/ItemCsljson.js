// GET /freememo/items/:key/csljson
// Pre:   Origin in allowlist; :key resolves to a user-library item.
// Post:  200 {csl: <map|null>}. null when the item has no CSL representation
//        (e.g. raw attachments).
// Blame: 404 {csl:null} when key resolves to nothing — treated like "no CSL"
//        per the existing freememo.zotero/get-item-csljson contract
//        (not-found is allowed).

FreeMemoEndpoints = FreeMemoEndpoints || {};

FreeMemoEndpoints.ItemCsljson = class {
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
    if (!item) {
      return FreeMemoPlugin.jsonResponse(200, origin, { csl: null });
    }
    try {
      // Zotero.Utilities.Item.itemToCSLJSON is the canonical converter; falls
      // through to {csl:null} if Zotero raises (e.g. attachment-only items).
      const csl = Zotero.Utilities.Item.itemToCSLJSON(item);
      return FreeMemoPlugin.jsonResponse(200, origin, { csl: csl || null });
    } catch (e) {
      Zotero.debug("FreeMemo csljson: conversion failed " + e);
      return FreeMemoPlugin.jsonResponse(200, origin, { csl: null });
    }
  }
};
