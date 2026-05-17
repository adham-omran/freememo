// GET /freememo/items/top?limit=&start=
// Pre:   Origin in allowlist; limit and start are non-negative integers (defaults: 200, 0).
// Post:  200 {items: [{key,title,creators_summary,year,item_type}], next_start: <int|null>}.
//        Items are top-level user-library entries ordered by dateAdded desc.
//        next_start is null once the page is the last page.
// Blame: 403 on bad origin (caller); 500 on Zotero API failure (impl/Zotero bug).

FreeMemoEndpoints = FreeMemoEndpoints || {};

function creatorsSummary(creators) {
  if (!creators || !creators.length) return null;
  const first = creators[0];
  const name = first.lastName || first.name;
  if (!name) return null;
  return creators.length > 1 ? (name + " et al.") : name;
}

function yearOf(item) {
  let d;
  try { d = item.getField("date"); } catch (_) { d = null; }
  if (!d) return null;
  const m = String(d).match(/\d{4}/);
  return m ? m[0] : null;
}

function shapeItem(item) {
  let title;
  try { title = item.getField("title"); } catch (_) { title = null; }
  let creators;
  try { creators = item.getCreators(); } catch (_) { creators = []; }
  return {
    key: item.key,
    title: title || "(Untitled)",
    creators_summary: creatorsSummary(creators),
    year: yearOf(item),
    item_type: Zotero.ItemTypes.getName(item.itemTypeID)
  };
}

FreeMemoEndpoints.ItemsTop = class {
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

    const params = FreeMemoPlugin.searchParamsOf(request);
    const limit = Math.max(1, Math.min(500, parseInt(params.get("limit"), 10) || 200));
    const start = Math.max(0, parseInt(params.get("start"), 10) || 0);

    const libraryID = Zotero.Libraries.userLibraryID;
    // getAll(libraryID, asIDs, onlyTopLevel, includeDeleted)
    const all = await Zotero.Items.getAll(libraryID, false, true, false);
    // Sort by dateAdded desc (ISO 8601 strings are lexicographically comparable).
    all.sort(function (a, b) {
      const da = a.dateAdded || "";
      const db = b.dateAdded || "";
      if (da === db) return 0;
      return da < db ? 1 : -1;
    });
    const page = all.slice(start, start + limit);
    const items = page.map(shapeItem);
    const nextStart = (start + limit < all.length) ? (start + limit) : null;
    return FreeMemoPlugin.jsonResponse(200, origin, { items: items, next_start: nextStart });
  }
};
