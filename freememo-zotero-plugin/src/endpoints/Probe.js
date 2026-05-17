// GET /freememo/probe
// Pre:   request.headers.Origin is in the configured allowlist.
// Post:  200 {ok, plugin_version, zotero_version, library_id}.
// Blame: 403 {error: "origin-not-allowed"} when pre is violated — caller bug.
//
// The body deliberately includes diagnostic fields used by the FreeMemo Settings
// status banner (plugin_version for "is this the build I expect?",
// zotero_version for "what Zotero am I talking to?", library_id for "which
// library will endpoints in P2+ read from?").

FreeMemoEndpoints = FreeMemoEndpoints || {};

FreeMemoEndpoints.Probe = class {
  constructor() {
    this.supportedMethods = ["GET"];
    this.supportedDataTypes = ["application/json"];
    this.permitBookmarklet = false;
    // Bypass Zotero's "Preventing request from browser" CSRF guard so
    // legitimate fetch() calls from freememo.net reach our handler.
    // The handler's own Origin allowlist + per-origin CORS header is
    // the actual security boundary.
    this.allowRequestsFromUnsafeWebContent = true;
  }

  async init(request) {
    // First-run diagnostic: surface the request shape to Zotero's debug log
    // so we can verify origin extraction works against the actual Zotero 9
    // Server.RequestHandler shape (currently inferred from server.js source).
    try {
      Zotero.debug("FreeMemo probe: request keys = " +
                   JSON.stringify(Object.keys(request || {})));
      if (request && request.headers) {
        Zotero.debug("FreeMemo probe: header keys = " +
                     JSON.stringify(Object.keys(request.headers)));
      }
    } catch (e) {
      Zotero.debug("FreeMemo probe: log failed " + e);
    }

    const origin = FreeMemoPlugin.originOf(request);
    if (!FreeMemoPlugin.originAllowed(origin)) {
      return FreeMemoPlugin.jsonResponse(403, origin, {
        error: "origin-not-allowed",
        got: origin || null
      });
    }
    return FreeMemoPlugin.jsonResponse(200, origin, {
      ok: true,
      plugin_version: FreeMemoPlugin.version,
      zotero_version: Zotero.version,
      library_id: Zotero.Libraries.userLibraryID
    });
  }
};
