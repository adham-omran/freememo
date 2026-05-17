// FreeMemo plugin server module.
// Pre:   Zotero global available; FreeMemoEndpoints.* classes loaded before start().
// Post:  every endpoint listed in start() is registered on Zotero.Server.Endpoints
//        and stop() removes exactly those entries.
// Inv:   never overwrites a path that is not in registeredPaths (won't clobber other plugins).

FreeMemoPlugin = (function () {
  const ALLOWED_ORIGINS = ["https://freememo.net"];
  // Dev: any http://localhost or http://127.0.0.1 (with optional :port).
  const LOCAL_ORIGIN_RE = /^http:\/\/(localhost|127\.0\.0\.1)(:\d+)?$/;

  let pluginVersion = "unknown";
  let registeredPaths = [];

  function originOf(request) {
    if (!request || !request.headers) return null;
    // Zotero wraps headers in a case-insensitive proxy, but be defensive
    // against shape drift across versions.
    return request.headers.Origin || request.headers.origin || null;
  }

  function searchParamsOf(request) {
    // Zotero passes a URLSearchParams in request.searchParams (verified
    // against server.js). Fall back to parsing request.query / request.url
    // if the shape ever drifts.
    if (request && request.searchParams) return request.searchParams;
    if (request && typeof request.query === "string") return new URLSearchParams(request.query);
    if (request && typeof request.url === "string") {
      try { return new URL(request.url, "http://localhost").searchParams; } catch (_) {}
    }
    return new URLSearchParams("");
  }

  function pathParamOf(request, name) {
    if (request && request.pathParams && request.pathParams[name] != null) {
      return String(request.pathParams[name]);
    }
    return null;
  }

  function originAllowed(origin) {
    if (!origin) return false;
    if (ALLOWED_ORIGINS.indexOf(origin) >= 0) return true;
    return LOCAL_ORIGIN_RE.test(origin);
  }

  function corsHeaders(origin) {
    return {
      "Access-Control-Allow-Origin": origin || "https://freememo.net",
      "Access-Control-Allow-Methods": "GET, POST, OPTIONS",
      "Access-Control-Allow-Headers": "Content-Type",
      // Chrome's Private Network Access blocks fetches from public HTTPS
      // origins to private hosts (127.0.0.1) unless the response carries
      // this header. Always-on; harmless on browsers that don't enforce PNA.
      "Access-Control-Allow-Private-Network": "true",
      "Vary": "Origin"
    };
  }

  function jsonResponse(status, origin, body) {
    const headers = Object.assign({ "Content-Type": "application/json" }, corsHeaders(origin));
    return [status, headers, JSON.stringify(body)];
  }

  function register(path, handlerClass) {
    if (!handlerClass) {
      Zotero.debug("FreeMemo: refusing to register " + path + " — handler missing");
      return;
    }
    Zotero.Server.Endpoints[path] = handlerClass;
    registeredPaths.push(path);
    Zotero.debug("FreeMemo: registered " + path);
  }

  function start(opts) {
    pluginVersion = (opts && opts.pluginVersion) || "unknown";
    const E = FreeMemoEndpoints || {};
    register("/freememo/probe",                       E.Probe);
    register("/freememo/items/top",                   E.ItemsTop);
    register("/freememo/items/:key/csljson",          E.ItemCsljson);
    register("/freememo/items/:key/children/pdfs",    E.ItemPdfChildren);
    register("/freememo/items/:key/file",             E.ItemFile);
  }

  function stop() {
    for (const path of registeredPaths) {
      if (Zotero.Server.Endpoints[path]) {
        delete Zotero.Server.Endpoints[path];
        Zotero.debug("FreeMemo: unregistered " + path);
      }
    }
    registeredPaths = [];
  }

  return {
    get version() { return pluginVersion; },
    originOf: originOf,
    originAllowed: originAllowed,
    searchParamsOf: searchParamsOf,
    pathParamOf: pathParamOf,
    corsHeaders: corsHeaders,
    jsonResponse: jsonResponse,
    start: start,
    stop: stop
  };
})();
