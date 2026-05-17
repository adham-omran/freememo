var FreeMemoPlugin;
var FreeMemoEndpoints;

function log(msg) {
  Zotero.debug("FreeMemo: " + msg);
}

function install(data, reason) {}

async function startup({ id, version, rootURI }, reason) {
  log("Starting " + version);
  Services.scriptloader.loadSubScript(rootURI + "src/endpoints/Probe.js");
  Services.scriptloader.loadSubScript(rootURI + "src/endpoints/ItemsTop.js");
  Services.scriptloader.loadSubScript(rootURI + "src/endpoints/ItemCsljson.js");
  Services.scriptloader.loadSubScript(rootURI + "src/endpoints/ItemPdfChildren.js");
  Services.scriptloader.loadSubScript(rootURI + "src/endpoints/ItemFile.js");
  Services.scriptloader.loadSubScript(rootURI + "src/server.js");
  FreeMemoPlugin.start({ pluginVersion: version });
}

function shutdown(data, reason) {
  log("Shutting down");
  if (FreeMemoPlugin) {
    FreeMemoPlugin.stop();
    FreeMemoPlugin = undefined;
  }
  FreeMemoEndpoints = undefined;
}

function uninstall(data, reason) {}
