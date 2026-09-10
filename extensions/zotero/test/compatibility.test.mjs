import assert from "node:assert/strict";
import { readFile } from "node:fs/promises";
import test from "node:test";
import vm from "node:vm";

const pluginRoot = new URL("../", import.meta.url);

test("the manifest permits Zotero 10", async () => {
  const manifest = JSON.parse(
    await readFile(new URL("manifest.json", pluginRoot), "utf8")
  );

  assert.equal(
    manifest.applications.zotero.strict_max_version,
    "10.0.*",
    "Zotero 10 rejects the plugin before bootstrap.js runs"
  );
});

test("browser-facing endpoints opt out of the Zotero 10 request guard", async () => {
  const endpointNames = [
    "Probe.js",
    "ItemsTop.js",
    "ItemCsljson.js",
    "ItemPdfChildren.js",
    "ItemFile.js"
  ];

  for (const endpointName of endpointNames) {
    const source = await readFile(
      new URL(`src/endpoints/${endpointName}`, pluginRoot),
      "utf8"
    );
    assert.match(
      source,
      /this\.allowRequestsFromUnsafeWebContent\s*=\s*true/,
      `${endpointName} must allow the browser requests that its origin check validates`
    );
  }
});

test("the item list requests only live top-level items", async () => {
  const calls = [];
  const context = {
    FreeMemoEndpoints: {},
    FreeMemoPlugin: {
      originOf: () => "https://freememo.net",
      originAllowed: () => true,
      searchParamsOf: request => request.searchParams,
      jsonResponse: (status, _origin, body) => [status, {}, JSON.stringify(body)]
    },
    URLSearchParams,
    Zotero: {
      Libraries: { userLibraryID: 1 },
      Items: {
        getAll: async (...args) => {
          calls.push(args);
          return [];
        }
      }
    }
  };
  const source = await readFile(
    new URL("src/endpoints/ItemsTop.js", pluginRoot),
    "utf8"
  );
  vm.runInNewContext(source, context);

  await new context.FreeMemoEndpoints.ItemsTop().init({
    searchParams: new URLSearchParams("limit=20&start=0")
  });

  assert.deepEqual(
    calls,
    [[1, true, false, false]],
    "Zotero.Items.getAll expects libraryID, onlyTopLevel, includeDeleted, asIDs"
  );
});

test("the plugin update manifest publishes the current package", async () => {
  const manifest = JSON.parse(
    await readFile(new URL("manifest.json", pluginRoot), "utf8")
  );
  const updates = JSON.parse(
    await readFile(new URL("updates.json", pluginRoot), "utf8")
  );
  const plugin = updates.addons[manifest.applications.zotero.id];
  const release = plugin.updates.find(update => update.version === manifest.version);

  assert.equal(
    manifest.applications.zotero.update_url,
    "https://raw.githubusercontent.com/adham-omran/freememo/main/extensions/zotero/updates.json"
  );
  assert.ok(release, `updates.json must publish version ${manifest.version}`);
  assert.equal(
    release.applications.zotero.strict_max_version,
    manifest.applications.zotero.strict_max_version
  );
});
