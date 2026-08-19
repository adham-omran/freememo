// Where this build sends captures, and the settings the popup and the worker
// both read.
//
// The origin is hardcoded on purpose. A self-hosted instance needs a manifest
// change and a store re-review, which is a deliberate decision recorded in
// plans/chrome-capture-extension.md, not an omission.

export const ORIGIN = "https://freememo.net";

export const ENDPOINTS = {
  paste: `${ORIGIN}/api/upload-paste`,
  file: `${ORIGIN}/api/upload-file`,
  staged: `${ORIGIN}/api/upload-staged`,
};

// Pre : none. Post: {onDuplicate} with a value this build understands.
// "existing" returns the topic already stored. "version" stores a sibling.
export async function readSettings() {
  const { onDuplicate } = await chrome.storage.sync.get({ onDuplicate: "existing" });
  return { onDuplicate: onDuplicate === "version" ? "version" : "existing" };
}

export async function writeSettings(next) {
  await chrome.storage.sync.set(next);
}
