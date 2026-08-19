// The popup starts a capture and closes. It never uploads.
//
// Chrome destroys a popup when it loses focus, and a capture can take tens of
// seconds. Anything running here would die with the window, so the only work
// this file does is name a mode and hand it to the service worker.

/**
 * Ask the worker for one capture, then close.
 * Pre : `mode` is "dom", "selection" or "screenshot".
 * Post: the window is closed. The outcome arrives as a toolbar badge.
 */
function start(mode) {
  chrome.runtime.sendMessage({ type: "capture", mode });
  window.close();
}

for (const mode of ["dom", "selection", "screenshot"]) {
  document.getElementById(mode).addEventListener("click", () => start(mode));
}

document.getElementById("options").addEventListener("click", (event) => {
  event.preventDefault();
  chrome.runtime.openOptionsPage();
  window.close();
});
