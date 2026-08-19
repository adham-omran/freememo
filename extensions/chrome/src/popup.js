// The popup starts a capture and closes. It never uploads.
//
// Chrome destroys a popup when it loses focus, and a capture can take tens of
// seconds. Anything running here would die with the window, so the only work
// this file does is name a mode and hand it to the service worker.

/**
 * Ask the worker for one capture, then close.
 *
 * Pre : `mode` is "dom", "selection" or "screenshot".
 * Post: the worker has ACKNOWLEDGED the request, and the window is closed.
 *       The outcome arrives later as a toolbar badge.
 * Inv : the close waits on the acknowledgement. `sendMessage` is asynchronous
 *       and closing the popup destroys the context it is delivered in, so a
 *       synchronous `window.close()` on the next line can abort the message
 *       before the worker ever hears it — a click that does nothing at all,
 *       with no error anywhere to say so.
 */
async function start(mode) {
  try {
    await chrome.runtime.sendMessage({ type: "capture", mode });
  } catch (error) {
    // The worker is gone or failed to register. Say so where a click cannot.
    console.warn("FreeMemo: the extension worker did not answer:", error);
  }
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
