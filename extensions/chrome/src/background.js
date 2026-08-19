// The service worker. It captures and it uploads.
//
// Both live here rather than in the popup for one measured reason: Chrome
// destroys a popup when it loses focus, and that kills an in-flight upload.
// Karakeep's extension uploads from its popup and its own docs warn a capture
// "might take 30+ seconds", with no recovery. The popup here sends one message
// and closes.
//
// The worker is also the only context that may fetch a third-party subresource.
// Chrome states that a content script's cross-origin request is treated as the
// page's own "even if the extension has host permissions", so image inlining
// and PDF passthrough both belong here.

import { ENDPOINTS, readSettings } from "./config.js";
import { buildPdf, tileToJpeg } from "./pdf.js";

// chrome.tabs.captureVisibleTab is documented as MAX_CAPTURE_VISIBLE_TAB_CALLS_
// PER_SECOND = 2 and rejects past it. 550ms leaves a margin over the 500ms floor.
const CAPTURE_INTERVAL_MS = 550;
const SETTLE_MS = 150;
const MAX_INLINE_IMAGE_BYTES = 2 * 1024 * 1024;
const MAX_INLINE_TOTAL_BYTES = 40 * 1024 * 1024;

const BADGE = {
  working: { text: "…", color: "#6b7280" },
  created: { text: "✓", color: "#16a34a" },
  duplicate: { text: "=", color: "#2563eb" },
  failed: { text: "!", color: "#dc2626" },
};

/** Show one outcome on the toolbar. Pre: `state` is a BADGE key. */
async function badge(tabId, state) {
  const { text, color } = BADGE[state];
  await chrome.action.setBadgeText({ tabId, text });
  await chrome.action.setBadgeBackgroundColor({ tabId, color });
  if (state !== "working") {
    setTimeout(() => chrome.action.setBadgeText({ tabId, text: "" }), 6000);
  }
}

/**
 * Inject the content script into a tab, once per capture.
 *
 * Pre : the extension holds activeTab or host permission for `tabId`.
 * Post: `window.__freememoCapture` is defined in that tab's isolated world.
 * Throws when Chrome refuses the injection — a chrome:// page, the Web Store,
 * or the built-in PDF viewer — which is what the screenshot fallback reads.
 */
async function injectCaptureApi(tabId) {
  await chrome.scripting.executeScript({ target: { tabId }, files: ["src/content.js"] });
}

/**
 * Call one exported content-script function in the page.
 * Pre : `injectCaptureApi` has run for this tab in this capture.
 * Post: the function's return value, or null when it returned nothing.
 */
async function inPage(tabId, fnName, args = []) {
  const [result] = await chrome.scripting.executeScript({
    target: { tabId },
    func: (name, callArgs) => window.__freememoCapture[name](...callArgs),
    args: [fnName, args],
  });
  return result ? result.result : null;
}

const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// ---------------------------------------------------------------------------
// Image inlining
// ---------------------------------------------------------------------------

/**
 * Fetch the page's images and map each one to a data: URI.
 *
 * Pre : `refs` came from the content script's `imageRefs`, so every `index`
 *       still addresses an element in that page.
 * Post: {index: dataUri} for the images that fetched. An image that fails is
 *       absent, so it keeps its remote URL and costs the capture nothing.
 * Inv : the inlined total stays under MAX_INLINE_TOTAL_BYTES. Beyond it the
 *       rest stay remote — measured, only a 1158-image page approached it.
 *
 * The fetch belongs here and nowhere else. Chrome treats a content script's
 * cross-origin request as the page's own even when the extension holds host
 * permissions, so a third-party image would need that host to send
 * Access-Control-Allow-Origin.
 */
async function fetchImagesAsDataUris(refs) {
  const substitutions = {};
  let budget = 0;

  for (const { index, url } of refs) {
    if (budget >= MAX_INLINE_TOTAL_BYTES) break;
    try {
      const response = await fetch(url, { credentials: "include" });
      if (!response.ok) continue;
      const blob = await response.blob();
      if (blob.size > MAX_INLINE_IMAGE_BYTES) continue;
      if (!blob.type.startsWith("image/")) continue;

      const dataUri = await blobToDataUri(blob);
      budget += dataUri.length;
      substitutions[index] = dataUri;
    } catch {
      // The page rendered it; we could not. Leave the remote URL in place.
    }
  }
  return substitutions;
}

function blobToDataUri(blob) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => resolve(reader.result);
    reader.onerror = reject;
    reader.readAsDataURL(blob);
  });
}

// ---------------------------------------------------------------------------
// Screenshot
// ---------------------------------------------------------------------------

/**
 * Scroll the tab and stitch every viewport into one bitmap.
 *
 * Pre : `tabId` is the active tab and the extension holds activeTab for it.
 * Post: an ImageBitmap of the whole page, at device pixel ratio.
 * Inv : the page's scroll position is restored and pinned elements are shown
 *       again, on success and on failure alike.
 *
 * Pinned elements are hidden for the duration. A sticky header would otherwise
 * appear in every viewport and stamp itself down the finished image.
 */
async function stitchFullPage(tabId, metrics) {
  const { fullHeight, viewportHeight, devicePixelRatio: dpr, originalScrollY } = metrics;
  const shots = [];

  await inPage(tabId, "hidePinnedElements");
  try {
    for (let y = 0; y < fullHeight; y += viewportHeight) {
      await chrome.scripting.executeScript({
        target: { tabId },
        func: (top) => window.scrollTo(0, top),
        args: [y],
      });
      await sleep(SETTLE_MS);
      const dataUrl = await chrome.tabs.captureVisibleTab({ format: "png" });
      shots.push({ y, bitmap: await createImageBitmap(await (await fetch(dataUrl)).blob()) });
      await sleep(CAPTURE_INTERVAL_MS - SETTLE_MS);
    }
  } finally {
    await chrome.scripting.executeScript({
      target: { tabId },
      func: (top) => window.scrollTo(0, top),
      args: [originalScrollY],
    });
    await inPage(tabId, "restorePinnedElements");
  }

  const width = shots[0].bitmap.width;
  const canvas = new OffscreenCanvas(width, Math.round(fullHeight * dpr));
  const ctx = canvas.getContext("2d");
  ctx.fillStyle = "#ffffff";
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  for (const shot of shots) {
    const destY = Math.round(shot.y * dpr);
    // The last viewport overlaps the one above it whenever the page height is
    // not a whole number of viewports. Drawing its BOTTOM slice at the page
    // bottom keeps the seam from repeating content.
    const overflow = destY + shot.bitmap.height - canvas.height;
    if (overflow > 0) {
      ctx.drawImage(
        shot.bitmap,
        0, overflow, shot.bitmap.width, shot.bitmap.height - overflow,
        0, destY + overflow, shot.bitmap.width, shot.bitmap.height - overflow,
      );
    } else {
      ctx.drawImage(shot.bitmap, 0, destY);
    }
  }
  return canvas.transferToImageBitmap();
}

// ---------------------------------------------------------------------------
// Upload
// ---------------------------------------------------------------------------

/**
 * Post a captured document.
 * Pre : `html` is sanitized by nobody yet — the server owns that.
 * Post: {ok, flow} where flow is "captured" or "already-exists".
 */
async function uploadDocument({ html, url, csl, version }) {
  const body = new URLSearchParams({
    capture: "true",
    content: html,
    url: url || "",
    csljson: JSON.stringify(csl),
  });
  if (version) body.set("version", "true");

  const response = await fetch(ENDPOINTS.paste, {
    method: "POST",
    credentials: "include",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body,
  });
  const json = await response.json().catch(() => ({}));
  if (!response.ok || !json.success) {
    throw new Error(json.error || `Upload failed (HTTP ${response.status})`);
  }
  return { ok: true, flow: json.flow || "captured" };
}

/**
 * Post binary bytes, then commit the staged upload.
 * Pre : `bytes` is a Uint8Array holding a PDF. `filename` ends in .pdf.
 * Post: {ok, flow}. A server that recognizes the URL answers on the first call
 *       and there is nothing to commit.
 */
async function uploadPdf({ bytes, filename, csl }) {
  const form = new FormData();
  form.append("file", new Blob([bytes], { type: "application/pdf" }), filename);
  form.append("csljson", JSON.stringify(csl));

  const staged = await fetch(ENDPOINTS.file, {
    method: "POST",
    credentials: "include",
    body: form,
  });
  const stagedJson = await staged.json().catch(() => ({}));
  if (!staged.ok || !stagedJson.success) {
    throw new Error(stagedJson.error || `Upload failed (HTTP ${staged.status})`);
  }
  if (stagedJson.flow === "already-exists") return { ok: true, flow: "already-exists" };

  const committed = await fetch(ENDPOINTS.staged, {
    method: "POST",
    credentials: "include",
    headers: { "Content-Type": "application/x-www-form-urlencoded" },
    body: new URLSearchParams({ upload_id: stagedJson.upload_id }),
  });
  const committedJson = await committed.json().catch(() => ({}));
  if (!committed.ok || !committedJson.success) {
    throw new Error(committedJson.error || `Commit failed (HTTP ${committed.status})`);
  }
  return { ok: true, flow: "captured" };
}

// ---------------------------------------------------------------------------
// Modes
// ---------------------------------------------------------------------------

/**
 * True when the tab is showing Chrome's built-in PDF viewer.
 *
 * Pre : `tab` is a tabs.Tab. Post: a boolean.
 * Inv : asks the network what the resource is, and only guesses from the URL
 *       when the question cannot be answered. A URL suffix alone misses every
 *       PDF served from a path that does not end in .pdf, which is most of
 *       them behind a content-management system.
 */
async function showsPdfViewer(tab) {
  const url = tab.url || "";
  if (!/^https?:/i.test(url)) return false;
  try {
    const head = await fetch(url, { method: "HEAD", credentials: "include" });
    const type = head.headers.get("content-type") || "";
    if (type.toLowerCase().includes("application/pdf")) return true;
    // A server that answers HEAD without a type tells us nothing; fall through.
    if (type) return false;
  } catch {
    // HEAD refused or blocked. The suffix is all that is left.
  }
  return /\.pdf($|[?#])/i.test(url);
}

/** A filename the server's prettify-title can turn into a usable topic name. */
function pdfFilename(title, url) {
  const base = (title || new URL(url).hostname || "capture")
    .replace(/[\\/:*?"<>|]+/g, " ")
    .replace(/\s+/g, " ")
    .trim()
    .slice(0, 120);
  return `${base || "capture"}.pdf`;
}

/**
 * Read an open PDF as itself, rather than as a picture of a viewer.
 * Pre : the tab is showing a PDF. Post: {bytes, filename, csl}.
 * Inv : uploads nothing.
 */
async function collectPdfPassthrough(tab) {
  const response = await fetch(tab.url, { credentials: "include" });
  if (!response.ok) throw new Error(`Could not read the PDF (HTTP ${response.status})`);
  return {
    bytes: new Uint8Array(await response.arrayBuffer()),
    filename: pdfFilename(tab.title, tab.url),
    csl: { type: "webpage", URL: tab.url, title: tab.title || tab.url },
  };
}

/**
 * Build the PDF bytes for a full-page screenshot.
 * Pre : `tab` is active. Post: {bytes, filename, csl} ready to upload.
 * Inv : uploads nothing. Separating this from the upload is what lets the
 *       caller tell a capture failure from a server failure.
 */
async function collectScreenshot(tab) {
  let metrics = null;
  try {
    await injectCaptureApi(tab.id);
    metrics = await inPage(tab.id, "pageMetrics");
  } catch {
    // A restricted page takes no content script. activeTab still permits
    // captureVisibleTab there, so one viewport is what we can get.
  }
  const bitmap = metrics
    ? await stitchFullPage(tab.id, metrics)
    : await createImageBitmap(
        await (await fetch(await chrome.tabs.captureVisibleTab({ format: "png" }))).blob(),
      );
  return {
    bytes: buildPdf(await tileToJpeg(bitmap)),
    filename: pdfFilename(tab.title, tab.url),
    csl: { type: "webpage", URL: tab.url, title: tab.title || tab.url },
  };
}

/**
 * Read the page as HTML, with its images inlined.
 *
 * Pre : `mode` is "dom" or "selection".
 * Post: {html, csl} — or null for "dom" when the page yielded nothing, which
 *       is the caller's signal to fall back to a screenshot.
 * Throws for "selection" with nothing selected, and whenever Chrome refuses
 *       the injection. Both are capture failures, never upload failures.
 * Inv : uploads nothing.
 */
async function collectHtml(tab, mode) {
  await injectCaptureApi(tab.id);

  if (mode === "selection") {
    const html = await inPage(tab.id, "serializeSelection");
    if (!html) throw new Error("Select some text first");
    const csl = (await inPage(tab.id, "harvestCsl", [tab.url])) || { type: "webpage", URL: tab.url };
    return { html, csl };
  }

  const refs = (await inPage(tab.id, "imageRefs")) || [];
  const substitutions = refs.length ? await fetchImagesAsDataUris(refs) : {};
  const html = Object.keys(substitutions).length
    ? await inPage(tab.id, "serializeWithImages", [substitutions])
    : await inPage(tab.id, "serializeDocument");

  if (!html) return null;
  const csl = (await inPage(tab.id, "harvestCsl", [tab.url])) || { type: "webpage", URL: tab.url };
  return { html, csl };
}

// ---------------------------------------------------------------------------
// Entry point
// ---------------------------------------------------------------------------

/**
 * Run one capture end to end.
 *
 * Pre : `mode` is "dom", "selection" or "screenshot". The popup has closed.
 * Post: exactly one badge state is shown. Nothing is retried automatically —
 *       a retry after a lost response could duplicate an upload that landed,
 *       so the user re-clicks instead.
 *
 * The screenshot fallback is driven by failure, never by a guess: a content
 * script that cannot be injected, or that returns nothing, is what triggers it.
 */
async function capture(mode) {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  if (!tab) return;

  await badge(tab.id, "working");
  try {
    const { onDuplicate } = await readSettings();
    const version = onDuplicate === "version";

    // Capture first, upload second, and never let one stand in for the other.
    // A server error, an expired session or a dropped connection is not a
    // reason to decide the page was unreadable and silently store a
    // screenshot of it instead.
    let document = null;
    let pdf = null;

    if (mode === "screenshot") {
      pdf = await collectScreenshot(tab);
    } else if (await showsPdfViewer(tab)) {
      // A fourth mode with no button. Chrome renders a PDF in a viewer no
      // content script can reach, and a screenshot of that viewer is one
      // screenful of a document.
      pdf = await collectPdfPassthrough(tab);
    } else {
      document = await collectHtml(tab, mode).catch((error) => {
        if (mode === "selection") throw error;
        return null; // Injection refused — fall back to a picture.
      });
      if (!document) pdf = await collectScreenshot(tab);
    }

    const result = document
      ? await uploadDocument({ ...document, url: tab.url, version })
      : await uploadPdf(pdf);

    await badge(tab.id, result.flow === "already-exists" ? "duplicate" : "created");
  } catch (error) {
    console.warn("FreeMemo capture failed:", error);
    await badge(tab.id, "failed");
    await chrome.storage.session.set({ lastError: String(error.message || error) });
  }
}

/**
 * Receive a capture request from the popup.
 *
 * Pre : `message.mode` is "dom", "selection" or "screenshot".
 * Post: the sender is answered AT ONCE, so its popup can close, and the
 *       capture continues here.
 * Inv : the acknowledgement says "started", never "finished". Holding the
 *       message channel open until the capture completes would keep the
 *       worker alive, but it would also hold the popup open for the whole
 *       capture — the popup awaits this answer. Fast acknowledgement is what
 *       lets the popup close, which is the whole reason the work is here.
 *
 * Worker lifetime is not solved by this handler. Chrome terminates an idle
 * worker after 30 seconds, and since Chrome 110 a pending event holds it —
 * every `chrome.scripting` and `chrome.tabs` call in a capture is such an
 * event, so a stitch or an upload keeps resetting the timer. The gap that
 * remains is a single fetch that takes more than 30 seconds to answer at all.
 */
chrome.runtime.onMessage.addListener((message, _sender, sendResponse) => {
  if (message && message.type === "capture") {
    capture(message.mode);
    sendResponse({ started: true });
  }
  return false;
});
