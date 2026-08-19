// Runs in the page, injected on demand by the service worker.
//
// It reads and it measures. It never uploads and it never fetches a
// subresource: Chrome treats a content script's cross-origin request as the
// page's own, so an image on another host would need that host to send
// Access-Control-Allow-Origin. The service worker has host permissions and is
// not bound that way, so every fetch belongs there.
//
// Returns plain JSON. chrome.runtime messages are JSON-serialized with a 64 MiB
// ceiling, which one measured page approached only with 1158 images inlined.

/**
 * Serialize the whole rendered document.
 * Pre : called in a page context. Post: an HTML string, never null.
 */
function serializeDocument() {
  return document.documentElement.outerHTML;
}

/**
 * Serialize the current selection, or null when nothing is selected.
 * Pre : called in a page context.
 * Post: an HTML string wrapping the selected range, or null.
 */
function serializeSelection() {
  const selection = window.getSelection();
  if (!selection || selection.isCollapsed || selection.rangeCount === 0) return null;
  const holder = document.createElement("div");
  for (let i = 0; i < selection.rangeCount; i += 1) {
    holder.appendChild(selection.getRangeAt(i).cloneContents());
  }
  const html = holder.innerHTML.trim();
  return html.length > 0 ? html : null;
}

/**
 * Every image the page rendered, as {index, url}, in document order.
 *
 * Pre : called in a page context.
 * Post: an array whose `index` addresses `document.images[index]` and whose
 *       `url` is what the worker should fetch.
 * Inv : `url` is `currentSrc` when the browser resolved a `srcset`, so the
 *       worker fetches the candidate actually displayed rather than the
 *       fallback in the `src` attribute.
 *
 * The index, not the URL, is the handle. `serializeWithImages` rewrites the
 * element it names, so a URL that also appears in an `href` is untouched and a
 * `srcset` image whose `currentSrc` never appears in the markup still resolves.
 */
function imageRefs() {
  const refs = [];
  [...document.images].forEach((img, index) => {
    const url = img.currentSrc || img.src;
    if (url && /^https?:/i.test(url)) refs.push({ index, url });
  });
  return refs;
}

/**
 * Serialize the document with the named images replaced by data: URIs.
 *
 * Pre : `substitutions` maps an index from `imageRefs` to a data: URI. The DOM
 *       has not changed since that call.
 * Post: an HTML string. Only `src` attributes on the named images differ from
 *       `serializeDocument`'s output.
 * Inv : the live page is never mutated — the rewrite lands on a clone. An
 *       index that no longer resolves is skipped rather than throwing.
 */
function serializeWithImages(substitutions) {
  const clone = document.documentElement.cloneNode(true);
  const images = clone.querySelectorAll("img");
  for (const [index, dataUri] of Object.entries(substitutions)) {
    const img = images[Number(index)];
    if (!img) continue;
    img.setAttribute("src", dataUri);
    img.removeAttribute("srcset");
    img.removeAttribute("loading");
  }
  return clone.outerHTML;
}

/**
 * Read the page's own bibliographic metadata, in biblio-web's precedence:
 * Highwire citation_* first, then JSON-LD, then Dublin Core, then OpenGraph.
 * Pre : called in a page context.
 * Post: a CSL-JSON-shaped object. Absent fields are omitted, never null.
 *
 * The server harvests the same tags from the HTML it receives. Doing it here
 * as well costs one pass and covers the case where a tag lives in <head>,
 * which the main-region step discards before the server's harvester runs.
 */
function harvestCsl(pageUrl) {
  const meta = (selector) => {
    const el = document.querySelector(selector);
    const v = el && (el.getAttribute("content") || "").trim();
    return v || null;
  };
  const metaAll = (selector) =>
    [...document.querySelectorAll(selector)]
      .map((el) => (el.getAttribute("content") || "").trim())
      .filter(Boolean);

  let jsonLd = {};
  for (const node of document.querySelectorAll('script[type="application/ld+json"]')) {
    try {
      const parsed = JSON.parse(node.textContent);
      const first = Array.isArray(parsed) ? parsed[0] : parsed;
      if (first && typeof first === "object") {
        jsonLd = first;
        break;
      }
    } catch {
      // A malformed blob costs its own fields, never the capture.
    }
  }

  const title =
    meta('meta[name="citation_title"]') ||
    (typeof jsonLd.headline === "string" ? jsonLd.headline : null) ||
    meta('meta[name="DC.title"]') ||
    meta('meta[property="og:title"]') ||
    (document.title || "").trim() ||
    null;

  const authors = metaAll('meta[name="citation_author"]');
  const dcAuthors = authors.length ? [] : metaAll('meta[name="DC.creator"]');
  const allAuthors = authors.length ? authors : dcAuthors;

  const issued =
    meta('meta[name="citation_publication_date"]') ||
    meta('meta[name="citation_date"]') ||
    meta('meta[name="DC.date"]') ||
    meta('meta[property="article:published_time"]') ||
    (typeof jsonLd.datePublished === "string" ? jsonLd.datePublished : null);

  const container =
    meta('meta[name="citation_journal_title"]') ||
    meta('meta[property="og:site_name"]') ||
    location.hostname;

  const doi = meta('meta[name="citation_doi"]');

  const csl = { type: "webpage", URL: pageUrl };
  if (title) csl.title = title;
  if (allAuthors.length) csl.author = allAuthors.map((literal) => ({ literal }));
  if (container) csl["container-title"] = container;
  if (doi) csl.DOI = doi.replace(/^doi:/i, "");

  const dateParts = parseDateParts(issued);
  if (dateParts) csl.issued = { "date-parts": [dateParts] };

  return csl;
}

/** "2023-01-19T09:00:00Z" → [2023, 1, 19]. Null when unparseable. */
function parseDateParts(value) {
  if (!value) return null;
  const m = /^(\d{4})(?:[-/](\d{1,2}))?(?:[-/](\d{1,2}))?/.exec(String(value).trim());
  if (!m) return null;
  const year = Number(m[1]);
  if (year < 1900 || year > 2100) return null;
  return [year, m[2] && Number(m[2]), m[3] && Number(m[3])].filter(Boolean);
}

/** Everything the worker needs to size and drive a scroll-and-stitch capture. */
function pageMetrics() {
  const de = document.documentElement;
  const body = document.body;
  return {
    fullWidth: Math.max(de.clientWidth, de.scrollWidth, body ? body.scrollWidth : 0),
    fullHeight: Math.max(de.clientHeight, de.scrollHeight, body ? body.scrollHeight : 0),
    viewportWidth: window.innerWidth,
    viewportHeight: window.innerHeight,
    devicePixelRatio: window.devicePixelRatio || 1,
    originalScrollY: window.scrollY,
  };
}

/**
 * Hide position:fixed and sticky elements for the duration of a stitch.
 * Pre : called in a page context. Post: returns how many were hidden.
 *
 * A pinned header is captured in every viewport and stamps itself down the
 * finished image. GoFullPage pads its scroll by 200px to soften that. Hiding
 * the elements removes the cause instead, and the worker restores them.
 */
function hidePinnedElements() {
  const hidden = [];
  // Only an element that establishes its own layout box can be pinned, and a
  // pinned one is always positioned. Reading `position` off every node forces a
  // full style resolution over the tree; this narrows the candidates to
  // elements a stylesheet plausibly positions before paying for it.
  const candidates = document.body
    ? document.body.querySelectorAll(
        "header, footer, nav, aside, div, section, [class*='sticky'], [class*='fixed'], [style*='position']",
      )
    : [];
  for (const el of candidates) {
    const pos = getComputedStyle(el).position;
    if (pos === "fixed" || pos === "sticky") {
      hidden.push([el, el.style.visibility]);
      el.style.visibility = "hidden";
    }
  }
  window.__freememoHidden = hidden;
  return hidden.length;
}

function restorePinnedElements() {
  for (const [el, previous] of window.__freememoHidden || []) {
    el.style.visibility = previous;
  }
  delete window.__freememoHidden;
  return true;
}

// The worker calls these by name through chrome.scripting.executeScript, so
// each must be reachable on the page's own global.
Object.assign(window, {
  __freememoCapture: {
    serializeDocument,
    serializeSelection,
    serializeWithImages,
    imageRefs,
    harvestCsl,
    pageMetrics,
    hidePinnedElements,
    restorePinnedElements,
  },
});
