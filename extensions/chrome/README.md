# FreeMemo Web Capture

A Chrome extension that saves the page you are reading into FreeMemo.

The server cannot fetch a page that needs your session. It has no cookies, it
runs no JavaScript, and many sites refuse it outright. This extension captures
what the browser already rendered.

## Modes

The popup shows three buttons. A fourth mode has no button.

| Mode | What it sends | Becomes |
|---|---|---|
| Page | The rendered DOM, with images inlined | A `web` topic |
| Selection | Only the highlighted range | A `web` topic |
| Screenshot | A full-page image, tiled and wrapped in a PDF | A `pdf` topic |
| PDF passthrough | The PDF file itself | A `pdf` topic |

PDF passthrough runs by itself when Chrome is showing a PDF. No content script
can reach that viewer, and a screenshot of it captures one screenful of a
document, so the extension fetches the file instead.

A page that cannot be read falls back to a screenshot. The trigger is failure,
never a guess — an injection Chrome refuses, or a content script that returns
nothing.

## Install for development

1. Open `chrome://extensions`.
2. Turn on Developer mode.
3. Click **Load unpacked** and choose this directory.

Change `ORIGIN` in `src/config.js` to point at a local server.

## Build

```sh
./build.sh
```

Produces `dist/freememo-web-capture-<version>.zip` for the Chrome Web Store.

There is no bundler and no dependency. Every file is an ES module the browser
loads directly. The one thing that would have needed a bundler, a PDF library,
is `src/pdf.js` instead.

## Layout

```
extensions/chrome/
├── manifest.json         MV3, host_permissions <all_urls>
├── build.sh              pack for the store
└── src/
    ├── background.js     service worker: captures, uploads, badges
    ├── content.js        runs in the page: reads and measures only
    ├── pdf.js            JPEG tiles into a PDF, no library
    ├── config.js         origin, endpoints, settings
    ├── popup.html/js     three buttons, then closes
    └── options.html/js   what to do about a repeat capture
```

## Why the work sits where it does

**The service worker uploads, not the popup.** Chrome destroys a popup when it
loses focus, which kills an upload in flight. A capture can take tens of
seconds.

**The service worker fetches images, not the content script.** Chrome treats a
content script's cross-origin request as the page's own, so a third-party image
would need that host to send `Access-Control-Allow-Origin`. Most do not.

**The content script only reads.** It serializes, measures and harvests
metadata. Everything with a network or a canvas in it happens in the worker.

## Permissions

`<all_urls>` is required, not optional. Image inlining and PDF passthrough both
fetch from arbitrary hosts, and the worker can only do that for origins it holds
permission for.

`activeTab`, `scripting` and `tabs` drive the capture. `storage` holds one
setting.

## Authentication

There is none to configure. Requests carry this browser's FreeMemo session
cookie. Chrome treats an extension request as same-site when the extension holds
host permissions for the target, so the cookie attaches.

Sign in to FreeMemo in this browser and the extension follows. Sign out and it
stops working.

## What the badge means

| Badge | Meaning |
|---|---|
| `…` | Working |
| `✓` | Saved as a new topic |
| `=` | Already captured — the server returned what you had |
| `!` | Failed. Click again to retry. |

Nothing retries by itself. A retry after a lost response can create a second
copy of an upload that actually arrived, so the decision stays with you.

## Limits

A capture records what the page chose to render. On a virtualized surface —
a long comment thread, an infinite feed — that is a fraction of what exists.
One measured GitHub issue rendered 26 of 404 comments.

Captures are absent from FreeMemo's default fuzzy search when they are long.
Exact search still finds them.
