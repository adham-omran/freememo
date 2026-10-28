# Capture real FreeMemo screens

Use this guide when the user needs a new app screenshot.
If the user supplied a usable screenshot, skip the app server and browser setup.

## Prepare the source

- Read the feature's existing Playwright test for its route, controls, and ready selectors.
- Use a dedicated demo account and an isolated database for local fixtures.
- Read database settings and demo credentials from the environment. Never hardcode secrets.
- Use synthetic documents with useful titles and readable content. Exclude real user data.
- Avoid paid calls, external messages, and imports that require external accounts.
- A screenshot request authorizes local capture. It does not authorize publication or deployment.
- Reuse authorization already given in this chat. Do not request it again.

## Start a local app only when needed

Read the repository's current development commands before starting a server.
Start PostgreSQL alone with `docker compose up -d postgres` if no suitable instance exists.
The `ontop` container and the dev metadata navigator both use port 8081 by default.
Choose free app and metadata ports. Do not replace an existing server.

Disable SMTP before `freememo.logging/init!` can install email handlers.
Local configuration can contain live SMTP credentials even when environment variables are absent.
One local entrypoint is:

```clojure
(require 'dev 'freememo.config)
(with-redefs [freememo.config/smtp-config (constantly nil)]
  (dev/-main))
```

Run this through the current dev alias with isolated database settings in the environment.
Do not print configuration files, credentials, cookies, or complete WebSocket logs.

## Prove the source and choose the state

Call `assertServedBuildMatchesWorktree` from `playwright/support/build-identity.mjs` for a local worktree capture.
It checks CSS, client modules, and file identity. A dev commit value alone cannot identify the worktree.
For a deployed capture, record the target and release. Do not claim a local build is live.

Use `playwright/support/sign-in.mjs` with `FREEMEMO_USER` and `FREEMEMO_PASSWORD`.
Set `FREEMEMO_ENV=dev` for the local GET shim. Use a non-dev value for production POST login.
The helper waits for the authenticated shell instead of trusting a redirect.

Open panels through real controls. Do not change app DOM or CSS to invent feature behavior.
Choose a state that explains the feature, with its primary action fully visible.
For Split article, show a real preview, selected sections, and a meaningful nested heading.
Keep examples short enough to read at the final export size.
Use normal controls to close unrelated panels and remove tooltips.

## Capture at high resolution

Use a 1600 × 1200 viewport and device scale factor 3 for a 4800 × 3600 source.
Use light mode and reduced motion for the approved marketing style.
Wait for fonts, feature content, authenticated images, and any preview iframe content.
Use ready selectors and content assertions. Avoid fixed delays or `networkidle` for the reactive app.

Adapt this snippet in a temporary `.mjs` file inside `playwright/` so package imports resolve.
Supply the environment values and add the feature's real control sequence at the marked line.
`CAPTURE_COMPONENT` must select the panel itself, excluding the dimmed modal backdrop.

```js
import { chromium } from '@playwright/test';
import { mkdir } from 'node:fs/promises';
import { join } from 'node:path';
import { assertServedBuildMatchesWorktree } from './support/build-identity.mjs';
import { signIn } from './support/sign-in.mjs';

const { BASE_URL, OUTPUT_DIR, CAPTURE_ROUTE, CAPTURE_COMPONENT } = process.env;
if (!BASE_URL || !OUTPUT_DIR || !CAPTURE_ROUTE || !CAPTURE_COMPONENT)
  throw new Error('Supply BASE_URL, OUTPUT_DIR, CAPTURE_ROUTE, and CAPTURE_COMPONENT.');
await assertServedBuildMatchesWorktree(BASE_URL);
await mkdir(OUTPUT_DIR, { recursive: true });
const browser = await chromium.launch();
try {
  const page = await browser.newPage({ baseURL: BASE_URL,
    viewport: { width: 1600, height: 1200 }, deviceScaleFactor: 3,
    colorScheme: 'light', reducedMotion: 'reduce' });
  await signIn(page);
  await page.goto(CAPTURE_ROUTE);
  // Add real feature controls and content assertions here.
  const component = page.locator(CAPTURE_COMPONENT);
  await component.waitFor({ state: 'visible' });
  await page.evaluate(() => document.fonts.ready);
  await page.mouse.move(0, 0);
  const capture = { animations: 'disabled', caret: 'hide' };
  await page.screenshot({ ...capture, path: join(OUTPUT_DIR, 'screenshot.png') });
  await component.screenshot({ ...capture, path: join(OUTPUT_DIR, 'component.png') });
} finally {
  await browser.close();
}
```

Inspect both PNGs before composition. Confirm readable text, complete controls, and no private information.
Keep the raw screen and clean component crop beside the final marketing image.
Delete only fixtures and temporary files created for this capture.
Stop only the browser and server owned by this task. Leave shared services unchanged.
