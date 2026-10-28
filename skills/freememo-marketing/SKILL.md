---
name: freememo-marketing
description: Create FreeMemo marketing images and feature announcements from real product screenshots, using the approved blue-and-white announcement style. Use for Discord launch images, product teasers, and related promotional material.
---

# FreeMemo marketing

Create marketing material that pairs a real feature screenshot with a clear user benefit.
Keep this workflow local to the repository.
Preserve the user's supplied copy and format requirements.
For copy alone, write the announcement without starting the app or rendering an image.
For a raw screenshot, use the capture guidance without promotional framing.

## Start with the approved reference

Inspect [the approved announcement](references/style-reference.png) before designing.
It uses the actual Split article dialog, not a recreated interface.
The [original dialog capture](references/split-article-dialog.png) is available for template checks.
Use that example only when demonstrating Split article.

Preserve these design choices unless the user asks for a different direction:

| Role | Default |
|---|---|
| Canvas | 1800 × 1200 logical pixels, exported at 2× as a 3600 × 2400 PNG |
| Layout | Benefit copy on the left, real feature screenshot on the right |
| Background | White on the left, ice blue `#eef4ff` on the right |
| Accent | FreeMemo blue `#2563eb` |
| Text | Ink `#12243b`, secondary slate `#52647c` |
| Type | Self-hosted Open Sans, a large bold headline, quiet supporting copy |
| Spacing | About 90 pixels at the left edge, generous whitespace, no crowded footer |
| Screenshot | Full component, correct proportions, subtle shadow, visible primary action |

Use the actual logo in `resources/public/freememo/favicon.svg`.
Do not invent another FreeMemo logo.
The supplied template reads the logo and local font from the repository.

## Choose the message and evidence

Name one feature and one user benefit.
Confirm its behavior from the current code, approved spec, or a real app run.
Use a short two-line headline and one or two supporting lines.
Add a small workflow or feature list only when it explains the screenshot.
Keep release claims accurate.
A local build or pushed branch does not prove a production deployment.

Use natural marketing copy.
The repository's controlled prose rules do not apply to marketing copy.
Avoid implementation details, test counts, unsupported metrics, and generic praise in the artwork.

## Capture the actual feature

If the user supplies a suitable screenshot, framing it requires no app server or database.
Otherwise read [capture-guide.md](references/capture-guide.md).
It covers this repository's build check, sign-in helpers, demo setup, capture, and cleanup.

Use meaningful demo content and an informative UI state.
Show the actual controls and primary action.
Capture at 3× device scale when the final layout uses a 2× export.
Save the full original screen and a clean component crop when both help.
Preserve those captures unchanged.

Frame the screenshot with HTML/CSS.
Do not redraw the interface or change its content for the image.
Do not restyle the running app to imply a different product design.
If capture fails, use an accurate existing screenshot or report the limitation.
Do not substitute a generated interface and call it a screenshot.

## Render the announcement

Copy [the campaign example](assets/split-article.json) into the output directory.
Replace its copy with facts about the requested feature.
Use two headline lines.
Descriptions, steps, and details are arrays of plain text strings.
Steps, details, and the footer are optional.
The renderer escapes all campaign text.

Run from the repository root:

```bash
node skills/freememo-marketing/scripts/render.mjs \
  --config /absolute/path/campaign.json \
  --screenshot /absolute/path/real-component.png \
  --output /absolute/path/marketing-output
```

The renderer uses the existing `playwright` package dependencies.
If absent, run `npm ci --prefix playwright`.
Install Chromium with `cd playwright && npx playwright install chromium` if needed.
Use the normal permission flow when installation or browser launch requires it.
For permission failures, use that flow before changing browser launch options.

The font normally comes from the vendored Open Sans assets.
If those assets are absent, run `clj -X:build vendor/fetch!` or pass `--font /absolute/path/font.woff2`.
The renderer embeds the font, logo, and screenshot in a standalone HTML file.
It also saves a lossless PNG and an unchanged copy of the input screenshot.
It rejects overflowing text and screenshots that lack enough pixels for the layout.

Use the current visualization output directory when available.
Otherwise use an explicit output directory outside tracked source, such as `playwright/probe-out/marketing/`.
For a different requested aspect ratio, adapt the layout instead of stretching the finished PNG.
The bundled renderer implements the approved landscape default.

## Inspect and deliver

Open the exported PNG and inspect it visually.
Check headline fit, readable UI, whitespace, screenshot proportions, and the primary action.
Judge the image at Discord preview size as well as full size.
Change the framing or copy if it looks crowded.
Keep the source screenshot unchanged.

Return the image inline with its dimensions and a download link.
Offer the original screenshot when useful.
Creating an asset does not itself authorize posting, publishing, deployment, or a git commit.
Follow any explicit authorization already given for those actions.

## Reusable request

> Read `skills/freememo-marketing/SKILL.md`. Create a Discord announcement image for the requested FreeMemo feature in this style. Use a real screenshot, meaningful demo content, the actual logo, and a high-resolution PNG. Include the original screenshot and editable HTML.
