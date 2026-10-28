#!/usr/bin/env node
// Pre: campaign copy is accurate and the input is a real, sufficiently large screenshot.
// Post: standalone HTML, a 3600 x 2400 PNG, and an unchanged source image exist.
// Invariant: rendering does not modify the app, input screenshot, or repository assets.
import { readFile, writeFile, mkdir } from 'node:fs/promises';
import { createRequire } from 'node:module';
import { dirname, join, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';

const skillRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..');
const repoRoot = resolve(skillRoot, '../..');
const width = 1800, height = 1200, scale = 2;
const checkmark = '<svg viewBox="0 0 24 24" fill="none" aria-hidden="true"><path d="m5 12 4 4L19 6" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>';
const escapeHTML = value => value.replace(/[&<>"']/g, character => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[character]);

function argumentsForRender() {
  const flags = new Set(['--config', '--screenshot', '--output', '--font']);
  const values = {};
  const args = process.argv.slice(2);
  if (args.length === 1 && args[0] === '--help') return null;
  for (let index = 0; index < args.length; index += 2) {
    if (!flags.has(args[index]) || !args[index + 1] || args[index + 1].startsWith('--'))
      throw new Error('Use --config FILE --screenshot FILE --output DIRECTORY [--font FILE.woff2].');
    values[args[index].slice(2)] = resolve(args[index + 1]);
  }
  for (const key of ['config', 'screenshot', 'output'])
    if (!values[key]) throw new Error(`Supply --${key}.`);
  return values;
}

function text(value, field) {
  if (typeof value !== 'string' || !value.trim()) throw new Error(`${field} must contain text.`);
  return escapeHTML(value.trim());
}

function lines(value, field, minimum, maximum) {
  if (!Array.isArray(value) || value.length < minimum || value.length > maximum)
    throw new Error(`${field} must contain ${minimum} to ${maximum} text entries.`);
  return value.map((entry, index) => text(entry, `${field}[${index}]`));
}

function campaignTokens(copy) {
  const feature = text(copy.feature, 'feature');
  const headline = lines(copy.headline, 'headline', 2, 2);
  const description = lines(copy.description, 'description', 1, 2);
  const steps = lines(copy.steps ?? [], 'steps', 0, 4);
  const details = lines(copy.details ?? [], 'details', 0, 4);
  return {
    title: `FreeMemo — ${feature}`,
    feature,
    headline: headline.map((line, index) => `<span data-fit-line${index ? ' class="blue"' : ''}>${line}</span>`).join(''),
    description: description.map(line => `<span data-fit-line>${line}</span>`).join(''),
    workflowBlock: steps.length ? `<p class="workflow">${steps.join('<span>→</span>')}</p>` : '',
    detailsBlock: details.length ? `<div class="modes"><p class="modes-label">${text(copy.detailsLabel ?? 'Highlights', 'detailsLabel')}</p><div class="mode-grid">${details.map(line => `<div class="mode">${checkmark}<span data-fit-line>${line}</span></div>`).join('')}</div></div>` : '',
    footerBlock: copy.footer ? `<div class="closing">${checkmark}<span>${text(copy.footer, 'footer')}</span></div>` : '',
    screenshotAlt: text(copy.screenshotAlt ?? `${copy.feature} in FreeMemo.`, 'screenshotAlt'),
  };
}

async function fontFile(explicit) {
  if (explicit) return explicit;
  const directory = join(repoRoot, 'resources/public/freememo');
  const css = await readFile(join(directory, 'open-sans.css'), 'utf8');
  const latin = css.split('/* latin */').at(-1);
  const relative = latin.match(/url\(['"]?([^)'"\s]+\.woff2)/)?.[1];
  if (!relative) throw new Error('Cannot find the local Open Sans font. Supply --font FILE.woff2.');
  return resolve(directory, relative);
}

function screenshotType(bytes) {
  if (bytes.subarray(0, 8).equals(Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]))) return ['image/png', '.png'];
  if (bytes[0] === 255 && bytes[1] === 216 && bytes[2] === 255) return ['image/jpeg', '.jpg'];
  if (bytes.toString('ascii', 0, 4) === 'RIFF' && bytes.toString('ascii', 8, 12) === 'WEBP') return ['image/webp', '.webp'];
  throw new Error('Use a PNG, JPEG, or WebP screenshot.');
}

async function main() {
  const options = argumentsForRender();
  if (!options) {
    console.log('node scripts/render.mjs --config FILE --screenshot FILE --output DIRECTORY [--font FILE.woff2]');
    return;
  }
  const copy = JSON.parse(await readFile(options.config, 'utf8'));
  const tokens = campaignTokens(copy);
  let font;
  try { font = await readFile(await fontFile(options.font)); }
  catch (error) { throw new Error('Open Sans is unavailable. Run clj -X:build vendor/fetch! or supply --font FILE.woff2.', { cause: error }); }
  if (font.toString('ascii', 0, 4) !== 'wOF2') throw new Error('Supply a WOFF2 font file.');
  const screenshot = await readFile(options.screenshot);
  const [mime, extension] = screenshotType(screenshot);
  tokens.fontDataURL = `data:font/woff2;base64,${font.toString('base64')}`;
  tokens.screenshotDataURL = `data:${mime};base64,${screenshot.toString('base64')}`;
  tokens.brandLogo = await readFile(join(repoRoot, 'resources/public/freememo/favicon.svg'), 'utf8');
  const template = await readFile(join(skillRoot, 'assets/announcement.html'), 'utf8');
  const html = template.replace(/\{\{(\w+)\}\}/g, (_, key) => {
    if (!(key in tokens)) throw new Error(`Unknown template token: ${key}.`);
    return tokens[key];
  });
  let chromium;
  try { ({ chromium } = createRequire(join(repoRoot, 'playwright/package.json'))('@playwright/test')); }
  catch { throw new Error('Playwright is unavailable. Run npm ci --prefix playwright.'); }
  const browser = await chromium.launch({ headless: true });
  try {
    const page = await browser.newPage({ viewport: { width, height }, deviceScaleFactor: scale, colorScheme: 'light', reducedMotion: 'reduce' });
    await page.setContent(html);
    await page.evaluate(async () => {
      await document.fonts.ready;
      await document.querySelector('.preview img').decode();
    });
    const result = await page.evaluate(({ scale, width, height }) => {
      const failures = [];
      if (!document.fonts.check('750 90px "FreeMemo Open Sans"')) failures.push('The embedded font did not load.');
      for (const element of document.querySelectorAll('[data-fit-line]')) {
        const range = document.createRange();
        range.selectNodeContents(element);
        const parent = element.closest('.mode') ?? element.parentElement;
        if (range.getClientRects().length > 1 || range.getBoundingClientRect().right > parent.getBoundingClientRect().right + 1)
          failures.push(`Shorten this line: ${element.textContent}`);
      }
      for (const element of document.querySelectorAll('.eyebrow, .workflow, .modes-label'))
        if (element.scrollWidth > element.clientWidth + 1) failures.push(`Shorten this text: ${element.textContent}`);
      const copy = document.querySelector('.copy').getBoundingClientRect();
      const footer = document.querySelector('.closing')?.getBoundingClientRect();
      if (copy.bottom > (footer?.top ?? height - 72) - 24) failures.push('The copy overlaps the footer. Shorten the copy or feature list.');
      if (footer && footer.right > 752) failures.push('Shorten the footer to fit the left column.');
      const image = document.querySelector('.preview img');
      const box = image.getBoundingClientRect();
      if (image.naturalWidth + 1 < box.width * scale || image.naturalHeight + 1 < box.height * scale)
        failures.push('The screenshot lacks enough pixels. Capture the component at 3x device scale.');
      if (box.left < 792 || box.right > width || box.top < 0 || box.bottom > height)
        failures.push('The screenshot extends outside its layout area.');
      return { failures, sourceWidth: image.naturalWidth, sourceHeight: image.naturalHeight };
    }, { scale, width, height });
    if (result.failures.length) throw new Error(result.failures.join('\n'));
    await mkdir(options.output, { recursive: true });
    await writeFile(join(options.output, 'announcement.html'), html);
    await writeFile(join(options.output, `source-screenshot${extension}`), screenshot);
    await page.screenshot({ path: join(options.output, 'announcement.png'), animations: 'disabled' });
    console.log(JSON.stringify({ image: join(options.output, 'announcement.png'), html: join(options.output, 'announcement.html'), width: width * scale, height: height * scale, sourceWidth: result.sourceWidth, sourceHeight: result.sourceHeight }));
  } finally {
    await browser.close();
  }
}

main().catch(error => {
  console.error(`Marketing render failed: ${error.message}`);
  process.exitCode = 1;
});
