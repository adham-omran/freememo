# Accessibility

FreeMemo targets **WCAG 2.2 Level AA**. As of 2026-07, all app states audited
below report zero AA violations under axe-core 4.10, in both light and dark
themes. This file documents what is implemented, how to operate the app by
keyboard, how to test, and the known gaps.

## Conformance status

Audited states (axe-core 4.10, WCAG 2.0/2.1/2.2 A+AA rules, light + dark):
landing (logged out), home, learn, learn session, document viewer, library
documents, library cards, search, import, settings, Add-Card form, Actions
modal. All zero violations. Automated coverage is necessary but not
sufficient — the manual checks below cover what axe cannot see.

## Implemented capabilities

### Structure and semantics
- `lang="en"` on the document; `<nav>` / `<main>` landmarks; a skip link is
  the first focusable element on every page.
- The Learn and Library tables expose one ARIA table each (`role=table` with
  explicit `columnheader`/`row`/`cell` roles) spanning the visually split
  header/body tables, so screen readers associate column headers with cells.
  Sortable headers expose `aria-sort`.
- All form controls have accessible names (visible labels on login,
  `aria-label` on compact filters and steppers).

### Color and contrast
- All text meets 4.5:1 and UI graphics 3:1 in **both** themes. The palette
  encodes this in `resources/public/freememo/index.css` design tokens:
  `--color-primary`/`--color-danger` are filled-control backgrounds (safe
  under white text); `--color-primary-text`/`--color-danger-text` are accent
  text colors (safe on page/card backgrounds). No single value can satisfy
  both roles in dark mode — keep the roles apart when adding UI.
- Links inside body text are underlined, not color-only.
- Inline content highlights force dark text so light-pastel highlights stay
  readable in dark mode (`.ql-editor [style*="background-color"]`).

### Keyboard operation
| Action | Keys |
| --- | --- |
| Skip past navigation | `Tab` (first stop) → `Enter` |
| Activate a row (open topic / edit card / show diff) | `Tab` to the row cell → `Enter` or `Space` |
| Expand/collapse a tree node | `Tab` to the arrow → `Enter` |
| Sort a library column | `Tab` to the header → `Enter` |
| Move (re-nest) a topic without dragging | `Tab` to the ⠿ grip → `Enter` arms the move → `Tab` to the destination row's grip → `Enter` completes; `Enter` on the armed grip or `Escape` cancels |
| Resize panels/panes | `Tab` to the divider (`role=separator`) → arrow keys; double-click still resets the main split |
| Command palette | `Cmd+K`; `↑`/`↓` or `Ctrl-n`/`Ctrl-p` (also `Cmd-p`) move selection; `Enter` runs; `Escape` closes. `Cmd-n` is browser-reserved (new window) and cannot be bound. |
| Leave a rich-text editor | `Tab` / `Shift+Tab` move focus like any control (Quill's Tab-to-indent bindings are removed — WCAG 2.1.2); `Escape` also blurs |
| Close any modal | `Escape`; focus returns to the element that opened it |

Focus is always visible: a 2px ring, drawn inset inside table cells (their
`overflow:hidden` clips outset rings), with the whole row tinted via
`:has()` so a focused cell reads as a row selection.

### Modals
Every modal gets `role="dialog"`, `aria-modal`, an accessible name, focus
moved inside on open, a Tab trap, Escape-to-close, and focus restore to the
opener — provided by `freememo.modal-shell/ModalEscape`. New modals should
call it rather than re-implement any part.

### Dynamic content
- Toasts use `role="alert"` and announce without focus stealing.
- Imported documents are repaired at render time
  (`freememo.rich-text-editor/apply-content-a11y!`): images that lost `alt`
  in Quill's Delta conversion are marked decorative, nameless links get
  href-derived labels, and code-block language pickers get labels. A
  MutationObserver re-applies this as content re-renders.
- Quill toolbars (both themes) get labeled pickers and buttons via
  `freememo.a11y/label-quill-toolbar!`.

### Touch and pointer
- Interactive targets are ≥24×24 CSS px (WCAG 2.5.8); `pointer: coarse`
  media queries widen the split dividers further on touch devices.
- Drag-and-drop re-nesting has the click/keyboard "move mode" alternative
  (WCAG 2.5.7) described in the keyboard table.

### Motion
- `prefers-reduced-motion: reduce` collapses all transitions/animations to
  ~instant (events still fire).

## Testing

### Automated (axe)
Run the dev server, log in as the test user, then drive axe over each state.
The audit script pattern (Playwright + axe-core 4.10) lives in the session
transcripts of the 2026-07 audit; the essential loop:

```js
await page.addScriptTag({ path: 'axe.min.js' });
const res = await page.evaluate(() => axe.run(document, {
  runOnly: { type: 'tag',
             values: ['wcag2a','wcag2aa','wcag21a','wcag21aa','wcag22aa'] }}));
// assert res.violations.length === 0 — repeat with data-theme="dark"
```

### Manual keyboard walk
Unplug the mouse and run the keyboard table above end to end. The two things
automation cannot check: every *click* affordance has a keyboard path, and
focus never disappears or traps.

### VoiceOver (macOS, works fine with Chrome)
1. Toggle VoiceOver: **Cmd+F5** (or triple-press Touch ID on laptops).
2. The VO modifier is **Ctrl+Option** (or Caps Lock if configured).
3. Navigate: `VO+→` / `VO+←` step through content; `VO+Space` activates.
4. Rotor: `VO+U` opens lists of landmarks / headings / links / form
   controls — verify the `nav`/`main` landmarks and table structure appear.
5. Tables: cursor into a table; `VO+Cmd+↑/↓/←/→` moves by cell and should
   announce the column header with each cell.
6. First run in Chrome: VoiceOver may ask to enable accessibility for the
   app; Chrome exposes its tree natively (inspect it at
   `chrome://accessibility` if something seems unannounced).

What to listen for here specifically: modal open announcements ("dialog,
Actions history"), the grip labels changing as a move is armed, and focus
stability across Electric's server-pushed re-renders.

## Known limitations

- User-picked **dark** Quill highlight colors defeat the forced-dark-text
  rule (assumes pastel highlights).
- Click-move re-nesting cannot target rows without grips (PDF page stubs);
  mouse drag can.
- Tree rows cost up to 3 Tab stops each (cell, grip, arrow); roving
  tabindex / ARIA tree arrow-navigation is a known follow-up.
- Responsive breakpoints are inconsistent (600/640/800px) — tracked
  separately; reflow at 320px passes today.

## Resources

- WCAG 2.2 quick reference: <https://www.w3.org/WAI/WCAG22/quickref/>
- Understanding WCAG (MDN): <https://developer.mozilla.org/en-US/docs/Web/Accessibility/Guides/Understanding_WCAG>
- ARIA Authoring Practices (dialog, table, window-splitter patterns): <https://www.w3.org/WAI/ARIA/apg/patterns/>
- axe-core rule descriptions: <https://dequeuniversity.com/rules/axe/4.10>
- VoiceOver user guide: <https://support.apple.com/guide/voiceover/welcome/mac>
