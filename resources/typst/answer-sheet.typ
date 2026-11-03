// The printable answer sheet for one comprehension cycle.
//
// freememo.answer-sheet writes the cycle to data.json beside this file and
// compiles it. Every value from data.json is a string or a number shown as
// text, never evaluated as markup. A prompt that contains `#read(...)` prints
// those characters. Keep it that way: do not pass a value to `eval`.
//
// The sheet carries no answer key. data.json holds only what a learner may
// see before answering.
//
// Layout: the question pages, then one bubble page for the multiple-choice
// questions. The scan reads the bubble page into one letter per question
// (resources/prompts/comprehension-answer-ocr.md).

#let data = json("/data.json")

#let bottom-margin = 2cm

#set document(title: data.title)
#set page(
  paper: "a4",
  margin: (x: 2cm, top: 2.4cm, bottom: bottom-margin),
  header: context text(size: 8pt, fill: luma(110))[
    #data.title (cycle #data.cycle)
    #h(1fr)
    Page #counter(page).display() of #counter(page).final().first()
  ],
)
#set text(font: ("Libertinus Serif", "Noto Naskh Arabic"), size: 11pt, lang: "en")
#set par(leading: 0.7em)

#let rule = line(length: 100%, stroke: 0.4pt + luma(170))

// One ruled writing line is this tall, spaced for handwriting.
#let line-gap = 0.85cm
// Exactly count × line-gap tall. The lines are placed in one block, because a
// `line` in the flow also gets block spacing, which made the pitch 1.3cm and
// pushed a long essay past the page.
#let ruled(count) = block(above: 0pt, below: 0pt, height: count * line-gap, width: 100%,
  for i in range(count) { place(top, dy: (i + 1) * line-gap, rule) })

#let field(label) = grid(columns: (auto, 1fr), column-gutter: 0.6em, align: bottom,
  text(fill: luma(90), label), rule)

// The fewest writing lines per essay format. An essay and these lines stay on
// one page. Lines are added after them only to fill a page bottom that the next
// question cannot use.
#let essay-lines = ("short-essay": 8, "long-essay": 18)
#let question-gap = 1.1cm

#let points(n) = if n == 1 [1 point] else [#n points]

#let bubbled = data.questions.filter(q => q.bubbled)

#let question(q) = block(breakable: false, above: question-gap, width: 100%, {
  grid(columns: (1fr, auto), align: bottom,
    text(weight: "bold")[Question #q.number],
    text(size: 9pt, fill: luma(110), points(q.points)))
  v(0.3cm)
  {
    // An Arabic or Hebrew question reads right to left, like the app shows it.
    set text(dir: rtl, lang: "ar") if q.rtl
    q.prompt
    if q.options.len() > 0 {
      v(0.2cm)
      // One grid, so every option's text starts in the same column. The id
      // column fits the longest id: an old form may name one `option-two`.
      grid(columns: (auto, 1fr), column-gutter: 0.5em, row-gutter: 0.45em,
        ..q.options.map(o => ([#o.id.], o.text)).flatten())
    }
  }
  let lines = essay-lines.at(q.format, default: 0)
  if lines > 0 { ruled(lines) } else if not q.bubbled { v(0.5cm); field[Answer] }
})

// Writing lines from here to the page bottom, when `next` would not fit in
// that space anyway. `next` is the following question, or none after the last
// one. Post: the lines end above the bottom margin, so they never start a page.
#let fill-page(next) = layout(size => context {
  let left = page.height - bottom-margin - here().position().y
  let need = if next == none { left + 1pt } else {
    measure(question(next), width: size.width).height + question-gap
  }
  if need > left {
    ruled(calc.max(0, calc.floor((left - 2pt) / line-gap)))
  }
})

#text(size: 17pt, weight: "bold", data.title)

#grid(columns: (1fr, 1fr), column-gutter: 1.5cm, align: bottom, field[Name], field[Date])

#v(0.4cm)
#text(fill: luma(70))[
  #if bubbled.len() > 0 [
    Mark each multiple-choice answer on the answer sheet on the last page.
  ]
  Write each essay on the lines below its question. If an answer continues on
  another page, write its question number there too. You can also answer on
  ordinary paper.
]

#for (i, q) in data.questions.enumerate() {
  question(q)
  if q.format in essay-lines {
    fill-page(data.questions.at(i + 1, default: none))
  }
}

// ── Bubble page ──────────────────────────────────────────────────────────────

#let bubble(id) = box(circle(radius: 0.3cm, stroke: 0.8pt + luma(60),
  align(center + horizon, text(size: 8pt, fill: luma(130), id))))

#let bubble-row(q) = (
  align(right + horizon, text(weight: "bold", str(q.number))),
  ..q.options.map(o => bubble(o.id)),
)

// At most 3 columns of rows. A grid of 21 multiple-choice questions fits in 2.
#let bubble-columns(qs) = {
  let count = if qs.len() <= 12 { 1 } else if qs.len() <= 30 { 2 } else { 3 }
  let per = calc.ceil(qs.len() / count)
  let width = calc.max(..qs.map(q => q.options.len()))
  grid(columns: (1fr,) * count, column-gutter: 1cm,
    ..range(count).map(c => grid(
      columns: (1cm,) + (auto,) * width, column-gutter: 0.35cm, row-gutter: 0.3cm,
      ..qs.slice(c * per, calc.min(qs.len(), (c + 1) * per)).map(bubble-row).flatten())))
}

#if bubbled.len() > 0 {
  pagebreak()
  text(size: 15pt, weight: "bold")[Multiple-choice answer sheet]
  v(0.3cm)
  field[Name]
  v(0.4cm)
  text(fill: luma(70))[
    Fill one circle for each question, completely, with a dark pen. To change an
    answer, draw an X through the wrong circle and fill the right one.
  ]
  v(0.6cm)
  bubble-columns(bubbled)
}
