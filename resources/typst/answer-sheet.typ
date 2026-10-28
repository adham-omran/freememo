// The printable answer sheet for one comprehension cycle.
//
// freememo.answer-sheet writes the cycle to data.json beside this file and
// compiles it. Every value from data.json is a string or a number shown as
// text, never evaluated as markup. A prompt that contains `#read(...)` prints
// those characters. Keep it that way: do not pass a value to `eval`.
//
// The sheet carries no answer key. data.json holds only what a learner may
// see before answering.

#let data = json("/data.json")

#set document(title: data.title)
#set page(
  paper: "a4",
  margin: (x: 2cm, top: 2.4cm, bottom: 2cm),
  header: context text(size: 8pt, fill: luma(110))[
    #data.title (cycle #data.cycle)
    #h(1fr)
    Page #counter(page).display() of #counter(page).final().first()
  ],
)
#set text(font: ("Libertinus Serif", "Noto Naskh Arabic"), size: 11pt, lang: "en")
#set par(leading: 0.7em)

#let rule = line(length: 100%, stroke: 0.4pt + luma(170))

// Ruled writing lines, spaced for handwriting.
#let ruled(count) = {
  for _ in range(count) {
    v(0.85cm)
    rule
  }
}

#let answer-line = {
  v(0.5cm)
  grid(columns: (auto, 1fr), column-gutter: 0.6em, align: bottom,
    text(fill: luma(90))[Answer], rule)
}

// Writing lines per essay format. The first `kept` lines stay on the page
// with their question. The rest may continue on the next page, so a long
// essay never pushes its question off a page that has room to start it.
#let essay-lines = ("short-essay": 8, "long-essay": 18)
#let kept = 3

#let points(n) = if n == 1 [1 point] else [#n points]

#text(size: 17pt, weight: "bold", data.title)

#grid(columns: (1fr, 1fr), column-gutter: 1.5cm, align: bottom,
  grid(columns: (auto, 1fr), column-gutter: 0.6em, align: bottom, [Name], rule),
  grid(columns: (auto, 1fr), column-gutter: 0.6em, align: bottom, [Date], rule))

#v(0.4cm)
#text(fill: luma(70))[
  Number each answer. If an answer continues on another page, write its
  question number there too. You can also answer on ordinary paper.
]

#for q in data.questions {
  let lines = essay-lines.at(q.format, default: 0)
  block(breakable: false, above: 1.1cm, width: 100%, {
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
        // column fits the longest id: a model may name one `option-two`.
        grid(columns: (auto, 1fr), column-gutter: 0.5em, row-gutter: 0.45em,
          ..q.options.map(o => ([#o.id.], o.text)).flatten())
      }
    }
    if lines == 0 { answer-line } else { ruled(calc.min(lines, kept)) }
  })
  if lines > kept { ruled(lines - kept) }
}
