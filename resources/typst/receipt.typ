// The receipt for one completed credit order.
//
// freememo.receipt writes the order to data.json beside this file and compiles
// it. Every value is a string the caller formatted: the template formats no
// number or date, so what the PDF shows is what the email body shows. Values are
// shown as text, never evaluated as markup. Keep it that way: do not pass a
// value to `eval`.

#let data = json("/data.json")

#set document(title: "FreeMemo receipt " + data.number)
#set page(paper: "a4", margin: (x: 2.4cm, top: 2.6cm, bottom: 2.4cm))
#set text(font: ("Libertinus Serif", "Noto Naskh Arabic"), size: 11pt, lang: "en")

#let muted = luma(100)
#let rule(weight) = line(length: 100%, stroke: weight + luma(170))
#let row(label, value, strong: false) = {
  let style = if strong { (weight: "bold", size: 12pt) } else { (:) }
  grid(columns: (1fr, auto), align: (left, right),
    text(fill: if strong { black } else { muted }, ..style, label),
    text(..style, value))
}

#grid(columns: (1fr, auto), align: (left + top, right + top),
  {
    text(size: 20pt, weight: "bold", data.seller.name)
    linebreak()
    // A deployment with no site configured sends "", and shows the address alone.
    text(size: 9.5pt, fill: muted,
      (data.seller.site, data.seller.email).filter(s => s != "").join(linebreak()))
  },
  text(size: 20pt, fill: muted)[Receipt])

#v(0.9cm)
#rule(0.8pt)
#v(0.3cm)
#stack(spacing: 0.55em,
  row("Receipt number", data.number),
  row("Date paid", data.paid-at))
#v(0.3cm)
#rule(0.4pt)
#v(0.3cm)
#stack(spacing: 0.7em,
  text(fill: muted, grid(columns: (1fr, auto), align: (left, right), [Description], [Amount])),
  grid(columns: (1fr, auto), align: (left, right), data.line-item, data.amount))
#v(0.3cm)
#rule(0.8pt)
#v(0.3cm)
#stack(spacing: 0.55em,
  row("Amount paid", data.amount, strong: true),
  row("Credits added", data.credits))

#v(1fr)
// An address must not break at its hyphen: the reader would copy half of it.
#text(size: 9pt, fill: muted, hyphenate: false)[
  This receipt confirms a payment already made. \
  Questions: reply to the email this came with, or write to
  #box(data.seller.email).
]
