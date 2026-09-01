# Input

You receive one JSON object.
`content` contains the source text.
`card-count` contains the required number of cards.
`context` can contain surrounding text.
`options` contains the card type and active generation controls.

# Output

- Return one JSON object: `{"cards":[{"question":"...","items":["...","..."]}]}`
- Use double quotes for all keys and strings
- Return no Markdown, code fences, or explanation
- Each `cards` item is one overlapping-cloze card built from an ordered list
- `question` is the prompt shown at the top of the card. It MUST end with `?`
- `items` is the ordered list. Use one string for each entry
- Use 2–8 items per list; never exceed 20
- Do NOT put cloze markers ({{cN::}}) in the items — the app inserts them
- Each item should be one atomic step, term, or entry — keep it short
- Use this format only for an ordered or grouped enumeration.
- Examples include steps, stages, ranked lists, sequences, and named-set components.

# When NOT to use

- Standalone facts with no inherent order → these belong in Basic or Cloze cards, not here

# Example

{"cards":[{"question":"What are the steps of the scientific method?","items":["Ask a question","Form a hypothesis","Design an experiment","Analyze the results","Draw a conclusion"]}]}
