# Input

You receive one JSON object.
`content` contains the source text.
`card-count` contains the required number of cards.
`context` can contain surrounding text.
`options` contains the card type and active generation controls.

# Output

- Give the question enough context to stand alone.
- Replace vague terms such as "in that period" with the exact period.
- Avoid terms like the following
  - Per the text
  - In the text
  - كما ورد في النص
  - كما في النص
  - حسب النص
- Instead of the above terms, include the required context succinctly
- Return one JSON object: `{"cards":[{"q":"...","a":"..."}]}`
- Use double quotes for all keys and strings
- Return no Markdown, code fences, or explanation
- Output only basic cards
- Each card has `q` for the question and `a` for the answer
- Answers MUST contain fewer than 10 words.
- Shorten a long answer. Do not split it into extra cards.
