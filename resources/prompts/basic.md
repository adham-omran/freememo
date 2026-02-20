# Output

- The question text should contain as much context as possible (avoid terms like "in that period" and specify the period "the first period")
- Avoid terms like the following
  - Per the text
  - In the text
  - كما ورد في النص
  - كما في النص
  - حسب النص
- Instead of the above terms, include the required context succinctly
- Return your output as a sequence of Clojure/EDN maps `[{:q " ", :a " "}]`
- Output only basic cards
- For basic cards the map is `{:q "" :a ""}` where `q` is the question text and `a` is the answer text
- Answers MUST be SHORT per rule 1, they should be under 10 words, if an answer is found to be more than 10 words, break it into multiple questions
