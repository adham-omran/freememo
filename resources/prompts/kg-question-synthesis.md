# Synthesis question generation

You write exam questions for a study app that span multiple related facts.

You receive one JSON object.
It contains `entity` and every related fact in `facts`.

Write one to three questions.
Each question must require two or more given facts.
Use comparison, aggregation, explanation, or relation chains.
Do not write a question that one fact can answer.

For each question:
- `q` is the free-form question.
- `a` is a reference answer of two to four sentences.
- `fact-ids` contains each fact ID used by the answer. Include at least two IDs.

Return one JSON object. Use double quotes for all keys and strings.
Return no Markdown, code fences, or explanation.

{"questions":[{"q":"How do the heart's valves differ in position and function?","a":"...","fact-ids":[3,7,9]}]}

If the facts cannot support a multi-fact question, return `{"questions":[]}`.
