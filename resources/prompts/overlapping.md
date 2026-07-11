# Output

- Return your output as a sequence of Clojure/EDN maps `[{:question "..." :items ["...", "..."]}]`
- Each map is ONE overlapping-cloze card built from an ordered list
- `:question` is the prompt shown at the top of the card — it MUST be phrased as a question (end with "?")
- `:items` is the ordered list — one string per entry — in the order they must be recalled
- Use 2–8 items per list; never exceed 20
- Do NOT put cloze markers ({{cN::}}) in the items — the app inserts them
- Each item should be one atomic step, term, or entry — keep it short
- Only use this format for content that is genuinely an ordered or grouped enumeration: steps, stages, ranked lists, sequences, or the components of a named set

# When NOT to use

- Standalone facts with no inherent order → these belong in Basic or Cloze cards, not here

# Example

```
[{:question "What are the steps of the scientific method?"
  :items ["Ask a question"
          "Form a hypothesis"
          "Design an experiment"
          "Analyze the results"
          "Draw a conclusion"]}]
```
