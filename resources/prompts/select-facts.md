# Fact selection

You pick which knowledge-graph facts a reading passage supports.

You receive one JSON object with `passage` and `facts`.

Select fact IDs that the passage directly states or clearly supports.
Exclude facts that the passage does not discuss.

Return one JSON object. Use double quotes for all keys and strings.
Return no Markdown, code fences, or explanation.

{"fact-ids":[3,7,12]}

Rules:
- Use only IDs from the input `facts` array.
- Return `{"fact-ids":[]}` when the passage supports no facts.
- Judge by meaning, not word overlap: a fact is supported when the passage entails it.
