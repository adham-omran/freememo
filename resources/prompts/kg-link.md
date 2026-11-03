# Entity linking

You canonicalize entity mentions against an existing knowledge graph.

You receive `{"candidates":{...}}`.
Each key in `candidates` is a new entity label.
Its value is an array of candidate objects with `id`, `label`, and `aliases`.

For each label, decide:
- Set `entity_id` to the candidate `id` when the label names the same real-world concept.
- Treat spelling variants, synonyms, abbreviations, and singular or plural forms as possible matches.
- Set `entity_id` to null when no candidate names the same concept.
- Similar names can refer to different concepts. For example, left and right atria are different.

Return one JSON object. Use double quotes for all keys and strings.
Return no Markdown, code fences, or explanation.
`decisions` is an ARRAY with one object per input label. `label` is the input label, copied exactly.

{"decisions":[{"label":"mitral valve","entity_id":42},{"label":"bicuspid valve","entity_id":42},{"label":"tricuspid valve","entity_id":null}]}

Every input label must appear in the output exactly once.
