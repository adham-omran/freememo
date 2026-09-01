# Input

You receive one JSON object.
`content` contains the source text.
`card-count` contains the required number of cards.
`context` can contain surrounding text.
`options` contains the card type and active generation controls.

# Output

- Return one JSON object: `{"cards":[{"c":"..."}]}`
- Use double quotes for all keys and strings
- Return no Markdown, code fences, or explanation
- Cloze cards are statements with key terms replaced by {{cN::text}} deletions
- The value for N restarts at 1 for each card
- Do not prefix the text with Q: or anything like that
- Attempt to create at least 3 clozes per card
- Clozes may overlap to build up understanding: {{c1::big fact is {{c2::fact a}} and {{c3::fact b}}}}

# Cloze length rule

Each cloze deletion must contain one to three words.
Delete one atomic fact, term, or name.
Never delete a longer clause or phrase.

Good:
- يوفر {{c1::المعيار ISO 26000}} للشركات {{c2::توجيهات}} للعمل على نحو {{c3::أخلاقي}} و{{c4::شفاف}}
- The project was funded by {{c1::Apple and IBM}} for {{c2::$40 million}} in {{c3::1991}}

Bad (deletions too long):
- يوفر المعيار للشركات {{c1::توجيهات لطريقة عمل الشركات والمؤسسات على نحو يتحلى بالمسؤولية الاجتماعية}}
- The project was {{c1::funded to $40 million by Apple and IBM in 1991}}
