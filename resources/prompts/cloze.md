# Output

- Return your output as a sequence of Clojure/EDN maps `[{:c " "}]`
- Cloze cards are not necessarily questions, they can be statements with the clozes
- With the syntax {{cN:: Text}}The value for N restarts at 1 for each question
- Do not prefix the text wit Q: or anything like that
- The cloze text must not be more than 3 words
- Attempt to create at least 3 clozes per card
- Clozes may overlap to build up understanding {{c1:: big fact is {{c2::fact a}} and {{c3::fact b}}}}
