# cpsc312-crypto

Ask a question from the terminal:

```
> swipl main.pl
?- main(R).
Ask a question:
```

Right now supports these questions:

- What is the [*price/volume/change*] of [*coin name*]?
- What is the [*price/volume/change*] of [*coin name*] in [*currency*]?
- What is the [*price/volume/change*] of [*coin name*] at [*market name*]?
- What is the [*price/volume/change*] of [*coin name*] in [*currency*] at [*market name*]?

Eg:

 - What is the price of Bitcoin in USD at Bitstamp?

 ## TODOs

 - Add more coins and currencies
 - Add some variation to the sentence structure e.g. 'What's' instead of 'What is', non-capitalized things, etc.
 - Add some new types of questions e.g.:
   - Which markets sell Bitcoin in CAD?
   - What market has the highest price for Bitcoin?
 - Better terminal interaction? Could format answers better. Deleting anything at the "Ask a question" prompt breaks things. 
