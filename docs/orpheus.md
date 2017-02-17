# Orpheus
---

For input, we will use MusicXML in which there is a large collection of
free databases where we can get data. For initial testing of the model
we will hard code our data in Hakaru.

For output, we will use a CSound Haskell embedding because it is the
simplist tool to produce sound from our model.

## Data Flow
```
           MusicXML
              |
              v
			Parse           <------- Haskell Data Model
              |
              |
              v
      Marshall to Hakaru    <------- Hakaru Data Model
              |
              |
              v
        -------------
        - Inference -        <------- Hakaru Probabilistic Model
        -------------
              |
              |
              v
     Marshall from Hakaru
              |
              v
       Generate CSound
```

## Questions to Ask the Data

* how long are the pieces?
* given a set of pieces, generate another
* find the key center of a piece
* given a sequence of `n` pieces, generate an `n+1` piece
* given a corpus and a subset of pieces, find the most probable author
