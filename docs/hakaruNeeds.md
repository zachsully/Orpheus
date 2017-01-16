# Hakaru

In order to module music we will need some kind of recursion. Here are
three areas of the model where we would want recursion for:

1. We can say that the shortest note one can play is a 128th note, but
   theoretically we could play a contably infinitely small note.

2. More importantly we would want recursion in the length of the piece.
   A sample from a musical distribution could contain a score of just one
   bar or hundreds.

3. We would also want recursion for parallel composition of pieces. For
   instance, we might want to be able to draw a 2 part harmony and 4 part
   harmony from the same sample.
