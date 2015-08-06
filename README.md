# Solve Sudoku puzzles in Racket

## Motivation

I wanted to learn Scheme and I wanted to solve sudoku puzzles.

## Implementation

Puzzles are entered as 9x9 matrices:

'((1 2 0 4 5 6 0 8 9)
  (4 0 6 7 0 0 0 2 3)
  ...
  (3 1 2 6 0 4 9 7 8))

The solver operates in two stages. First, it enters any completely
determined spaces (recursively of course). If it gets stuck, phase
two picks a space and guesses each valid number in turn until it
finds a solution.

## Bugs

Don't give the solver a malformed or unsolvable puzzle yet, I'm not
sure what will happen.

## License

I wrote this all by my lonesome. Use it all you want, just don't try
to steal it for your homework assignment.
