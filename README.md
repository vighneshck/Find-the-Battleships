# Guess the Ships
A two-player logical guessing game similar to Battleship.

To test-run the code, run ghci on the terminal:

`$ ghci`

In the prelude command prompt that opens up, load the code file:

`Prelude> :l Code.hs`

To feed in target locations and to find the number of guesses required to reach the correct solution from the given initial guesses, run:

`Prelude> runiter [Loc 'G' 3, Loc 'D' 3, Loc 'E' 1]` (you can use any triad of valid target locations)

To obtain an average number of guesses required to reach the correct solution from the given initial guesses, run:

`Prelude> let gam = genCombination 3 [(x:[]) ++ show y | x <- ['A'..'H'], y <- [1..4]]` (this will generate ALL 4960 possible solutions)
  
`Prelude> let totalguess = [runiter (map fromJust (map toLocation a)) | a <- gam]` (this will store the number of guesses it took to reach each possible solution)

`Prelude> sum totalguess` (divide this number by 4960 to obtain the average number of guesses required to reach the solution)

For the given initial guesses, it would take an average of ~6 guesses to correctly reach the solution.
  


