-- Author: Vighnesh Chenthil Kumar <vchenthilkum@student.unimelb.edu.au>
-- Purpose: Implement a two-player logical guessing game similar to Battleship
--
-- The game is played on a 4x8 grid, with columns labelled from [A..H] and rows 
-- numbered from [1..4]). This game involves a "guesser" trying to guess
-- the location of all 3 battleships correctly, and a "hider" who has hidden the
-- ships and returns a feedback (consisting of 3 integers) for each guess made 
-- by the "guesser", depending on how close the guesses are to the actual 
-- locations. The game ends when the "guesser" correctly guesses the locations 
-- of all the 3 battleships in one go. 

module Proj1 (Location, toLocation, feedback,
              GameState, initialGuess, nextGuess) where
   
import Data.List

-- A new datatype called Location is created, with a data constructor Loc, and 
-- it accepts a Char (eg: 'A') and an Int (eg: 1) value.
-- eg: Loc 'A' 1

data Location = Loc Char Int
    deriving (Eq)

-- A type called GameState is created, which contains a list of list of strings.
-- The GameState is required to hold the possible combinations of the three game
-- cells that can be chosen by the guesser.
-- eg: [["A1","A2","A3"], ["A2","A3","A4"]....]

type GameState = [[String]]

-- This is an instance declaration such that the Location datatype has access
-- to the Show class. Using this, we can convert a variable of Location datatype
-- to a string (for comparing with the locations in the GameState).

instance Show Location where
    show (Loc c i) = (c:[]) ++ show i

-- The toLocation function takes in a string, and typecasts it into the 
-- (Maybe) Location datatype.
-- eg: "A1" will convert into Loc 'A' 1, while "helloworld" will return an error

toLocation :: String -> Maybe Location
toLocation strng
    | elem (head strng) ['A'..'H'] && elem (last strng) ['1'..'4'] 
      = Just (Loc (head strng) (read (tail strng) :: Int))  
    | otherwise = Nothing

-- The toString function converts a variable of Location datatype to a String 
-- type.
-- eg: Loc 'A' 1 becomes "A1"

toString :: Location -> String
toString (Loc c i) = show (Loc c i)

-- The fromJust function takes a variable of Maybe Location datatype, and 
-- converts it into Location datatype, since every other funtion only accepts a 
-- Location datatype.

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "no value to extract"

-- Given a list of possible game cells ["A1".."H4"], the genCombination function
-- returns a list of all possible ways of choosing n game cells.

genCombination :: Int -> [a] -> [[a]]
genCombination 0 _ = [[]]
genCombination _ [] = []
genCombination n (x:xs) = ((map (x:) (genCombination (n-1) xs)) 
                            ++ (genCombination n xs))
-- genCombination n (x:xs) = filter (\x -> length x == n) (subsequences (x:xs))
-- This one is too computationally expensive as there is a list created with
-- 2^32 elements, which might exceed memory bounds.

-- The actual guessing functionalities are implemented BELOW.
-- The following are some location combinations (initial guess candidates)
-- that were tested against all 4960 possible target location combinations to 
-- find a good initial guess based on the lowest average number of guesses that 
-- were required to correctly guess the location of all 3 battleships.
--
-- ----------------------------------------------------------------
-- |    Candidate Initial guess    |    Average no. of guesses    |
-- ----------------------------------------------------------------
-- |            (A1,A2,A3)         |            8.26              |
-- |            (H1,H2,H3)         |            7.14              |
-- |            (A1,B2,C3)         |            8.22              |
-- |            (H1,G2,F3)         |            6.69              |
-- |            (C1,D1,E1)         |            7.67              |
-- |            (C4,D4,E4)         |            7.36              |
-- |            (C1,D2,E3)         |            7.54              |
-- |            (E1,D2,C3)         |            7.70              |
-- |            (D1,D2,F3)         |            6.73              |
-- |            (G3,H3,H1)         |            6.75              |
-- |            (G3,A2,E1)         |            6.32              |
-- |            (G3,D3,E1)         |            6.15              |
-- |            (A1,H2,E4)         |            6.09              |
-- |            (H1,A2,D4)         |            6.49              |
-- |            (H4,A3,D1)         |            6.63              |
-- |            (A4,H3,E1)         |            6.07  <---        |
-- ----------------------------------------------------------------
--
-- As we can see from above, (A4,H3,E1) is a good initial guess to finish the
-- game within ~6 guesses on an average.

-- The initialGuess function returns an initial guess (here, [A4,H3,E1]) 
-- required to start the guessing game, and a GameState having all 4960 (32c3)
-- location combinations (of any 3 locations) at the beginning of the game.

initialGuess :: ([Location],GameState)
initialGuess = ([Loc 'A' 1, Loc 'H' 2, Loc 'E' 4], 
             genCombination 3 [(x:[]) ++ show y | x <- ['A'..'H'], y <- [1..4]])

-- The nextGuess function simply takes in previousGuess, previousGameState and 
-- previousFeedback, and returns a new guess along with the pruned GameState
-- (which now has lesser number of possible target location combinations 
-- compared to the previousGameState). The goal of this game is to prune the
-- GameState until only the actual target location combination is left.
-- Note: For now, the first element of the pruned GameState is returned as the
-- new Guess. From whatever experiments, this was found to have returned 
-- relatively lesser number of guesses.

nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (prevLoc, prevGS) (x,y,z) =  (newLoc, newGS)
    where newLoc = map fromJust (map toLocation (middle newGS))
          newGS = secondPrune prevLoc prevGS (x,y,z)


middle :: [a] -> a
middle lst = lst !! (div (length lst) 2)

-- The secondPrune function prunes an already pruned GameState (by the 
-- firstPrune function). Here, we try to find all potential target location
-- combinations by checking the feedback that it would give our previousGuess.
-- If it is the same as previousFeedback, then it is a possible solution.
-- Note: The secondPrune function alone is enough to guess the solution, but we 
-- add the firstPrune function only to reduce computational complexity.

secondPrune :: [Location] -> GameState -> (Int,Int,Int) -> GameState
secondPrune prevLoc prevGS (x,y,z) = [a| a <- firstPrune prevLoc prevGS (x,y,z), 
                  feedback (map fromJust (map toLocation a)) prevLoc == (x,y,z)]

-- The firstPrune function is the first step of pruning, wherein it takes the
-- previousGameState and filters out location combinations which have locations 
-- that are n steps away from a previousGuess location. The value of n is 
-- determined from the previousFeedback values, as shown in the 4 conditions.
-- If no filtering is possible, we know we can remove the previousGuess atleast, 
-- as the game would have ended otherwise.

firstPrune :: [Location] -> GameState -> (Int,Int,Int) -> GameState
firstPrune prevLoc prevGS (a,b,c)
    | a == 0 && b == 0 && c == 0 = updateGS 2 prevLoc prevGS -- here, n=2
    | a == 0 && b == 0 && c /= 0 = updateGS 1 prevLoc prevGS 
    | a == 0 && b /= 0 && c /= 0 = updateGS 0 prevLoc prevGS 
    | a == 0 && b /= 0 && c == 0 = updateGS 0 prevLoc prevGS 
    | otherwise = delete (map toString prevLoc) prevGS

-- The updateGS updates the previousGameState by including the location
-- combinations that have locations which are NOT n steps away from any of the 
-- previousGuess locations, using the filterNotN function.

updateGS :: Int -> [Location] -> GameState -> GameState
updateGS n [] prevGS = prevGS
updateGS n (x:xs) prevGS = updateGS n xs [a | a <- prevGS, filterNotN n x a]

-- The filterNotN function returns True only if all 3 locations (in a location
-- combination) are NOT n steps away from a previousGuess location.

filterNotN :: Int -> Location -> [String] -> Bool
filterNotN n l [] = True
filterNotN n l (x:xs)
    | abs (fromEnum (head (toString l)) - fromEnum (head x)) <= n 
      && abs (fromEnum (last (toString l)) - fromEnum (last x)) <= n = False
    | otherwise = filterNotN n l xs

-- The feedback functions takes a target location combination and a
-- guessed location combination, and returns a feedback having 3 integers:
-- (i) The number of correctly guessed locations
-- (ii) The number of guesses which are at a distance of 1 from a battleship
-- (iii) The number of guesses which are at a distance of 2 from a battleship

feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback targetLoc [] = (0,0,0)
feedback targetLoc (x:xs)
    | elem x targetLoc = (p+1,q,r)
    | distanceN 1 (toString x) (map toString targetLoc) = (p,q+1,r) -- here, n=1 
    | distanceN 2 (toString x) (map toString targetLoc) = (p,q,r+1)
    | otherwise = (p,q,r)
    where (p,q,r) = feedback targetLoc xs

-- The distanceN function returns True if the given location is at a distance of
-- n from ANY of the locations given in the list of locations (third argument).

distanceN :: Int -> String -> [String] -> Bool
distanceN n l [] = False
distanceN n l (x:xs)
    | abs (fromEnum (head l) - fromEnum (head x)) <= n 
      && abs (fromEnum (last l) - fromEnum (last x)) <= n = True
    | otherwise = distanceN n l xs

runiter :: [Location] -> Int
runiter target = ru initialGuess
  where
    ru (guess, gs) = let fb = feedback target guess
                      in  if fb == (3,0,0) then 1 else 1 + ru (nextGuess (guess, gs) fb)
