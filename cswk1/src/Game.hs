--------------------------------------------------------------------------------
-- Functional Programming (CS256)                                             --
-- Coursework 1: Mastermind                                                   --
--------------------------------------------------------------------------------
-- Jack Piper
-- j.piper.2@warwick.ac.uk
--------------------------------------------------------------------------------

-- | This module should contain your game code.
module Game where
-- import Data.List
--------------------------------------------------------------------------------

-- | The number of pegs in a code.
pegs :: Int
pegs = 4

-- | Symbols are represented by characters.
type Symbol = Char

-- | The available symbols in the game.
symbols :: [Symbol]
symbols = ['a'..'f']

-- | A code is represented by a list of symbols.
type Code = [Symbol]

-- | Guesses are scored using coloured and white markers. The first component
-- of the pair gives the number of coloured markers and the right component
-- gives the number of white markers.
type Score = (Int, Int)

-- | A player is either human or computer-controlled.
data Player = Human | Computer

-- | The first codemaker in a play session.
codemaker :: Player
codemaker = Human

-- | The first guess the AI will make.
firstGuess :: Code
firstGuess = "aabb"

--------------------------------------------------------------------------------

-- | Determines whether a score indicates that a guess was correct or not.
------------------------------------
-- The first element in the tuple refers to the number of coloured markers, which
-- in the case of a correct solution should be exactly equal to the number of pegs.
-- The second element refers to the number of white markers, which should be 0
-- as none of the pegs should be in the wrong position. If both of these are
-- true, the guess is correct, otherwise it is incorrect.
correctGuess :: Score -> Bool
correctGuess score | (fst score == pegs) && (snd score == 0) = True
                   | otherwise = False

-- | This function should check that the code entered by a human player is
-- valid. In other words, it should have the length given by `pegs` and it
-- should only contain valid symbols.
------------------------------------
-- A code is valid if its length is the same as the number of pegs, and if
-- every character in the code is part of the symbols list. I created a function
-- to check through a list of characters to see if every element was also an
-- element in the symbols list. Then a simple if statement checks for valid
-- symbols and valid length, only returning true if both are valid.

allSymbol :: (Eq x) => [x] -> [x] -> Bool
allSymbol [y] xs = elem y xs
allSymbol (y:ys) xs | (elem y xs) && (allSymbol ys xs) = True
                    | otherwise = False

validateCode :: Code -> Bool
validateCode xs | (length xs == pegs) && (allSymbol xs symbols == True) = True
                | otherwise = False

-- | All possible codes.
------------------------------------
-- Using [[a,b,c,d] | a <- symbols, b <- symbols, c <- symbols, d <- symbols]
-- allows the symbol list to change, but if the number of pegs changes, the
-- function becomes useless.
codes :: [Code]
-- codes = permutations symbols
codes = [[a,b,c,d] | a <- symbols, b <- symbols, c <- symbols, d <- symbols]

--codeSize :: [x] -> Int
--codeSize [x] = 1
--codeSize (x:xs) = 1 + codeSize xs

-- | All possible scores.
------------------------------------
-- [Your explanation]
results :: [Score]
results = [(x,y) | x <- [0..pegs], y <- [0..pegs], (x + y <= pegs) && ((x,y) /= (3,1))]

-- | Scores a guess against a code. Symbols which are in the right place
-- and of the right type score a coloured marker. Symbols which are of the
-- right type but in the wrong place score a white marker.
------------------------------------
-- [Your explanation]

elemRemove :: Symbol -> Code -> Code
elemRemove _ []                         = []
elemRemove symbol (y:ys) | symbol == y  = elemRemove 'g' ys
                         | otherwise    = y : elemRemove symbol ys

calcWhite :: Code -> Code -> Int
calcWhite [x] code    | elem x code = 1
                      | otherwise = 0
calcWhite (x:xs) code | elem x code =  1 + (calcWhite xs newcode)
                      | otherwise = calcWhite xs code
                      where newcode = elemRemove x code

calcColour :: Code -> Code -> Int
calcColour [x] [y]        | x == y = 1
                          | otherwise = 0
calcColour (x:xs) (y:ys)  | x == y = 1 + calcColour xs ys
                          | otherwise = calcColour xs ys

score :: Code -> Code -> Score
score code guess = (calcColour code guess, calcWhite code guess - calcColour code guess)

-- | Chooses the next guess. If there is only one option left, choose it.
-- Otherwise, calculate the hit score for each code and choose the code
-- with the largest hit score.
------------------------------------
-- [Your explanation]
nextGuess :: [Code] -> Code
nextGuess [x] = x
-- nextGuess (x:xs) | scoreOfx > maxRest = x
--                  | otherwise = maxRest
--                  where maxRest = nextGuess xs
--                        scoreOfx = score x

-- | Remove all codes from the remaining possibilities which would result in
-- a different score for the guess if they were the code.
-- In other words, given the set of remaining possible codes, narrow it down
-- to those which would produce the same score we got from the codemaker.
------------------------------------
-- [Your explanation]
eliminate :: Score -> Code -> [Code] -> [Code]
eliminate lastScore guess [] = []
eliminate lastScore guess (x:xs)
        | score x guess == lastScore = x: eliminate lastScore guess xs
        | otherwise = eliminate lastScore guess xs

--------------------------------------------------------------------------------
