module Solver where
    
import Prelude hiding (Word)
import System.IO.Unsafe

type Word = [Char]
type SecretWord = [Char]
type Board = [[Char]]
data FoundWord = FoundWord {row :: Int, col :: Int, dir :: Direction, word :: Word} deriving Show 
data Direction = N | NE | E | SE | S | SW | W | NW deriving Show

getAllDirections = [N, NE, E, SE, S, SW, W, NW]
-- indeksy rosną w dół i w prawo i zaczynają się od 1, TODO można przerobić
-- (1,1) ... (1,c)
-- ...
-- (r, 1) ... (r,c)
newXY :: Direction -> Int -> Int -> (Int, Int)
newXY N r c = (r-1,c)
newXY NE r c = (r-1,c+1)
newXY E r c = (r,c+1)
newXY SE r c = (r+1,c+1)
newXY S r c = (r+1,c)
newXY SW r c = (r+1,c-1)
newXY W r c = (r,c-1)
newXY NW r c = (r-1,c-1)

type FileName = [Char]
------------ Main function -----------
solve wordsFileName boardFileName = secretWord -- printBoard solvedBoard -- FIXME printować: wykreśloną tablice, gdzie znaleziono słowa(??), secret word
                where
                    board = readBoard boardFileName
                    words = readWords wordsFileName
                    foundWords = doSolve board words
                    solvedBoard = fst (crossFoundWords foundWords board)
                    secretWord = "Secret word: " ++ (snd (crossFoundWords foundWords board))

------------- The algorithm ------------
doSolve :: Board -> [Word] -> [FoundWord] -- TODO: przefiltrować słowa do sprawdzenia jesli juz je znaleziono
doSolve board words = flatten [findWordsForLetter x y board words | x <- [1..rows], y <- [1..cols]]
        where
            rows = length board
            cols = length (head board)


findWordsForLetter :: Int -> Int -> Board -> [Word] -> [FoundWord]
findWordsForLetter x y board [] = []
findWordsForLetter x y board (w:ws) = foundForSingleWord ++ findWordsForLetter x y board ws -- TODO: przefiltrować słowa do sprawdzenia jesli juz je znaleziono
    where foundForSingleWord = if getLetter board x y == head w then [FoundWord x y dir w | dir <- getAllDirections, matches x y dir (tail w) board] else []


matches :: Int -> Int ->  Direction -> [Char] -> Board -> Bool
matches x y dir [] board = True
matches x y dir (wordFirstLetter:restOfWord) board = isLegal && l == wordFirstLetter && matches row col dir restOfWord board 
    where
        row = fst (newXY dir x y)
        col = snd (newXY dir x y)
        isLegal = 0 < row && row <= length board && 0 <  col &&  col <= length (head board)  
        l = getLetter board row col 


getLetter :: Board -> Int -> Int -> Char
getLetter array row col = nth (nth array row) col



crossFoundWords :: [FoundWord] -> Board -> (Board, SecretWord)
crossFoundWords [] board = (board, secretWord)
        where 
            secretWord = filter (/='-') (foldl (++) "" board)
crossFoundWords (w:restFoundWords) board = crossFoundWords restFoundWords (replace indexesToReplace board rowsNo colsNo)
            where 
                replace :: [(Int, Int)] -> Board -> Int -> Int -> Board
                replace toRepl board rowsNo colsNo = [([(if elem (row', col') toRepl then '-' else getLetter board row' col')  | col' <- [1..colsNo]]) | row' <- [1..rowsNo]]
                indexesToReplace = wordsIndexes (row w) (col w) (length (word w)) (dir w)
                wordsIndexes :: Int -> Int -> Int -> Direction -> [(Int, Int)]
                wordsIndexes _ _ 0 _ = []
                wordsIndexes xx yy length dir = (xx,yy) : wordsIndexes (fst (newXY dir xx yy)) (snd (newXY dir xx yy)) (length - 1) dir
                rowsNo = length board
                colsNo = length (head board)                    

----------------------- IO --------------------
readBoard :: FileName -> Board
readBoard filename = doReadFromFile filename

readWords :: FileName -> [Word]
readWords filename = map (\str -> filter (\x -> x /= ' ' && x /= '-') str) (doReadFromFile filename)

doReadFromFile :: FileName -> [String]
doReadFromFile filename = split (=='\n') (unsafePerformIO . readFile $ filename) 

printBoard :: Board -> IO [()] -- TODO jakiś smiec sie tam jeszcze printuje
printBoard board = 
    sequence [putStrLn (getRow board line) | line <- [1..rowsNo]]
    where
        rowsNo = length board
        getRow :: Board -> Int -> [Char]
        getRow board r = [c | c <- nth board r]

    
--------------- Util functions  --------------------

-- Arrays start at 1 :(
nth :: [a] -> Int -> a 
nth [] n = error "IndexArrayOutOfBoundException" 
nth (x:xs) 1 = x
nth (x:xs) index = nth xs (index-1)

flatten xss = do
    xs <- xss
    x <- xs
    return x

split     :: (Char -> Bool) -> String -> [String]
split p s =  case dropWhile p s of
                "" -> []
                s' -> w : split p s''
                    where (w, s'') = break p s'