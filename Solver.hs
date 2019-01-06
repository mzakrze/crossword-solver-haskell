module Solver where
    
import Prelude hiding (Word)
import System.IO.Unsafe

type Word = [Char]
type SecretWord = [Char]
type Board = [[Char]]
type GridXY = (Int, Int)
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
solve wordsFileName boardFileName = print 
                where
                    board = readBoard boardFileName
                    words = readWords wordsFileName
                    foundWords = doSolve board words
                    solvedBoard = fst (crossFoundWords foundWords board)
                    secretWord = "Secret word: " ++ (snd (crossFoundWords foundWords board))
                    print = printBoard board foundWords secretWord 

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

printBoard :: Board -> [FoundWord] -> [Char] -> IO [()] -- TODO jakiś smiec sie tam jeszcze printuje
printBoard board foundWords secretWord = 
    sequence (concat [[putStrLn (getRow board line) | line <- [1..rowsNo]], [putStrLn (show word) | (id, word) <- zip [0..] foundWords], [putStrLn (show secretWord)]])
    where
        rowsNo = length board
        wordsPositionsWithIndex = zip [0..] (map getFoundWordPositions foundWords)
        
        getRow :: Board -> Int -> [Char]
        getRow board r = concat [colorOutput [b] (getCharColor (c, r) wordsPositionsWithIndex)| (c, b) <- zip [1..] (nth board r)]
                 
        colorOutput :: [Char] -> Int -> [Char]
        colorOutput c nr = "\x1b[" ++ show (30 + nr) ++ "m" ++ c ++ " " ++ "\x1b[0m"

        getFoundWordPositions :: FoundWord -> [GridXY]
        getFoundWordPositions (FoundWord row col N word)  = [(col,row-i)   | i <- [0..((length word) - 1)]]
        getFoundWordPositions (FoundWord row col NE word) = [(col+i,row-i) | i <- [0..((length word) - 1)]] 
        getFoundWordPositions (FoundWord row col E word)  = [(col+i,row)   | i <- [0..((length word) - 1)]] 
        getFoundWordPositions (FoundWord row col SE word) = [(col+i,row+i) | i <- [0..((length word) - 1)]] 
        getFoundWordPositions (FoundWord row col S word)  = [(col,row+i)   | i <- [0..((length word) - 1)]] 
        getFoundWordPositions (FoundWord row col SW word) = [(col+i,row+i) | i <- [0..((length word) - 1)]]         
        getFoundWordPositions (FoundWord row col W word)  = [(col+i,row)   | i <- [0..((length word) - 1)]] 
        getFoundWordPositions (FoundWord row col NW word) = [(col+i,row-i) | i <- [0..((length word) - 1)]]   

        getCharColor :: GridXY -> [(Int, [GridXY])] -> Int
        getCharColor _ [] = 0
        getCharColor gridXY ((nr, tab):rest) | (checkWord gridXY tab) == True = 1 + mod nr 6
                                             | otherwise = getCharColor gridXY rest
        checkWord :: GridXY -> [GridXY] -> Bool
        checkWord _ [] = False
        checkWord (x,y) ((x1,y1):rest) | x == x1 && y == y1 = True
                                       | otherwise = checkWord (x,y) rest
    
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
