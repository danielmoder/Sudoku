-- Daniel Moder, May 2016

-- compiled with "ghc -o sudoku sudoku.hs"
-- sample usage:
   -- ./sudoku
   -- 024800070075004068308602000207040001000206000800090302000901706180700520060005810

-- general usage: provide a string of 81 digits representing an incomplete sudoku board,
-- starting from the top left, proceeding by row, and replacing blank slots with 0s


-- Improvements not yet made:
   -- in solveHelp, check Ps of ALL Zs (not just next), avoid unnecessary search
   -- in solveHelp, start by filling in Zs with fewest Ps (fewer branches)
   -- for the love of god, make one function for that mess of splits and joins

____________________________________________________________________________________

import Data.List



-- Defining Types --

type Row a = [a]
type Grid a = [Row a]

type Board = Grid Char
type PossBoard = Grid [Char]



-- COORDINATE FUNCTIONS --

data Coordinate = C Int Int
     deriving (Show, Eq, Ord)

getR :: Coordinate -> Int
getR (C r c ) = r

getC :: Coordinate -> Int
getC (C r c ) = c



-- BOARD FUNCTIONS --

-- Makes new object from list of characters
   --ASSUMPTION: 81 chars, only digits
makeBoard :: [Char] -> Board
makeBoard [] = []
makeBoard x = (take 9 x) : (makeBoard (drop 9 x))

-- Formats and prints board to console
showHelp :: Board -> String
showHelp [] = []						-- base case
showHelp (x:xs) = (intersperse ' ' x) ++ "\n" ++ showHelp xs	-- real work

showBoard :: Board -> IO()
showBoard board = putStrLn$ showHelp board


-- Returns locations of non-zeros:
nonZero :: Board -> [Coordinate]
nonZero b = [(C r c) | r <- [0..8], c <- [0..8], b!!r!!c /= '0']

-- Returns locations of zeros:
zero :: Board -> [Coordinate]
zero b = [(C r c) | r <- [0..8], c <- [0..8], b!!r!!c == '0']



-- STATE FUNCTIONS --

-- Contains a Board + corresponding PossBoard
data State = S Board PossBoard
           deriving (Show, Eq)

getB :: State -> Board
getB (S b pb) = b

getPB :: State -> PossBoard
getPB (S b pb) = pb

-- Returns list of zero-slots in board
getZs :: State -> [Coordinate]
getZs (S b pb) = zero b

-- Gets possible digits for next zero-slot in board
getPs :: State -> [Char]
getPs state = getIdxPB (getPB state) $ head (getZs state)

-- Returns true if State is solved
-- note: only checks if board is _filled_ (requires more work elsewhere)
solved :: State -> Bool
solved state = getZs state  == []



-- Returns list of states derived from inserting all possible values of
-- of the next zero-slot in the board
   --ASSUMPTION: only called on 
updateState :: State -> [State]
updateState state
		= let ps = getPs state		    -- [poss. values for empty]	
	              empty = head $ getZs state    -- next zero-slot of board
    	              board = getB state   	    
  	              pboard = getPB state

       	 in [(S b pb) | (b, pb) <- zip
	    	      	    	[setBoard board  empty x | x <- ps]
	    	      	    	[pruneAll pboard (toPrune empty) x | x <-ps]]
				
-- note to self: parallel comprehension equivalent



-- PRUNING FUNCTIONS --

-- Returns list of coordinates in the same 3x3 box as the given coordinate
box :: Coordinate -> [Coordinate]
box p = [(C r' c') | let r = (3 * quot (getR p) 3), let c = (3 * quot (getC p) 3), r' <- [r..r+2], c' <- [c..c+2]]


-- Returns list of coordinates in the same row as given coordinate
row :: Coordinate -> [Coordinate]
row p = [(C r c) | let r = getR p, c <- [0..8]]

-- Returns list of coordinates in the same column as given coordinate
col :: Coordinate -> [Coordinate]
col p = [(C r c) | r <- [0..8], let c = getC p]

-- Returns sorted list of coordinates in same box, row, or column
toPrune :: Coordinate -> [Coordinate]
toPrune c = sort ((box c) `union` (row c) `union` (col c))


-- SLOT FOR PRUNE ONCE ITS PRETTY --


-- Prunes a list of coordinates in a PossBoard for a single digit
pruneAll :: PossBoard -> [Coordinate] -> Char -> PossBoard
pruneAll pb [] v = pb
pruneAll pb (x:xs) v = pruneAll (prune pb x v) xs v
                     --Calls prune all again with singly-updated PossBoard,
		                          --remainder of list, and same value

-- Takes board in initial state (all lists of PossBoard = [1..9])
-- and prunes PossBoard according to values in board
initialPrune :: PossBoard -> Board -> [Coordinate] -> PossBoard
initialPrune pb b [] = pb
initialPrune pb b (x:xs) = let v = b!!(getR x)!!(getC x)

             in initialPrune (pruneAll pb (toPrune x) v) b xs




-- ABSTRACT THIS ISH --

-- Removes given digit from list at PossBoard coordinate (if there)
prune :: PossBoard -> Coordinate -> Char -> PossBoard
prune pboard coord v
                        = let r = getR coord
		      	      c = getC coord
			      f = fst (splitAt r pboard)
			      s = snd (splitAt r pboard)     -- f ++ s = pboard
			      h = head s
			      t = tail s		     -- h:t = s
			      f' = fst (splitAt c h)
			      s' = snd (splitAt c h)	     -- f' ++ s' = h
			      h' = head s'
			      t' = tail s'		     -- h':t' = s'
			      l = filter (\x -> x /= v) h'   -- remove if == v

		in f ++ (f' ++ l:t'):t	             	     -- link it back up


-- Sets the value of a slot in the board and returns updated board
setBoard :: Board -> Coordinate -> Char -> Board
setBoard b coord v
                        = let r = getR coord
			      c = getC coord
			      f = fst (splitAt r b)
			      s = snd (splitAt r b)	-- f ++ s = b
			      h = head s
			      t = tail s		-- h:t = s
			      f' = fst (splitAt c h)
			      s' = snd (splitAt c h)    -- f' ++ s' = h
			      h' = head s'
			      t' = tail s'              -- h':t' = s'
                              l = (\x -> v) h'  	-- set it to v

                in f ++ (f' ++ l:t'):t	    		-- link it back up


-- Returns the list of chars at a given index of a PossBoard
getIdxPB :: PossBoard -> Coordinate -> [Char]
getIdxPB pb coord
		= let r = getR coord
		      c = getC coord
		      f = fst (splitAt r pb)
		      s = snd (splitAt r pb)	-- f ++ s = pb
		      h = head s
		      t = tail s                -- h:t = s
		      f' = fst (splitAt c h)
		      s' = snd (splitAt c h)    -- f' ++ s' = h
		      h' = head s'
		      t' = tail s'              -- h':t' = s'
		      l = (\x -> x) h'  	-- get value

	in l	      	      	    		-- return value






-- Returns solved state(s?)
solveHelp :: [State] -> [State]
solveHelp states
	| states == [] = []					-- Failure
	| length states == 1 && solved (head states) = states	-- Success
	| otherwise = solveHelp . filter (\state -> (solved state) || (getPs state /= [])) . concat . map updateState $ states

-- note (mainly to self): "Chronological" order is reverse of dot-operator order
   -- call updateState on all of states ->
   -- flatten the lists into one list ->
   -- filter out failed branches (state !solved && no possibilities left) ->
   -- solveHelp


-- Calls solveHelp on list of updated states, returns solved state
solve :: State -> State
solve state = head . solveHelp . updateState $ state



-- MAIN FUNCTIONS --

-- Takes string of chars, makes, solves, and returns board, and prints it
mainHelp :: String -> IO ()
mainHelp str  =
     	    let board = makeBoard str
	    	newPB = replicate 9 $ replicate 9 ['1'..'9']
		pb = initialPrune newPB board (nonZero board)
		s0 = (S board pb)
		sf = solve s0
		finalBoard = getB sf

	in showBoard finalBoard

-- Takes input from console (for proper function, must be 81 digits), and solves
main :: IO ()
main = do
     line <- getLine
     mainHelp line
     main