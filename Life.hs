
data State = Alive | Dead
     deriving (Show, Eq)

type Board = [[State]]

boardSize = 20
testBoard = replicate boardSize Alive : replicate 19 (replicate boardSize Dead)

main = game testBoard

game :: Board -> IO()
game board = do
  printBoard board
  putStrLn "q to quit, enter for next step"
  input <- getChar
  case input of
    'q' -> putStrLn "\nBye bye"
    _ -> game $ step board


step :: Board -> Board
step board = [[life (getPosition board (x, y)) (getNeighbors board (x,y))| x <- [0..(boardSize-1)]] | y <- [0..(boardSize-1)]]

life :: State -> [State] -> State
life Alive neighbors | living neighbors < 2 = Dead
                     | living neighbors `elem` [2,3] = Alive
                     | living neighbors > 3 = Dead
life Dead neighbors | living neighbors == 3 = Alive
                    | otherwise = Dead 

living :: [State] -> Int
living = length . filter (== Alive)

printBoard :: Board -> IO()
printBoard a = do
               let translatedMap = map (map convert) a
               putStrLn $ unlines translatedMap
  where convert Alive = '0'
        convert Dead = ' '

getPosition :: Board -> (Int, Int) -> State
getPosition board (x,y) = board !! y !! x

getNeighbors board position = map (getPosition board)
                            $ filter (\(x, y) -> x >= 0
                                              && y >= 0
                                              && x < boardSize
                                              && y < boardSize)
                            $ neighbors position

neighbors (x,y) = [ (x-1, y-1)
                  , (x-1, y)
                  , (x-1, y+1)
                  , (x, y-1)
                  , (x, y+1)
                  , (x+1, y-1)
                  , (x+1, y)
                  , (x+1, y+1)
                  ]
