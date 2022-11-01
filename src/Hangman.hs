module Hangman where

import Control.Monad
-- import Control.Monad.ST
import Data.Char
import Data.List
import Text.Printf

-- import Data.List

data GameObj = GameObj {lives :: Int, target :: String, hits :: String, mode :: Mode} deriving (Show)

data Mode = Init | Running | Won | Lost | End deriving (Show)

-- type GameState = ST GameObj

clear = putStr "\ESC[2J"

gameStateInitial = GameObj {lives = 9, hits = "", target = "", mode = Init}

testState = gameStateInitial {hits = "adn", target = "abdomination"}

processMode :: GameObj -> GameObj
processMode go
  | length (hits go) == length (nub $ target go) = go {mode = Won}
  | lives go <= 0 = go {mode = Lost}
  | otherwise = go

runningRound :: Char -> GameObj -> GameObj
runningRound c state =
  processMode resultObj
  where
    resultObj =
      if looseLife
        then state {lives = lives state - 1}
        else state {hits = c : hits state}
      where
        looseLife = (c `elem` hits state) || notElem c (target state)

--   if c `elem` hits state
--     then state {lives = lives state - 1}
--     else state {hits = c : hits state}

-- asd :: Char -> IO Char
-- asd c = return c

-- simulate = mapM_ print ['c', 'd', 'a']

type Round = IO (Maybe GameObj)

wonRound :: GameObj -> IO (Maybe GameObj)
wonRound state = do
  putStrLn "Again? y/n"
  ans <- getLine
  if ans == "y"
    then return $ Just gameStateInitial
    else return $ Just gameStateInitial {mode = End}

calcState :: Char -> GameObj -> IO (Maybe GameObj)
calcState _ state@GameObj {mode = Init} = return $ Just state {mode = Running}
calcState c state@GameObj {mode = Running} = return $ Just $ runningRound c state
calcState _ state@GameObj {mode = Won} = wonRound state
calcState _ state@GameObj {mode = Lost} = wonRound state
calcState _ state@GameObj {mode = End} = return Nothing

renderHits :: GameObj -> String
renderHits state = map (\c -> if c `elem` hits state then c else '_') (target state)

renderState :: GameObj -> IO ()
renderState state = do
  clear
  let messageFormat =
        "-------------------------- \n\
        \state: %s\n\
        \lives: %d\n\
        \guesses: %s\n"
  printf messageFormat (show $ mode state) (lives state) (renderHits state)

getGameInput :: IO Char
getGameInput = do
  head <$> getLine

runGame go = do
  renderState go
  c <- getGameInput
  go' <- calcState c go
  case go' of
    (Just go'') -> runGame go''
    Nothing -> return ()
