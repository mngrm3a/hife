module Hife.Grid
  ( Grid,
    CellState (..),
    CellIx,
    mkGrid,
    livingCells,
    evoluteBounded,
    evoluteToroidal,
    cix2ix,
    ix2cix,
  )
where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V

type CellIx = (Int, Int)

data CellState = Dead | Alive deriving (Eq, Show)

data Grid = Grid
  { _numCols :: !Int,
    _numRows :: !Int,
    _cells :: !(Vector CellState)
  }
  deriving (Eq, Show)

mkGrid :: Int -> Int -> [CellIx] -> Grid
mkGrid nc nr acs =
  Grid nc nr $ V.generate (nc * nr) $ \ix ->
    if ix2cix nc ix `elem` acs then Alive else Dead

livingCells :: Grid -> [CellIx]
livingCells (Grid nc _ cs) = V.ifoldl' go [] cs
  where
    go ixs ix Alive = ix2cix nc ix : ixs
    go ixs _ _ = ixs

evoluteBounded :: Grid -> Grid
evoluteBounded = evolute neighbors
  where
    neighbors nc nr i =
      [ i + dx + dy * nc
        | dx <- [-1, 0, 1],
          dy <- [-1, 0, 1],
          dx /= 0 || dy /= 0,
          let nx = (i `mod` nc) + dx,
          let ny = (i `div` nc) + dy,
          nx >= 0,
          nx < nc,
          ny >= 0,
          ny < nr
      ]

evoluteToroidal :: Grid -> Grid
evoluteToroidal = evolute neighbors
  where
    neighbors nc nr i =
      [ (ny `mod` nr) * nc + (nx `mod` nc)
        | dx <- [-1, 0, 1],
          dy <- [-1, 0, 1],
          dx /= 0 || dy /= 0,
          let nx = (i `mod` nc) + dx,
          let ny = (i `div` nc) + dy
      ]

-- Birth: A dead cell with exactly three live neighbors becomes alive.
-- Survival: A live cell with two or three live neighbors stays alive.
-- Death: In all other cases, a cell dies or remains dead.
evolute :: (Int -> Int -> Int -> [Int]) -> Grid -> Grid
evolute nbs (Grid nc nr cs) = Grid nc nr $ (`V.imap` cs) $ \ix cell ->
  fate cell $ length $ filter (== Alive) $ map (cs !) $ nbs nc nr ix
  where
    fate Alive n | n == 2 || n == 3 = Alive
    fate Dead 3 = Alive
    fate _ _ = Dead

cix2ix :: Int -> (Int, Int) -> Int
cix2ix nc (cn, rn) = rn * nc + cn

ix2cix :: Int -> Int -> (Int, Int)
ix2cix _ 0 = (0, 0)
ix2cix nc ix = divMod ix nc
