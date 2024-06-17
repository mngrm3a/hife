module Hife.Grid
  ( Grid,
    CellState (..),
    CellIx,
    mkGrid,
    livingCells,
    evoluteBounded,
  )
where

import Control.Monad.ST.Strict (ST)
import Data.Maybe (mapMaybe)
import Data.Vector (MVector, Vector, modify)
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as MV

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

cellStates :: Grid -> Vector CellState
cellStates = _cells

livingCells :: Grid -> [CellIx]
livingCells (Grid nc _ cs) = V.ifoldl' go [] cs
  where
    go ixs ix Alive = ix2cix nc ix : ixs
    go ixs _ _ = ixs

evoluteBounded :: Grid -> Grid
evoluteBounded = evolute cix2ixBounded
  where
    cix2ixBounded nc nr cix@(cn, rn)
      | checkColumnBounds && checkRowBounds = Just $ cix2ix nc cix
      | otherwise = Nothing
      where
        checkColumnBounds = 0 <= cn && cn < nc
        checkRowBounds = 0 <= rn && rn < nr

-- Birth: A dead cell with exactly three live neighbors becomes alive.
-- Survival: A live cell with two or three live neighbors stays alive.
-- Death: In all other cases, a cell dies or remains dead.
evolute :: (Int -> Int -> CellIx -> Maybe Int) -> Grid -> Grid
evolute f (Grid nc nr cs) =
  Grid nc nr $ (`modify` cs) $ \mcs ->
    MV.iforM_ mcs $ \ix cell -> do
      countAliveNeighbors (f nc nr) mcs (ix2cix nc ix)
        >>= MV.write mcs ix . fate cell
  where
    fate Alive n | n == 2 || n == 3 = Alive
    fate Dead 3 = Alive
    fate _ _ = Dead

countAliveNeighbors ::
  (CellIx -> Maybe Int) ->
  MVector s CellState ->
  CellIx ->
  ST s Int
countAliveNeighbors toIx mcs (cn, rn) =
  sum <$> mapM cellValue (mapMaybe toIx neighbors)
  where
    neighbors =
      concat
        [ [(cn - 1, rn + dr) | dr <- [-1, 0, 1]],
          [(cn + 1, rn + dr) | dr <- [-1, 0, 1]],
          [(cn, rn + dr) | dr <- [-1, 1]]
        ]
    cellValue ix = numericValue <$> MV.read mcs ix
    numericValue Alive = 1
    numericValue Dead = 0

cix2ix :: Int -> (Int, Int) -> Int
cix2ix nc (cn, rn) = rn * nc + cn

ix2cix :: Int -> Int -> (Int, Int)
ix2cix _ 0 = (0, 0)
ix2cix nc ix = divMod ix nc
