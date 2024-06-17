module Main (main) where

import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Hife.Grid (Grid, evoluteBounded, livingCells, mkGrid)

main :: IO ()
main = do
  scrSize <- getScreenSize
  Gloss.simulate
    Gloss.FullScreen
    Gloss.white
    1
    (mkWorld scrSize 20)
    renderWorld
    (const updateWorld)

data World = World
  { _windowSize :: !(Int, Int),
    _cellSize :: !Float,
    _grid :: !Grid
  }

mkWorld :: (Int, Int) -> Int -> World
mkWorld winSize@(winWidth, winHeight) cellSize =
  World winSize (fromIntegral cellSize) $
    mkGrid
      (winWidth `div` cellSize)
      (winHeight `div` cellSize)
      [(c, r) | c <- [1 .. 40], r <- [1 .. 10]]

renderWorld :: World -> Gloss.Picture
renderWorld (World ws@(w, h) s g) =
  drawGridBackground w h s
    <> foldMap (drawCell ws s) (livingCells g)

updateWorld :: Float -> World -> World
updateWorld _ w = w {_grid = evoluteBounded $ _grid w}

drawCell :: (Int, Int) -> Float -> (Int, Int) -> Gloss.Picture
drawCell (width, height) size pos =
  Gloss.rectangleSolid size size
    & Gloss.translate x y
    & Gloss.color (Gloss.orange & Gloss.light)
  where
    (x, y) = mapGridToPoint width height size pos

drawGridBackground :: Int -> Int -> Float -> Gloss.Picture
drawGridBackground width height size =
  (horizontalPaths <> verticalPaths)
    <&> Gloss.line
    & Gloss.pictures
    & Gloss.color (Gloss.greyN 0.95)
  where
    horizontalPaths =
      [ [(x + cX, y + cY), (x + cX + size, y + cY)]
        | x <- [(-hsW), (-hsW) + size .. hsW - size],
          y <- [(-hsH), (-hsH) + size .. hsH]
      ]
    verticalPaths =
      [ [(x + cX, y + cY), (x + cX, y + cY + size)]
        | x <- [(-hsW), (-hsW) + size .. hsW],
          y <- [(-hsH), (-hsH) + size .. hsH - size]
      ]
    (hsW, hsH) = (fromIntegral width / 2, fromIntegral height / 2)
    (cX, cY) = (0, 0)

mapGridToPoint :: Int -> Int -> Float -> (Int, Int) -> Gloss.Point
mapGridToPoint width height size (col, row) =
  ( (-halfWidth) + halfSize + fromIntegral col * size,
    (-halfHeight) + halfSize + fromIntegral row * size
  )
  where
    halfWidth = fromIntegral width / 2
    halfHeight = fromIntegral height / 2
    halfSize = size / 2