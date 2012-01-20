-- |The OSM module for Gloss will download and cache map tiles
-- and provides easy display of maps using Gloss 'Picture's.
--
-- For example:
--
-- @
--  p <- buildOSMBackground (1280,1024) black 16 (pt 45 (-122.5) Nothing Nothing)
--  display (FullScreen (1280,1024)) white p
-- @
--
-- Will build a map of the given lat/lon center.  (Note: 'pt' i
module Graphics.Gloss.OSM
       ( -- * The main function of use builds a picture using OSM tiles
         buildOSMBackground
         -- * Utility functions
       , gridToPicture
       , repaToPicture
       ) where

import Codec.Picture.Repa
import Network.OSM
import Data.GPS
import Graphics.Gloss
import Data.Array.Repa ((:.)(..), Z, Z(..), extent, backpermute, DIM1, DIM0, DIM3, Array)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.ByteString as RB
import Data.Word
import Network.HTTP.Types (status501)
import Data.List (genericLength)

-- |@buildOSMBackground (w,h) c z center@ will build a 'Gloss' picture
-- of at least width @w@ and height @h@, centered around @center@ and fitting
-- with zoom level @z@.  Any unavailable tiles will be given the default
-- solid color of @c@.
buildOSMBackground :: (Coordinate a) => (Int,Int) -> Color -> Zoom -> a -> IO Picture
buildOSMBackground (w,h) color zoom center = do
  let tileIDs = selectTilesWithFixedDimensions (w,h) center zoom
      defaultTile = Color black $ Polygon [(0,256),(256,256),(256,0),(0,0)]
  cfgD <- defaultOSMConfig
  let cfg = cfgD { noCacheAction = Just $ \_ _ -> return (Left status501) }
  ts <- evalOSM (getTiles tileIDs zoom) cfg
  let decodeImageE (Right i) = decodeImageRGBA i
      decodeImageE (Left _)  = Left ""
      grid :: [[(Int,Int,Picture)]]
      grid = map (map $ imgToPic . decodeImageE) ts
      imgToPic :: Either String (Img RGBA) -> (Int, Int, Picture)
      imgToPic = either (const (256,256,defaultTile)) id . fmap (repaToPicture True . imgData)
      (x,y,grid') = replaceIncorrectTiles defaultTile grid
      pic = gridToPicture 256 256 grid'
--  let pic = gridToPicture 256 256 [[Text ((show x) ++ "-" ++ (show y)) | x <- [0..10]] | y <- [0..10] ] 
  return pic

replaceIncorrectTiles :: Picture -> [[(Int, Int, Picture)]] -> (Int, Int, [[Picture]])
replaceIncorrectTiles _ [] = (0,0,[])
replaceIncorrectTiles p ([]:xs) = replaceIncorrectTiles p xs
replaceIncorrectTiles p ps@((a:as):xs) =
  let (w,h,_) = a
      rep (w',h',p') = if w' /= w || h' /= h then p else p'
  in (w,h,map (map rep) ps)

-- |Convert a repa RGBA array into a 'gloss' 'Picture' assumes the
-- array shape is @(Z :. columns :. rows :. color channel)@ where the
-- color channel is RGBA (vs ABGR, which many OpenGL formats use).
repaToPicture :: Bool -> Array DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = (col, row, bitmapOfByteString row col (RB.toByteString arr) b)
 where
   (Z :. row :. col :. _) = extent arr

-- | Given a list of list of pictures in a grid format, place them all
-- in a grid taking the inner list as rows and the elements to
-- represent the columns of their given rows (starting from the upper
-- left of the picture).  For example, the list @[ [0, 1], [2, 3] ]@
-- would display as:
--
-- @
--    0  1
--    2  3
-- @
gridToPicture :: Int -> Int -> [[Picture]] -> Picture
gridToPicture x y arrs =
  let rows = map (\(r,a) -> Translate 0 (r*yF) (adjustColumns a)) (zip [(genericLength arrs / (-2))..] arrs)
      yF = (-1) * fromIntegral y
      xF = fromIntegral x
      adjustColumns :: [Picture] -> Picture
      adjustColumns = Pictures
                    . map (\(c,a) -> Translate (c*xF) 0 a)
                    . (\x -> zip [genericLength x / (-2)..] x)
  in Pictures rows
