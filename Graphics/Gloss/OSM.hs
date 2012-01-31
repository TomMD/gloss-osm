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
       ( -- * Recommended Service-Based Interface
         startService
       , serveBackground
         -- * Single-Request Interface
       , buildOSMBackground
         -- * Utility functions
       , gridToPicture
       , repaToPicture
       ) where

import Codec.Picture.Repa
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBChan
import Control.Monad
import Control.Concurrent.MonadIO
import Data.GPS
import Data.List (genericLength)
import Data.Word
import Graphics.Gloss
import Network.HTTP.Types (status501)
import Network.OSM
import Data.Array.Repa ((:.)(..), Z, Z(..), extent, backpermute, DIM1, DIM0, DIM3, Array)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.ByteString as RB
import Data.IORef
import Data.Cache.LRU (LRU)
import qualified Data.Cache.LRU as LRU
import Data.ByteString (ByteString)

type RenderCache = LRU (TileID,Zoom) Picture
type OSMService a transactionId = (TBChan ((Int, Int), Zoom, a, transactionId)
                                  ,TBChan (Picture, transactionId)
                                  ,IORef Picture)

startService :: Coordinate a => IO (OSMService a t)
startService = do
  -- The bound is low because we shouldn't be even one request behind
  -- in this task.  Almost worth making it an MVar Either.
  let lru = LRU.newLRU (Just 384) -- Max of 256MB in the LRU
  req  <- newTBChanIO 1
  resp <- newTBChanIO 1
  frameRef <- newIORef (Text "Loading")
  cfgD <- defaultOSMConfig
  let cfg = cfgD { noCacheAction = Just $ \_ _ -> return (Left status501) }
  forkIO (evalOSM (serve lru req resp) cfg)
  return (req,resp,frameRef)
  where
    -- serve :: TBChan a -> OSM ()
    serve lru req resp = do
      (wh,zoom,center,t) <- liftIO (atomically (readTBChan req))
      (pic,lru') <- buildBackgroundLRU lru wh black zoom center
      liftIO (atomically (writeTBChan resp (pic,t)))
      serve lru' req resp

serveBackground :: OSMService a () -> (Int, Int) -> Zoom -> a -> IO Picture
serveBackground (req,resp,prevRef) wh zoom center = do
  mp <- atomically $ do
    tryWriteTBChan req (wh,zoom,center,())
    tryReadTBChan resp
  case mp of 
    Just (p,_) -> writeIORef prevRef p >> return p
    Nothing    -> readIORef prevRef

-- FIXME make an in-memory LRU cache of decoded tiles that we use
-- instead of hitting the acid-state and decoding every time!
buildBackgroundLRU :: (Coordinate a) => 
     RenderCache ->
     -- A cache of map tiles
     (Int, Int) ->
     -- The window width and height
     Color ->
     -- default background color
     Zoom ->
     a ->
     -- Screen Center
     OSM IO (Picture,RenderCache)
buildBackgroundLRU lru wh color zoom center = do
  let tileIDs = selectTilesWithFixedDimensions wh center zoom
      defaultTile = Color color $ Polygon [(0,256), (256,256), (256,0), (0,0)]
  (lru',grid) <- getTilesWithLRU lru tileIDs
  return (gridToPicture 256 256 grid, lru')
 where
  defaultTile :: Picture
  defaultTile = Color color (Polygon [(0,256),(256,256),(256,0),(0,0)])
  getTilesWithLRU :: RenderCache -> [[TileID]] -> OSM IO (RenderCache, [[Picture]])
  getTilesWithLRU lru xs = foldMap (foldMap getTileWithLRU) lru xs
  getTileWithLRU :: RenderCache -> TileID -> OSM IO (RenderCache,Picture)
  getTileWithLRU lru t =
   case LRU.lookup (t,zoom) lru of
      (lru', Just p)  -> return (lru',p)
      (lru', Nothing) -> do
          esb <- getTile t zoom
          let pic :: Picture
              pic = either (const defaultTile) id 
                  . fmap ((\(_,_,p) -> p) . repaToPicture True . imgData)
                  . decodeImageE
                  $ esb
              lruFinal = LRU.insert (t,zoom) pic lru'
          return (lruFinal,pic)

foldMap :: Monad m => (a -> t -> m (a,p)) -> a -> [t] -> m (a, [p])
foldMap op a ts = go a ts
 where
  go a []     = return (a,[])
  go a (t:ts) = op a t >>= \(a',p) -> go a' ts >>= \(aF,pF) -> return (aF,p:pF)

decodeImageE :: Either a ByteString -> Either String (Img RGBA)
decodeImageE (Right i) = decodeImageRGBA i
decodeImageE (Left _)  = Left ""

buildBackground :: (Coordinate a) => 
     (Int, Int) ->                    -- The window width and height
     Color ->                         -- default background color
     Zoom ->
     a ->                             -- Screen Center
     OSM IO Picture
buildBackground wh color zoom center = do
  let tileIDs = selectTilesWithFixedDimensions wh center zoom
      defaultTile = Color color $ Polygon [(0,256), (256,256), (256,0), (0,0)]
  ts <- getTiles tileIDs zoom
  let grid :: [[(Int,Int,Picture)]]
      grid = map (map $ imgToPic . decodeImageE) ts
      imgToPic :: Either String (Img RGBA) -> (Int, Int, Picture)
      imgToPic = either (const (256,256,defaultTile)) id . fmap (repaToPicture True . imgData)
      (x,y,grid') = replaceIncorrectTiles defaultTile grid
      pic = gridToPicture 256 256 grid'
  return pic

-- |@buildOSMBackground (w,h) c z center@ will build a 'Gloss' picture
-- of at least width @w@ and height @h@, centered around @center@ and fitting
-- with zoom level @z@.  Any unavailable tiles will be given the default
-- solid color of @c@.
--
-- You should not call 'buildOSMBackground', which runs an OSM monad from 'osm-downloader'
-- with any regularity (ex: not on a per-frame basis).  Doing so can get you banned from
-- OSM tile server services. Consider the service interface, above.
buildOSMBackground :: (Coordinate a) => (Int,Int) -> Color -> Zoom -> a -> IO Picture
buildOSMBackground wh color zoom center = do
  cfgD <- defaultOSMConfig
  let cfg = cfgD { noCacheAction = Just $ \_ _ -> return (Left status501) }
  evalOSM (buildBackground wh color zoom center) cfg

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

