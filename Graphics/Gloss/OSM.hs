-- |The OSM module for Gloss will download and cache map tiles
-- and provides easy display of maps using Gloss 'Picture's.
--
-- For example:
--
-- @
--  p <- buildOSMBackground black (Frame 1280 1024 (pt 45 (-122.5) Nothing Nothing) 16)
--  display (FullScreen (1280,1024)) white p
-- @
--
-- Will build a map of the given lat/lon center.  (Note: 'pt' is from the 'GPX' package)
module Graphics.Gloss.OSM
       ( -- * Types
         Frame(..)
       , OSMService
       , Zoom
         -- * Recommended Service-Based Interface
       , startService
       , serveBackground
       , flushCache
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
import Geo.Computations as Geo
import Data.List (genericLength)
import Data.Word
import Graphics.Gloss
import Network.HTTP.Types (status501)
import Network.OSM
import Data.Array.Repa ((:.)(..), Z, Z(..), extent, DIM3, Array)
import qualified Data.Array.Repa as R
import qualified Data.Array.Repa.Repr.ForeignPtr as F
import Data.IORef
import Data.Cache.LRU (LRU)
import qualified Data.Cache.LRU as LRU
import Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BI -- (fromForeignPtr)

type RenderCache = LRU (TileID,Zoom) Picture
data OSMCmd a transactionId = FlushCache | GetFrame Frame transactionId

type OSMService a tId = (TBChan (OSMCmd a tId)
                        ,TBChan (Picture, tId)
                        ,IORef Picture)

-- |A cache of 384 RGBA decoded tiles should fit in ~128MB of RAM
lruSize :: Integer
lruSize = 384

-- |Run a service providing OpenStreetMap images, allowing the actual
-- images to be acquired via 'serveBackground'.
startService :: IO (OSMService Geo.Point t)
startService = do
  -- The bound is low because we shouldn't be even one request behind
  -- in this task.  Almost worth making it an MVar Either.
  let lru = LRU.newLRU (Just lruSize)
  req  <- newTBChanIO 4
  resp <- newTBChanIO 4
  frameRef <- newIORef (Text "Loading")
  cfgD <- defaultOSMConfig
  let cfg = cfgD { noCacheAction = Just $ \_ _ -> return (Left status501) }
  forkIO (evalOSM (serve lru req resp) cfg)
  return (req,resp,frameRef)
  where
    -- serve :: TBChan a -> OSM ()
    serve lru req resp = do
      cmd <- liftIO (atomically (readTBChan req))
      case cmd of
        FlushCache -> serve (LRU.newLRU (Just lruSize)) req resp
        GetFrame frm t -> do
          (pic,lru') <- buildBackgroundLRU lru black frm
          liftIO (atomically (writeTBChan resp (pic,t)))
          serve lru' req resp

-- |A blocking operation that will flush the LRU cache.  This will NOT
-- flush the local, sqlite, persistent on-disk cache, but only the
-- in-memory decoding of that on-disk cache.  This is useful if the
-- persistent data has changed and that change is not being reflected
-- by the served pictures.  No mechanism currently exists to flush the
-- on-disk cache (part of 'osm-download').
flushCache :: OSMService a () -> IO ()
flushCache (req,_,_) = atomically $ writeTBChan req FlushCache

-- |Non-blocking.  Issues a request for a background, returning the
-- first frame available from 'osm-download'.  If no frame is
-- available then it uses the previously returned frame (which could be
-- an initial "Loading" text).
serveBackground :: OSMService a () -> Frame -> IO Picture
serveBackground (req,resp,prevRef) frm = do
  mp <- atomically $ do
    tryWriteTBChan req (GetFrame frm ())
    tryReadTBChan resp
  case mp of 
    Just (p,_) -> writeIORef prevRef p >> return p
    Nothing    -> readIORef prevRef

buildBackgroundLRU ::
     RenderCache ->
     -- A cache of map tiles
     Color ->
     -- default background color
     Frame ->
     -- Screen Center
     OSM (Picture,RenderCache)
buildBackgroundLRU lru color (Frame w h center zoom) = do
  let frame = Frame w h center zoom
      tileIDs = selectTilesForFrame frame
      (cx,cy) = point2pixel frame center
      (dx,dy) = ( fromIntegral (-cx)
                , fromIntegral cy) 
  (lru',grid) <- getTilesWithLRU lru tileIDs
  return (Translate dx dy (gridToPicture 256 256 grid), lru')
 where
  defaultTile :: Picture
  defaultTile = Color color (Polygon [(0,256),(256,256),(256,0),(0,0)])
  getTilesWithLRU :: RenderCache -> [[TileID]] -> OSM (RenderCache, [[Picture]])
  getTilesWithLRU lru xs = foldMap (foldMap getTileWithLRU) lru xs
  getTileWithLRU :: RenderCache -> TileID -> OSM (RenderCache,Picture)
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
              lruFinal = if pic == defaultTile then lru' else LRU.insert (t,zoom) pic lru'
          return (lruFinal,pic)

foldMap :: Monad m => (a -> t -> m (a,p)) -> a -> [t] -> m (a, [p])
foldMap op a ts = go a ts
 where
  go a []     = return (a,[])
  go a (t:ts) = op a t >>= \(a',p) -> go a' ts >>= \(aF,pF) -> return (aF,p:pF)

decodeImageE :: Either a ByteString -> Either String (Img RGBA)
decodeImageE (Right i) = either Left (Right . onImg flipVertically)
                                   (decodeImageRGBA i)
decodeImageE (Left _)  = Left ""

buildBackground ::
     (Int, Int) ->                    -- The window width and height
     Color ->                         -- default background color
     Zoom ->
     Geo.Point ->                             -- Screen Center
     OSM Picture
buildBackground wh color zoom center = do
  let tileIDs = selectTilesForFrame (Frame (fst wh) (snd wh) center zoom)
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
buildOSMBackground :: (Int,Int) -> Color -> Zoom -> Geo.Point -> IO Picture
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
repaToPicture :: Bool -> Array F.F DIM3 Word8 -> (Int, Int, Picture)
repaToPicture b arr = 
	let fptr = F.toForeignPtr arr
	    bs   = BI.fromForeignPtr fptr 0 len
	in (col, row, bitmapOfByteString row col bs b)
 where
  len = row * col * depth
  (Z :. row :. col :. depth) = extent arr

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
  let rows = map (\(r,a) -> Translate 0 (r*yF) (adjustColumns a)) (zip [1..] arrs)
      yF = (-1) * fromIntegral y
      xF = fromIntegral x
      adjustColumns :: [Picture] -> Picture
      adjustColumns = Pictures
                    . map (\(c,a) -> Translate (c*xF) 0 a)
                    . (\x -> zip [1..] x)
  in Pictures rows

