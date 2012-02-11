#!/usr/bin/env runhaskell
import Graphics.Gloss.OSM as OSM
import Graphics.Gloss.Interface.IO.Game
-- import Graphics.Gloss.Interface.Animate
import Data.GPS
import Control.Concurrent (threadDelay)

data World a = W (Frame a)

main = do
  svc <- OSM.startService
  let wh   = (1680,1050)
      zoom = 16
      downtownSomewhere = pt 45.52 (-122.685) Nothing Nothing
      center = Color red (circleSolid 10)
      zeroWorld = W (Frame (fst wh) (snd wh) downtownSomewhere zoom)
  playIO (FullScreen wh) white 30 zeroWorld (renderWorld svc) serveWorld stepWorld

renderWorld :: OSMService a () -> World a -> IO Picture
renderWorld svc (W f) = serveBackground svc f

stepWorld :: Float -> World a -> IO (World a)
stepWorld _ w = return w

serveWorld :: Coordinate a => Event -> World a -> IO (World a)
serveWorld (EventKey (MouseButton WheelUp) _ _ _) (W (Frame w h c z)) =
  return (W (Frame w h c (z + 1)))
serveWorld (EventKey (MouseButton WheelDown) _ _ _) (W (Frame w h c z)) =
  return (W (Frame w h c (z - 1)))
serveWorld (EventKey (SpecialKey k) _ _ _) world@(W (Frame w h c z)) =
  let hd = case k of
                KeyLeft -> Just west
                KeyRight -> Just east
                KeyUp -> Just north
                KeyDown -> Just south
                _       -> Nothing
      c' = fmap (\h -> addVector (40,h) c) hd
  in case c' of
      Nothing -> return world
      Just x  -> return (W (Frame w h x z))
serveWorld _ w = return w
