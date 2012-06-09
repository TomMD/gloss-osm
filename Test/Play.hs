#!/usr/bin/env runhaskell
import Graphics.Gloss.OSM as OSM
import Graphics.Gloss.Interface.IO.Game
import Geo.Computations as Geo
import Control.Concurrent (threadDelay)
import System.Exit

data World a = W (Frame a)

main = do
  -- Start the OSM tile download and cache service
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

serveWorld :: Event -> World Geo.Point -> IO (World Geo.Point)
serveWorld (EventKey (MouseButton WheelUp) _ _ _) (W (Frame w h c z)) =
  return (W (Frame w h c (z + 1)))
serveWorld (EventKey (MouseButton WheelDown) _ _ _) (W (Frame w h c z)) =
  return (W (Frame w h c (z - 1)))
serveWorld (EventKey (Char 'q') _ _ _) _ = exitWith ExitSuccess
serveWorld (EventKey (SpecialKey k) _ _ _) world@(W (Frame w h c z)) =
  let hd = case k of
                KeyLeft -> Just west
                KeyRight -> Just east
                KeyUp -> Just north
                KeyDown -> Just south
                _       -> Nothing
      c' = fmap (\h -> addVector (40,h) c) hd -- Each step, adjust the GPS center as needed. (Use Data.GPS operations)
  in case c' of
      Nothing -> return world
      Just x  -> return (W (Frame w h x z)) -- return the new "frame of view"   That's it!
serveWorld _ w = return w
