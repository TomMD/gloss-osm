#!/usr/bin/env runhaskell
import qualified Graphics.Gloss.OSM as OSM
import Graphics.Gloss.Interface.IO.Simulate
-- import Graphics.Gloss.Interface.Animate
import Geo.Computations
import Control.Concurrent (threadDelay)

main = do
  svc <- OSM.startService
  let wh   = (1680,1050)
      zoom = 16
      pnt  = downtownElsewhere
      downtownSomewhere = pt 45.523 (-122.69) Nothing Nothing
      downtownElsewhere = pt 45.522 (-122.688) Nothing Nothing
      center = Color red (circleSolid 10)
      serve _ = do
         p <- OSM.serveBackground svc (OSM.Frame (fst wh) (snd wh) pnt zoom)
         return (Pictures [p,center])
  simulateIO (FullScreen wh) white 30 () serve (\ _ _ _ -> return ())
