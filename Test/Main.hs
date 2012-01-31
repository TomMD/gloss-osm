#!/usr/bin/env runhaskell
import qualified Graphics.Gloss.OSM as OSM
import Graphics.Gloss.Interface.IO.Simulate
-- import Graphics.Gloss.Interface.Animate
import Data.GPS
import Control.Concurrent (threadDelay)

main = do
  svc <- OSM.startService
  let wh   = (1680,1050)
      zoom = 16
      pnt  = (pt 45.5 (-122.5787) Nothing Nothing)
      center = Color red (circleSolid 10)
      serve _ = do
         p <- OSM.serveBackground svc wh zoom pnt
         return (Pictures [p,center])
  simulateIO (FullScreen wh) white 30 () serve (\ _ _ _ -> return ())
