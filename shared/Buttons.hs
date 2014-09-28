module Buttons where

import Data.Monoid
import Graphics.Gloss
import Graphics.Gloss.Data.Extent


buttonC0, buttonT0, buttonC5, buttonT5, buttonC10, buttonT10 :: Extent
buttonC0  = makeExtent   35     5  (-60) (-140)
buttonT0  = makeExtent  (-5) (-35) (-60) (-140)
buttonC5  = makeExtent   35     5    40   (-40)
buttonT5  = makeExtent  (-5) (-35)   40   (-40)
buttonC10 = makeExtent   35     5   140    (60)
buttonT10 = makeExtent  (-5) (-35)  140    (60)

renderButtons :: (Int, Int, Int) -> Picture
renderButtons (count0, count5, count10)
    = renderButton buttonC0 (show count0)
   <> renderButton buttonT0 "Toggle"
   <> renderButton buttonC5 (show count5)
   <> renderButton buttonT5 "Toggle"
   <> renderButton buttonC10 (show count10)
   <> renderButton buttonT10 "Toggle"


renderButton :: Extent -> String -> Picture
renderButton ex s = color azure bg <> color white fg
  where
    bg = polygon (cornerPoints ex)
    fg = translate x y
       $ uscale 0.1
       $ translate (-180) (-50)  -- vertically centered, random x offset :(
       $ text s
    (x, y) = coord2point (centerCoordOfExtent ex)


uscale :: Float -> Picture -> Picture
uscale v = scale v v


coord2point :: Coord -> Point
coord2point (x,y) = (fromIntegral x, fromIntegral y)

cornerCoords :: Extent -> [Coord]
cornerCoords ex = [(w,n), (e,n), (e,s), (w,s)]
  where
    (n, s, e, w) = takeExtent ex

cornerPoints :: Extent -> [Point]
cornerPoints = map coord2point . cornerCoords
