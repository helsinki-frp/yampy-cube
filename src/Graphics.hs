module Graphics
    ( animate
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Colour.SRGB (toSRGB24, RGB(..))
import           Data.Text (Text)
import qualified Data.Vector.Storable as Vector

import           FRP.Yampa

import           Linear (V2(..), V4(..))
import           Linear.Affine (Point(..))

import qualified SDL

import Shapes
import Types

-- | (Object to render, should the app exit)
--   TODO: Datatype
type WinOutput = (Object, Bool)

animate :: Text                -- ^ window title
        -> Int                 -- ^ window width in pixels
        -> Int                 -- ^ window height in pixels
        -> SF WinInput WinOutput -- ^ signal function to animate
        -> IO ()
animate title winWidth winHeight sf = do
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow title windowConf
    SDL.showWindow window

    renderer <- SDL.createRenderer window (-1) rendererConf
    SDL.setRenderDrawColor renderer (V4 maxBound maxBound maxBound maxBound)

    lastInteraction <- newMVar =<< SDL.time

    let senseInput _canBlock = do
            currentTime <- SDL.time
            dt <- (currentTime -) <$> swapMVar lastInteraction currentTime
            mEvent <- SDL.pollEvent
            return (dt, Event . SDL.eventPayload <$> mEvent)

        renderOutput changed (obj, shouldExit) = do
            when changed $ do
                renderObject renderer winHeight obj
                SDL.renderPresent renderer
            return shouldExit

    reactimate (return NoEvent) senseInput renderOutput sf

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit

    where windowConf =
              SDL.defaultWindow { SDL.windowSize = V2 (fromIntegral winWidth)
                                                      (fromIntegral winHeight) }
          rendererConf = SDL.RendererConfig
              { SDL.rendererAccelerated   = True
              , SDL.rendererSoftware      = False
              , SDL.rendererTargetTexture = False
              , SDL.rendererPresentVSync  = False
              }

renderObject :: SDL.Renderer -> Int -> Object -> IO ()
renderObject renderer winHeight obj = setRenderAttrs >> renderShape
    where setRenderAttrs = do
              let (RGB r g b) = toSRGB24 $ objColour obj
              SDL.setRenderDrawColor renderer (V4 r g b maxBound)
          renderShape = case objShape obj of
              Rectangle x y -> SDL.renderFillRect renderer $ Just $
                                     SDL.Rectangle (P (V2 (toEnum $ floor px)
                                                          (toEnum $ winHeight - floor py)))
                                                   (V2 (toEnum x) (toEnum y))
              Scene objs -> do
                  SDL.renderClear renderer
                  mapM_ (renderObject renderer winHeight) objs
              Circle r -> SDL.renderDrawPoints renderer $ Vector.fromList $
                                map (\(x,y) -> P (V2 (toEnum x) (toEnum y))) $
                                translate (floor px, winHeight - floor py) $
                                rasterCircle  r
          (px, py) = objPos obj

-- | Get octant points for a circle of given radius.
octant :: (Num a, Ord a) => a -> [(a, a)]
octant r = takeWhile inOctant . map fst $ iterate step ((r, 0), 1 - r)
    where -- check if we are still in octant
          inOctant (x, y) = x >= y

          -- go to the next point in the circle
          step ((x, y), e)
              | e < 0     = ((x,     y + 1), e + 2 * (y + 1) + 1)
              | otherwise = ((x - 1, y + 1), e + 2 * (y - x + 2) + 1)

-- | Get quadrant points for a circle of given radius.
-- To do that we just mirror octant with respect to x = y line.
quadrant :: (Num a, Ord a) => a -> [(a, a)]
quadrant r = octant r >>= mirror
    where mirror (x, y) = [ (x, y), (y, x) ]

-- | Get points of a circle of given radius.
-- To do that we just mirror quadrant with respect to x = 0 and y = 0 lines.
rasterCircle :: (Num a, Ord a) => a -> [(a, a)]
rasterCircle r = quadrant r >>= mirror
    where mirror (x, y) = [ (u, v) | u <- [x, -x], v <- [y, -y] ]

-- | Move all points by a given vector.
translate :: (Num a, Eq a) => (a, a) -> [(a, a)] -> [(a, a)]
translate v = map (v .+)

-- | Vector addition generalized for Num
(.+) :: Num a => (a, a) -> (a, a) -> (a, a)
(x, y) .+ (u, v) = (x + u, y + v)
