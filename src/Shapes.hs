module Shapes
    ( Frame
    , Shape(..)
    , Object(..)
    , def
    , scene_
    , circle_
    , rectangle_
    , pos_
    , colour_
    , Colour(..)
    , sRGB24
    , (&)
    ) where

import Data.Default
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names

type Frame = Object

data Shape = Circle Int
           | Rectangle Int Int
           | Scene [Object] -- ^ Not really a shape
           deriving (Show, Eq)

data Object = Object { objShape  :: Shape
                     , objPos    :: (Double, Double)
                     , objColour :: Colour Double
                     } deriving (Show, Eq)

instance Default Object where
    def = Object { objShape = error "Object shape wasn't define"
                 , objPos   = (0, 0)
                 , objColour = white
                 }

-- Terms are written with postfix '_' indicating data rather than code.
-- (stolen from lucid)

-- It might worth to use lenses here in order to avoid building a
-- poor version of them

scene_ :: [Object] -> Object
scene_ objs = def { objShape = Scene objs, objColour = black}

circle_ :: Double -> Object
circle_ n = def { objShape = Circle (round n) }

rectangle_ :: Double -> Double -> Object
rectangle_ x y = def { objShape = Rectangle (round x) (round y) }

type AttributeSetter = Object -> Object

pos_ :: (Double, Double) -> AttributeSetter
pos_ pos obj = obj { objPos = pos }

colour_ :: Colour Double -> AttributeSetter
colour_ colour obj = obj { objColour = colour }

(&) :: Object -> AttributeSetter -> Object
(&) = flip ($)
