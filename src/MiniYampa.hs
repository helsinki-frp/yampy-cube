module MiniYampa
       ( module Control.Arrow
       , module FRP.Yampa.VectorSpace
       , Event(..), isEvent, tag, tagWith, attach, lMerge, filterE
       , SF, reactimate
       , constant, lift2
       , switch, kSwitch
       , integral, imIntegral
       , edge, edgeJust
       , accumHoldBy
       , noiseR
       ) where

import Control.Arrow
import Control.Category
import Control.Monad
import FRP.Yampa.VectorSpace -- No need to reimplement this...
import Prelude hiding (id, (.))
import System.Random

data Event a = Event a | NoEvent

instance Functor Event where
    fmap f NoEvent = NoEvent
    fmap f (Event x) = Event (f x)

newtype SF a b = SF (Double -> a -> (SF a b, b))

instance Category SF where
    id = idSF
      where
        idSF = SF (\dt x -> (idSF, x))
    SF sf2 . SF sf1 = SF sf12
      where
        sf12 dt x1 = (sf2' . sf1', x3)
          where
            (sf1', x2) = sf1 dt x1
            (sf2', x3) = sf2 dt x2

instance Arrow SF where
    arr f = arrSF
      where
        arrSF = SF (\dt x -> (arrSF, f x))
    first (SF sf) = SF firstF
      where
        firstF dt (x, y) = (first sf', (x', y))
          where
            (sf', x') = sf dt x

instance ArrowLoop SF where
    loop (SF sf) = SF loopF
      where
        loopF dt x = (loop sf', x')
          where
            (sf', (x', y)) = sf dt (x, y)


reactimate init sense actuate (SF sf0) = do
    a0 <- init
    let (sf, b0) = sf0 0 a0
    loop sf a0 b0
  where
    loop (SF sf) a b = do
        done <- actuate True b
        unless (a `seq` b `seq` done) $ do
            (dt, ma') <- sense False
            let a' = maybe a id ma'
                (sf', b') = sf dt a'
            loop sf' a' b'

constant x = arr (const x)

lift2 f sf1 sf2 = (sf1 &&& sf2) >>> arr (uncurry f)

integral :: VectorSpace a s => SF a a
integral = SF (integralF zeroVector)
  where
    integralF acc dt x = (SF (integralF res), acc)
      where
        res = acc ^+^ realToFrac dt *^ x

imIntegral :: VectorSpace a s => a -> SF a a
imIntegral init = SF (integralF init)
  where
    integralF acc dt x = (SF (integralF res), res)
      where
        res = acc ^+^ realToFrac dt *^ x

switch (SF sf) k = SF switchF
  where
    switchF dt x = case evt of
        NoEvent -> (switch sf' k, y)
        Event e -> sfNew dt x
          where
            SF sfNew = k e
      where
        (sf', (y, evt)) = sf dt x

kSwitch (SF sf0) raiseSF@(SF raiseF) k = SF (switchF sf0)
  where
    switchF sf dt x = case evt of
        NoEvent -> (kSwitch sf' raiseSF k, y)
        Event e -> sfNew dt x
          where
            SF sfNew = k (SF sf) e
      where
        (sf', y) = sf dt x
        (raiseSF', evt) = raiseF dt (x, y)

isEvent NoEvent = False
isEvent (Event _) = True

tag = flip tagWith

tagWith :: a -> Event x -> Event a
tagWith x = fmap (const x)

attach :: Event a -> b -> Event (a, b)
e `attach` b = fmap (\a -> (a, b)) e

edge = SF (edgeSF True)
  where
    edgeSF prev dt cur = (SF (edgeSF cur), if not prev && cur then Event () else NoEvent)

edgeJust = SF (edgeJustSF True)
  where
    edgeJustSF _prev dt Nothing = (SF (edgeJustSF False), NoEvent)
    edgeJustSF prev dt (Just x) = (SF (edgeJustSF True), if prev then NoEvent else Event x)

accumHoldBy op init = SF (accumHoldBySF init)
  where
    accumHoldBySF s dt NoEvent = (SF (accumHoldBySF s), s)
    accumHoldBySF s dt (Event x) = (SF (accumHoldBySF sNext), sNext)
      where
        sNext = op s x

noiseR range g0 = SF (randomSF g0)
  where
    randomSF g dt _ = (SF (randomSF g'), x)
      where
        (x, g') = randomR range g

lMerge :: Event a -> Event a -> Event a
lMerge e@(Event _) _ = e
lMerge NoEvent e = e

filterE :: (a -> Bool) -> Event a -> Event a
filterE p e@(Event a) = if p a then e else NoEvent
filterE _ NoEvent     = NoEvent
