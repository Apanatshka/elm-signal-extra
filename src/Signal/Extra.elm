module Signal.Extra where
{-| Utility functions that aren't in the `Signal` module from
`elm-lang/core`. 

# Flipped fancy map
@docs (~>)

# Zipping and unzipping
For those too lazy to write a record or ADT.  
@docs zip, zip3, zip4, unzip, unzip3, unzip4

# Stateful
@docs foldp', foldps, foldps', last, delayRound

# Old filters
The way they used to work, with weird value propagation. 
@docs keepWhenS, dropWhenS
-}
--    Mouse.clicks == 
--      let runningBuffer = runBuffer 2 Mouse.isDown
--          onValue v s = keepIf ((==) True) (s ~> ((==) v))
--      in always () <~ runningBuffer `onValue` [True, False]

import Signal (..)
import List

{-| The `(<~)` operator, but flipped. Doesn't play well with the other
two!

    Mouse.x ~> toFloat >> sqrt >> round
            >> isEven >> not
            >> asText
-}
(~>) : Signal a -> (a -> b) -> Signal b
(~>) = flip map
infixl 4 ~>

{-| zip two signals

    zip Mouse.x Mouse.y == Mouse.position
-}
zip : Signal a -> Signal b -> Signal (a,b)
zip = map2 (,)

{-| zip three signals -}
zip3 : Signal a -> Signal b -> Signal c -> Signal (a,b,c)
zip3 = map3 (,,)

{-| zip four signals -}
zip4 : Signal a -> Signal b -> Signal c -> Signal d -> Signal (a,b,c,d)
zip4 = map4 (,,,)

{-| unzip a zipped pair of signals

    unzip Mouse.position == (Mouse.x, Mouse.y)
-}
unzip : Signal (a,b) -> (Signal a, Signal b)
unzip pairS = (fst <~ pairS, snd <~ pairS)

{-| unzip three signals -}
unzip3 : Signal (a,b,c) -> (Signal a, Signal b, Signal c)
unzip3 pairS = ((\(a,_,_) -> a) <~ pairS, (\(_,b,_) -> b) <~ pairS, (\(_,_,c) -> c) <~ pairS)

{-| unzip four signals -}
unzip4 : Signal (a,b,c,d) -> (Signal a, Signal b, Signal c, Signal d)
unzip4 pairS = ((\(a,_,_,_) -> a) <~ pairS, (\(_,b,_,_) -> b) <~ pairS, (\(_,_,c,_) -> c) <~ pairS, (\(_,_,_,d) -> d) <~ pairS)

{- | `foldp'` is slighty more general than `foldp` in that you can base
the initial value of the state on the initial value of the input value. 

    foldp f b s == foldp' f (always b) s
-}
foldp' : (a -> b -> b) -> (a -> b) -> Signal a -> Signal b
foldp' fun initFun input =
  let -- initial has no events, only the initial value is used
      initial = initFun <~ sampleOn (constant ()) input
      -- both the initial value and the normal input are given to fun'
      rest = foldp fun' Nothing ((,) <~ input ~ initial)
      -- when mb is Nothing, input had its first event to use ini
      -- otherwise use the b from Just
      fun' (inp, ini) mb = case mb of
        Nothing -> Just <| fun inp ini
        Just b  -> Just <| fun inp b
      fromJust (Just a) = a
  in  fromJust <~ merge (Just <~ initial) rest

{-| Like `foldp`, but with a hidden state

    foldp f b i ==
      let d a = (a,a) -- doubling function
      in foldps (\a b -> f a b |> d) (d b) i
-}
foldps : (a -> s -> (b,s)) -> (b,s) -> Signal a -> Signal b
foldps f bs aS = fst <~ foldp (\a (_,s) -> f a s) bs aS

{-| Like `foldp'`, but with a hidden state
-}
foldps' : (a -> s -> (b,s)) -> (a -> (b,s)) -> Signal a -> Signal b
foldps' f iF aS = fst <~ foldp' (\a (_,s) -> f a s) iF aS

{-| A running buffer of the given size (`n`) of the given signal. 
The list of at most `n` of the last values on the input signal. Starts
with an empty list. Adds new values to the *end* of the list! So you get
a list with time going from left to right. 

    ((==) [1,2,3,4,5]) <~ runBuffer 5 (count (Time.every second))
-}
runBuffer : Int -> Signal a -> Signal (List a)
runBuffer = runBuffer' []

{-| Same as `runBuffer` but with an initial buffer. 
-}
runBuffer' : List a -> Int -> Signal a -> Signal (List a)
runBuffer' l n input =
  let f inp prev =
    let l = List.length prev
    in if l < n
        then prev ++ [inp]
        else List.drop (l-n+1) prev ++ [inp]
  in foldp f l input

{-| A combination of `Signal.sampleOn` and `Signal.keepWhen`. When the
first signal becomes `True`, the most recent value of the second signal
will be propagated.  
[Before Elm 0.12](
https://github.com/elm-lang/elm-compiler/blob/master/changelog.md#012)
 this was the standard behaviour of `keepWhen`.
-}
sampleWhen : Signal Bool -> a -> Signal a -> Signal a
sampleWhen bs def sig =
  let filtered = keepWhen bs def sig
      sampled  = sampleOn bsTurnsTrue sig
      bsTurnsTrue = keepIf ((==) True) False (dropRepeats bs)
  in merge filtered sampled

{-| Instead of delaying for some amount of time, delay for one round,
where a round is initiated by outside event to the Elm program. 
Also known to `delay` in E-FRP. 
-}
delayRound : b -> Signal b -> Signal b
delayRound b bS = foldps (\new old -> (old, new)) (b,b) bS
