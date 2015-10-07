module Signal.Fun
  ( scan
  , premap
  , postmap
  , bimap
  ) where
{-| Some utility functions for signals of functions. The question is:
Why are you dealing with signals of functions? Are you sure you want
this? It can be used to harm testability and separation of concerns,
which are endangered species and it's illegal to harm them! So please be
careful.

@docs scan, premap, postmap, bimap
-}

import Signal exposing (Signal)

{-| Just a shorthand for signals of functions
-}
type alias SF a b = Signal (a -> b)

{-| Takes a starting value for a state; applies each new function coming
in to calculate the new state. 
-}
scan : a -> SF a a -> Signal a
scan = Signal.foldp (<|)

{-| Compose the given function before every function that comes through
the signal.
-}
premap : (a -> b) -> SF b c -> SF a c
premap f1 = Signal.map (\fs -> f1 >> fs)

{-| Compose the given function after every function that comes through
the signal.
-}
postmap : (c -> d) -> SF b c -> SF b d
postmap f2 = Signal.map (\fs -> fs >> f2)

{-| Compose the given functions before and after every function that
comes through the signal respectively.
-}
bimap : (a -> b) -> (c -> d) -> SF b c -> SF a d
bimap f1 f2 = Signal.map (\fs -> f1 >> fs >> f2)
