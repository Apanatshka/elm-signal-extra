module Signal.Discrete(EventSource, es, whenEquals, whenChange, whenChangeTo, folde, switchWhen, switchSample, keepThen) where
{-| Helper functions for recognising events. Mostly useful in
combination with `Signal.sampleOn`, although there are uses. 

# EventSource
An `EventSource` is really just a `Signal` where we don't care about its
value, but only when it updates. A prime example is `Mouse.clicks`. 
@docs EventSource, es

# Basics
@docs whenEquals, whenChange, whenChangeTo

# `foldp` variations
@docs folde

# Filter extensions
-}

import Signal
import Signal ((<~), (~), Signal)
import Signal.Extra

{-| At some point in the future Elm will probably support something like
this:

    type alias EventSource = Signal _

That is, `EventSource` will become an alias for any `Signal a` where we
hide the `a` part.  
Until then, there is the `es` function to create an EventSource
-}
type alias EventSource = Signal ()

{-| Simple way to make an event signal from any signal
-}
es : Signal a -> EventSource
es = Signal.map (always ())

{-| Fires when the value of the input signal is equal to the given
value.

**NB:** Repeated updates to the same value will make the `EventSource`
fire repeatedly.  
See also: [`whenChangeTo`](#whenChangeTo). 

    Mouse.click == whenEqual True Mouse.isDown
-}
whenEquals : a -> Signal a -> EventSource
whenEquals value input =
  let matchEvent = Signal.keepIf ((==) value) value input
  in es matchEvent

{-| Fires when the value of the input signal changes. 
-}
whenChange : Signal a -> EventSource
whenChange input = es <| Signal.dropRepeats input

{-| Fires when the value of the input signal changes to the given value.

    spacebarPress = whenChangeTo True Keyboard.spacebar
-}
whenChangeTo: a -> Signal a -> EventSource
whenChangeTo value input = whenEquals value <| Signal.dropRepeats input

{-| `foldp` on an `EventSource`.

    toggleOnEnter = folde not False <| whenChangeTo True Keyboard.enter
-}
folde : (b -> b) -> b -> EventSource -> Signal b
folde step base evt = Signal.foldp (\_ b -> step b) base evt

{-| Switch between two signals. When the first signal is `True`, use the
 second signal, otherwise use the third. 
-}
switchWhen : Signal Bool -> Signal a -> Signal a -> Signal a
switchWhen b l r = switchHelper Signal.keepWhen b l r

{-| Same as the previous, but samples the signal it switches to. -}
switchSample : Signal Bool -> Signal a -> Signal a -> Signal a
switchSample b l r = switchHelper Signal.Extra.sampleWhen b l r

switchHelper : (Signal Bool -> Maybe a -> Signal (Maybe a) -> Signal (Maybe a))
             -> Signal Bool -> Signal a -> Signal a -> Signal a
switchHelper filter b l r =
  let bi = Signal.sampleOn (Signal.constant ()) b
      li = Signal.sampleOn (Signal.constant ()) l
      ri = Signal.sampleOn (Signal.constant ()) r
      base = (\bi' li' ri' -> Just <| if bi' then li' else ri') <~ bi ~ li ~ ri
      lAndR =
        Signal.merge
          (filter b Nothing (Just <~ l))
          (filter (not <~ b) Nothing (Just <~ r))
      fromJust (Just a) = a
  in fromJust <~ Signal.merge base lAndR

{-| Like `keepWhen`, but when the filter signal turn `False`, the output
changes back to the base value. 
-}
keepThen : Signal Bool -> a -> Signal a -> Signal a
keepThen choice base signal = 
  switchWhen choice signal
    <| always base <~ whenChangeTo False choice
