module Signal.Time where
{-| Time related functions for `Signal`s.

# Re-exports
Some functions from the `Time` module that fit in. 
@docs since, delay, timestamp

# Easy does it
Controlling too frequently changing signals. 
@docs limitRate, dropWithin, settledAfter
-}

import Signal (Signal, (<~), (~))
import Signal
import Time (Time)
import Time

{-| Drops non-initial updates within a time window of given duration.

After an update of the given signal, the given time is waited (a
"window"). All other updates within this window are dropped. The
original update that started the window is kept, without delays. 

    throttledMouseClicks = limitRate 60 Mouse.clicks

Also known to some areas as a `throttle` function. 
-}
limitRate : Time -> Signal a -> Signal a
limitRate dur sig = 
  let within newt oldt = if newt - oldt > second / dur
                           then newt
                           else oldt
      windowStart = fst <~ timestamp sig
                    |> foldp within 0
                    |> dropRepeats
  in  sampleOn windowStart sig

{-| Drops all but the first update of a flurry of updates (a stutter).
The stutter is defined as updates that happen with max. the given time
in between. 

The first update of the given signal is sent through. Then the given
delay is waited. If no other updates arrive during that time, then next
update will be sent through. Any update that arrives within the given
time of the last update is dropped. 

    noDoubleClicks = dropWithin (300 * milliseconds) Mouse.clicks

Also known to some areas as a "immediate" `debounce` function. 
-}
dropWithin : Time -> Signal a -> Signal a
dropWithin delay sig =
  let leading = since delay sig
                |> dropRepeats
                |> keepIf ((==) True) False
  in  sampleOn leading sig

{-| Gives the last update of a flurry of updates (a stutter) after has
settled* for the given time. The stutter is defined as updates that
happen within the given time. * Where settled the signal gets no further
updates for some time, it's **not** relating to the value changes of the
signal. 

After every update of the given signal, the given delay is waited. If no
other updates arrived during that time, the update is sent through. If a
new update arrives within the given time, the previous update is dropped
and the waiting is restarted. So `debounce`-ing a signal that keeps up
the flurry of updates all the time results in a signal that never
updates. 

    tooltip : Signal Bool
    tooltip = merge (always False <~ Mouse.position) 
                    (always True  <~ 
                      settledAfter (500 * milliseconds) Mouse.position)

Also known to some areas as a `debounce` function. 
-}
settledAfter : Time -> Signal a -> Signal a
settledAfter delay sig =
  let trailing = since delay sig
                |> dropRepeats
                |> keepIf ((==) False) True
  in  sampleOn trailing sig

{-| A re-export of [Time.since](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Time#since). 

Takes a time `t` and any signal. The resulting boolean signal is true
for time `t` after every event on the input signal. So ``(second `since`
Mouse.clicks)`` would result in a signal that is true for one second
after each mouse click and false otherwise.
-}
since : Time -> Signal a -> Signal bool
since = Time.since

{-| A re-export of [Time.since](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Time#delay). 

Delay a signal by a certain amount of time. So `(delay second
Mouse.clicks)` will update one second later than any mouse click.
-}
delay : Time -> Signal a -> Signal a
delay = Time.delay


{-| A re-export of [Time.timestamp](http://package.elm-lang.org/packages/elm-lang/core/1.0.0/Time#timestamp). 

Add a timestamp to any signal. Timestamps increase monotonically. When
you create `(timestamp Mouse.x)`, an initial timestamp is produced. The
timestamp updates whenever `Mouse.x` updates.

Timestamp updates are tied to individual events, so `(timestamp
Mouse.x)` and `(timestamp Mouse.y)` will always have the same timestamp
because they rely on the same underlying event (`Mouse.position`).
-}
timestamp : Signal a -> Signal (Time, a)
timestamp = Time.timestamp
