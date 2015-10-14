module Signal.Stream(Stream, map, fairMerge, merge, mergeMany, fold, filterMap, filter, keepIf, sample, never, timestamp, toSignal, fromSignal) where

{-| Uninitialised signals, that only give updates and don't have the concept of
a current value. Like `Signal.Event.EventStream` (also in this package), only with values. 

@docs Stream

This library provides the basic building blocks for routing these streams of
events to your application logic.

# Mapping
@docs map

# Merging
@docs merge, fairMerge, mergeMany

# Folding
@docs fold

# Filtering
@docs filter, filterMap, keepIf, sample

# Primitive Streams
@docs never, timestamp

# Conversions
@docs toSignal, fromSignal
-}

import Signal exposing (Signal)
import Signal.Extra as SignalE exposing ((~>), (<~))
import Signal.Time as SignalT exposing (Time)
import Maybe exposing (Maybe(..))
import Debug

{-| Streams of events. Many interactions with the world can be formulated as
a stream of discrete events: mouse clicks, responses from servers, key presses,
etc.
-}
type alias Stream a = Signal (Maybe a)

-- Helper function, unsafe because 
fromJust : Maybe a -> a
fromJust m =
  case m of
    Just a -> a
    Nothing ->
      Debug.crash <| "There was an implementation error somewhere in "
        ++ "Signal.Stream. If you're using the latest version of "
        ++ "Apanatshka/elm-signal-extra, please file an issue (if there isn't "
        ++ "such an issue yet). "

maybeMap2 : (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f l r =
  case (l,r) of
    (Just a, Just b) -> Just (f a b)
    _ -> Nothing

{-| Apply a function to events as they come in. This lets you transform
streams.

    type Action = MouseClick | TimeDelta Float

    actions : Stream Action
    actions =
        map (always MouseClick) Mouse.clicks
-}
map : (a -> b) -> Stream a -> Stream b
map f =
  Signal.map (Maybe.map f)

{-| Convert a stream of values into a signal that updates whenever an event
comes in on the stream.

    url : Signal String

    url =
        toSignal "waiting.gif" imageStream

    constant : a -> Signal a

    constant value =
        toSignal value Stream.never
-}
toSignal : a -> Stream a -> Signal a
toSignal a str =
  Signal.merge (Signal.constant <| Just a) str ~> fromJust

{-| Ignore all updates to a signal, so it becomes the constant initial value. 
-}
init : Signal a -> Signal a
init sig = Signal.sampleOn (Signal.constant ()) sig

{-| Get a stream that triggers whenever the signal is *updated*. Note
that an update may result in the same value as before, so the resulting
`Stream` can have the same value twice in a row.

    moves : Stream (Int,Int)
    moves =
      fromSignal Mouse.position
-}
fromSignal : Signal a -> Stream a
fromSignal sig =
  Signal.merge (Signal.constant Nothing) (Just <~ sig)

{-| Like `merge`, but you get to decide which event wins when they come in at the same time.
-}
fairMerge : (a -> a -> a) -> Stream a -> Stream a -> Stream a
fairMerge f =
  SignalE.fairMerge (maybeMap2 f)

{-| Merge two streams into one. This function is extremely useful for bringing
together lots of different streams to feed into a `fold`.

    type Action = MouseClick | TimeDelta Float

    actions : Stream Action
    actions =
        merge
            (map (always MouseClick) Mouse.clicks)
            (map TimeDelta (fps 40))

If an event comes from either of the incoming streams, it flows out the
outgoing stream. If an event comes on both streams at the same time, the left
event wins (i.e., the right event is discarded).
-}
merge : Stream a -> Stream a -> Stream a
merge =
  fairMerge (\l _ -> l)

{-| Merge many streams into one. This is useful when you are merging more than
two streams. When multiple events come in at the same time, the left-most
event wins, just like with `merge`.

    type Action = MouseMove (Int,Int) | TimeDelta Float | Click

    actions : Stream Action
    actions =
        mergeMany
            [ map MouseMove Mouse.position
            , map TimeDelta (fps 40)
            , map (always Click) Mouse.clicks
            ]
-}
mergeMany : List (Stream a) -> Stream a
mergeMany =
  List.foldr merge never

{-| Create a past-dependent value. Each update from the incoming stream will
be used to step the state forward. The outgoing signal represents the current
state.

    clickCount : Signal Int
    clickCount =
        fold (\click total -> total + 1) 0 Mouse.clicks

    timeSoFar : Stream Time
    timeSoFar =
        fold (+) 0 (fps 40)

So `clickCount` updates on each mouse click, incrementing by one. `timeSoFar`
is the time the program has been running, updated 40 times a second.
-}
fold : (a -> b -> b) -> b -> Stream a -> Signal b
fold f b str =
  Signal.foldp (f << fromJust) b str


{-| Filter out some events. The given function decides whether we should
keep an update. The following example only keeps even numbers.

    numbers : Stream Int

    isEven : Int -> Bool

    evens : Stream Int
    evens =
        keepIf isEven numbers
-}
keepIf : (a -> Bool) -> Stream a -> Stream a
keepIf isOk stream =
  filterMap (\v -> if isOk v then Just v else Nothing) stream


{-| Filter out some events. If the incoming event is mapped to a `Nothing` it
is dropped. If it is mapped to `Just` a value, we keep the value.

    numbers : Stream Int
    numbers =
        filterMap (\raw -> Result.toMaybe (String.toInt raw)) userInput

    userInput : Stream String
-}
filterMap : (a -> Maybe b) -> Stream a -> Stream b
filterMap f =
  map f >> filter

{-| Filter out and unwrap the `Just`s in the stream.
-}
filter : Stream (Maybe a) -> Stream a
filter str = SignalE.filter Nothing str

{-| Useful for augmenting a stream with information from a signal.
For example, if you are operating on a time delta but want to take the current
keyboard state into account.

    inputs : Stream ({ x:Int, y:Int }, Time)
    inputs =
        sample (,) Keyboard.arrows (fps 60)

Now we get events exactly with the `(fps 60)` stream, but they are augmented
with which arrows are pressed at the moment.
-}
sample : (a -> b -> c) -> Signal a -> Stream b -> Stream c
sample f signal events =
  Signal.map2 (maybeMap2 f) (Just <~ signal) events

{-| A stream that never gets an update. This is useful when defining functions
like `mergeMany` which needs to be defined even when no streams are given.

    mergeMany : List (Stream a) -> Stream a
    mergeMany streams =
        List.foldr merge never streams
-}
never : Stream a
never =
  Signal.constant Nothing


{-| Add a timestamp to any stream. Timestamps increase monotonically. When you
create `(timestamp Mouse.x)`, an initial timestamp is produced. The timestamp
updates whenever `Mouse.x` updates.

Timestamp updates are tied to individual events, so
`(timestamp Mouse.x)` and `(timestamp Mouse.y)` will always have the same
timestamp because they rely on the same underlying event (`Mouse.position`).
-}
timestamp : Stream a -> Stream (Time, a)
timestamp =
  SignalT.timestamp >> Signal.map (\(t,m) -> Maybe.map (\a -> (t,a)) m)
