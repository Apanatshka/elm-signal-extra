module Signal.ExtraTest where

import Signal exposing (Mailbox, mailbox, send, foldp, sampleOn)
import Task exposing (Task, sequence, andThen)
import Signal.Extra exposing (passiveMap2, withPassive, mapMany, andMap, deltas, (~), (<~))
import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)
import Native.ApanatshkaSignalExtra


-- A convenience for the testing DSL ... see use below ...
-- lets you do some andThens and then map last
(>>>) =
    flip Task.map

-- Basically, an `andThen` which ignores its argument
(>>-) task func =
    task `andThen` (always func)


infixl 4 >>>
infixl 4 >>-


{-| Construct a task which, when performed, will return the current value of a Signal. -}
sample : Signal a -> Task x a
sample =
    Native.ApanatshkaSignalExtra.sample


signalToSample = mailbox 0

sampleTest : Task x Test
sampleTest =
    send signalToSample.address 5
    >>- sample signalToSample.signal
    >>> assertEqual 5 >> test "should sample value"


-- Some mapping functions
consume2 : Int -> Int -> List Int
consume2 a b =
    [a, b]


consume4 : Int -> Int -> Int -> Int -> List Int
consume4 a b c d =
    [a, b, c, d]


signal1 = mailbox 0
signal2 = mailbox 0

-- We're testing the behaviour of a Signal, which is a little tricky,
-- since you want to make updates to the Signal and then see what
-- happens. 'Making updates to the signal' is essentially a matter
-- for Tasks -- that is, you stimulate a Signal via a Task. You can
-- then use the `sample` function above to get the value of a Signal,
-- and then use that to construct a test. So you end up with a Task
-- that eventually produces a Test. Once you've got that, you just
-- need to bundle them together and 'run' them.
passiveMap2OnlyFiresOnSignalA : Task x Test
passiveMap2OnlyFiresOnSignalA =
    let
        signal =
            passiveMap2 consume2 signal1.signal signal2.signal

        counter =
            foldp (\_ s -> s + 1) 0 signal

    in
        sequence (
            List.map (\int ->
                send signal1.address int
                >>- send signal2.address int
            ) [1 .. 5]
        )
        >>- sample counter
        >>> assertEqual 5 >> test "passiveMap2 fires only for first signal"


signal3 = mailbox 0
signal4 = mailbox 0

passiveMap2ActuallySamples : Task x Test
passiveMap2ActuallySamples =
    let
        signal =
            passiveMap2 consume2 signal3.signal signal4.signal

    in
        send signal4.address 25
        >>- send signal3.address 26
        >>- sample signal
        >>> assertEqual [26, 25] >> test "passiveMap2 actually samples"


signal5 = mailbox 0
signal6 = mailbox 0
signal7 = mailbox 0
signal8 = mailbox 0

complicatedMappingFiresCorrectly : Task x Test
complicatedMappingFiresCorrectly =
    let
        signal =
            consume4
                <~ signal5.signal
                ~ signal6.signal
                `withPassive` signal7.signal
                `withPassive` signal8.signal

        counter =
            foldp (\_ s -> s + 1) 0 signal

    in
        sequence (
            List.map (\int ->
                send signal5.address int
                >>- send signal6.address int
                >>- send signal7.address int
                >>- send signal8.address int
            ) [1 .. 5]
        )
        >>- sample counter
        >>> assertEqual 10 >> test "complicated signal fires only for first two signals"


signal9 = mailbox 0
signal10 = mailbox 0
signal11 = mailbox 0
signal12 = mailbox 0

complicatedMappingActuallySamples : Task x Test
complicatedMappingActuallySamples =
    let
        signal =
            consume4
                <~ signal9.signal
                ~ signal10.signal
                `withPassive` signal11.signal
                `withPassive` signal12.signal

    in
        send signal12.address 26
        >>- send signal11.address 27
        >>- send signal10.address 28
        >>- send signal9.address 29
        >>- sample signal
        >>> assertEqual [29, 28, 27, 26] >> test "passiveMap2 actually samples"


type alias User =
    { name : String
    , age : Int
    }

signal13 = mailbox ""
signal14 = mailbox 0

andMapAppliesSignalFunctionToSignal : Task x Test
andMapAppliesSignalFunctionToSignal =
    let
        signal = User
            `Signal.map` signal13.signal
            `andMap` signal14.signal
    in
        send signal13.address "Bobby"
        >>- send signal14.address 5
        >>- sample signal
        >>> assertEqual { name = "Bobby", age = 5 }
        >> test "andMap applies fn in first signal to result of second signal"


signal15 = mailbox ((+) 10)
signal16 = mailbox 0

andMapAppliesFunctionIfItUpdates : Task x Test
andMapAppliesFunctionIfItUpdates =
  let
      signal =
          signal15.signal
          `andMap` signal16.signal
  in
    send signal15.address ((+) 20)
    >>- sample signal
    >>> assertEqual 20
    >> test "andMap applies fn in first signal whenever it changes"


signal17 = mailbox ((+) 10)
signal18 = mailbox 0

andMapAppliesFunctionIfValueSignalUpdates : Task x Test
andMapAppliesFunctionIfValueSignalUpdates =
  let
      signal =
          signal17.signal
          `andMap` signal18.signal
  in
    send signal18.address 10
    >>- sample signal
    >>> assertEqual 20
    >> test "andMap applies fn in first signal to the value of the second when it changes"


deltasInitialValue : Task x Test
deltasInitialValue =
    let
        mbox =
            mailbox 0

        signal =
            deltas mbox.signal

    in
        sample signal
        >>> assertEqual (0, 0)
        >> test "deltas should start with the initial value of the signal duplicated"


deltasOneUpdate : Task x Test
deltasOneUpdate =
    let
        mbox =
            mailbox 0

        signal =
            deltas mbox.signal

    in
        send mbox.address 1
        >>- sample signal
        >>> assertEqual (0, 1)
        >> test "the first update to deltas should work"


deltasTwoUpdates : Task x Test
deltasTwoUpdates =
    let
        mbox =
            mailbox 0

        signal =
            deltas mbox.signal

    in
        send mbox.address 1
        >>- send mbox.address 2
        >>- sample signal
        >>> assertEqual (1, 2)
        >> test "the second update to deltas should work"


tests : Task x Test
tests =
    sequence
        [ sampleTest
        , passiveMap2OnlyFiresOnSignalA
        , passiveMap2ActuallySamples
        , complicatedMappingFiresCorrectly
        , complicatedMappingActuallySamples
        , andMapAppliesSignalFunctionToSignal
        , andMapAppliesFunctionIfItUpdates
        , andMapAppliesFunctionIfValueSignalUpdates
        , deltasInitialValue
        , deltasOneUpdate
        , deltasTwoUpdates
        ]
    >>> suite "Signal.Extra tests"
