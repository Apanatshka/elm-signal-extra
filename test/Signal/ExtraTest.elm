module Signal.ExtraTest where

import Signal exposing (Mailbox, mailbox, send, foldp, sampleOn, (~), (<~))
import Task exposing (Task, sequence, andThen)
import Signal.Extra exposing (lazyMap, withLazy, mapMany)
import ElmTest.Test exposing (..)
import ElmTest.Assertion exposing (..)


-- Some mapping functions
consume2 : Int -> Int -> List Int
consume2 a b =
    [a, b]


consume4 : Int -> Int -> Int -> Int -> List Int
consume4 a b c d =
    [a, b, c, d]


-- A convenience to make task types converge
ignore : Task x a -> Task () ()
ignore =
    Task.map (always ()) << Task.mapError (always ())


signal1 = mailbox 0
signal2 = mailbox 0

-- We're testing the behaviour of a Signal, and once you get
-- into Signal-world there is no way out -- you can only create
-- another Signal with a transformed type.  So, we're necessarily
-- creating a Signal of Tests, by mapping the Signal into a Test.
-- Then, we just need to stimulate the Signals in a way which will,
-- eventually, make the tests pass. That is, without the stimulation,
-- the tests will initially fail, but with the stimulation the tests
-- will eventually pass.
lazyMapOnlyFiresOnSignalA : Signal Test
lazyMapOnlyFiresOnSignalA =
    let
        signal =
            lazyMap consume2 signal1.signal signal2.signal

        counter =
            foldp (\_ s -> s + 1) 0 signal

        testify =
            assertEqual 5 >> test "lazyMap fires only for first signal"

    in
        Signal.map testify counter


-- And herewith the stimulation
lazyMapOnlyFiresOnSignalAStimulus : Task () ()
lazyMapOnlyFiresOnSignalAStimulus =
    ignore <| sequence <|
        List.map (\int ->
            send signal1.address int
            `Task.andThen`
            always (send signal2.address int)
        ) [1 .. 5]


signal3 = mailbox 0
signal4 = mailbox 0

lazyMapActuallySamples : Signal Test
lazyMapActuallySamples =
    let
        signal =
            lazyMap consume2 signal3.signal signal4.signal

        testify =
            assertEqual [26, 25] >> test "lazyMap actually samples"

    in
        Signal.map testify signal


lazyMapActuallySamplesStimulus : Task () ()
lazyMapActuallySamplesStimulus =
    ignore <|
        send signal4.address 25
        `Task.andThen`
        always (send signal3.address 26)


signal5 = mailbox 0
signal6 = mailbox 0
signal7 = mailbox 0
signal8 = mailbox 0

complicatedMappingFiresCorrectly : Signal Test
complicatedMappingFiresCorrectly =
    let
        signal =
            consume4
                <~ signal5.signal
                ~ signal6.signal
                `withLazy` signal7.signal
                `withLazy` signal8.signal

        counter =
            foldp (\_ s -> s + 1) 0 signal

        testify =
            assertEqual 10 >> test "complicated signal fires only for first two signals"

    in
        Signal.map testify counter


complicatedMappingFiresCorrectlyStimulus : Task () ()
complicatedMappingFiresCorrectlyStimulus =
    (ignore <| sequence <|
        List.map (\int ->
            send signal5.address int
            `Task.andThen`
            always (send signal6.address int)
            `Task.andThen`
            always (send signal7.address int)
            `Task.andThen`
            always (send signal8.address int)
        ) [1 .. 5]
    )


signal9 = mailbox 0
signal10 = mailbox 0
signal11 = mailbox 0
signal12 = mailbox 0

complicatedMappingActuallySamples : Signal Test
complicatedMappingActuallySamples =
    let
        signal =
            consume4
                <~ signal9.signal
                ~ signal10.signal
                `withLazy` signal11.signal
                `withLazy` signal12.signal

        testify =
            assertEqual [29, 28, 27, 26] >> test "lazyMap actually samples"

    in
        Signal.map testify signal


complicatedMappingActuallySamplesStimulus : Task () ()
complicatedMappingActuallySamplesStimulus =
    ignore <|
        send signal12.address 26
        `Task.andThen`
        always (send signal11.address 27)
        `Task.andThen`
        always (send signal10.address 28)
        `Task.andThen`
        always (send signal9.address 29)


tests : Signal Test
tests =
    mapMany
        (suite "Signal.Extra tests")
            [ lazyMapOnlyFiresOnSignalA
            , lazyMapActuallySamples
            , complicatedMappingFiresCorrectly
            , complicatedMappingActuallySamples
            ]


tasks : Task () ()
tasks =
    ignore <| sequence <|
        [ lazyMapOnlyFiresOnSignalAStimulus
        , lazyMapActuallySamplesStimulus
        , complicatedMappingFiresCorrectlyStimulus
        , complicatedMappingActuallySamplesStimulus
        ]
