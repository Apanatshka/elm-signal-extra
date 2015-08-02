module Test where

import Graphics.Element exposing (Element)
import Signal exposing (Signal, Mailbox, mailbox, constant, send)
import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)
import ElmTest.Runner.Element exposing (runDisplay)
import Task exposing (Task, andThen, sequence, map)
import Signal.ExtraTest
import Signal.Extra exposing (mapMany)


-- Testing signals is a bit tricky, since once you get into
-- Signal-world, you can't really get out -- the most you can do
-- is construct another Signal with a transformed type.  Thus, we
-- need to end up with a Signal of Elements, and working back from
-- that, a Signal of Tests.
main : Signal Element
main =
    Signal.map runDisplay tests.signal


tests : Mailbox Test
tests =
    mailbox <|
        test
            "elm-signal-tests: initial state"
            (assert False)


-- Note that the sequence is to permit additional tests to be defined
-- easily ...  that is, you can add to the list
port tasks : Task x ()
port tasks =
    Task.map
        (suite "elm-signal-extra tests")
        (sequence
            [ Signal.ExtraTest.tests
            ]
        )
    `andThen` send tests.address

