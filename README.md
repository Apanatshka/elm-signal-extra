`signal-extra`
==============

Signal-related, advanced and convenience functions. 

`Signal.Time`
-------------
Sometimes things just move too quickly. You just need to get a grip on
time. This module helps you with that. Well, it helps you with
time-based operations for Elm `Signal`s anyway.

`Signal.Extra`
--------------
Utility functions. `(~>)`, `zip`/`unzip`s, special `foldp`s, running
buffers and more. 

`Signal.Discrete`
-----------------
Reacting on events, to be used in `Signal.sampleOn` or
`Signal.Discrete.folde`. 

`Signal.Stream`
----------------
Uninitialised signals. Forward-compatible with plans made to add this to core,
but only for as far as is definable in Elm 0.15.

`Signal.Alt`
------------
Alternative signal API based on different primitives. Planned, but I
make no promises
