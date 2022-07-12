# ETC results on Gradualizer tests

Gradualizer is a gradual type checker for Erlang and, thanks to Gradient, also for Elixir.
The project is experimental, but has decent coverage of the Erlang and Elixir syntax.
However, as of writing this there are known bugs in the trackers of both Gradualizer and Gradient.

ETC is a bidirectional type checker for Erlang.
It's the result of an academic project into higher-rank polymorphism enabled bidirectional typechecking of Erlang code.

This gist compares their functionality on the tests accumulated over time in Gradualizer's repository
thanks to the work of many people who volunteered their time and effort to write them.

The setup:

-   upstream Gradualizer
    https://github.com/josefs/Gradualizer/tree/ab75f28f9e6f6195c01ddd6a132cd7a07b7e52a2

-   forked ETC - a small fix on top of upstream was necessary to get proper error code reporting to shell
    https://github.com/erszcz/ETC/commit/677c763d93fae7fdc326cd9e028c0f59f1803037

Commands used to run the tests are in the relevant log files.

`etc.gradualizer-tests.ab75f28.log` contains an `ok | failed` summary for each test file.
`etc.gradualizer-tests.ab75f28.long.log` contains the actual logs returned from ETC.

For comparison with Gradualizer please refer to Gradualizer CI results:
- all `should_fail` tests fail type checking,
- all `should_pass` tests pass type checking,
- `known_problems` are respectively known false negatives and false positives or, in some rare cases, typechecker crashes.

That being said, as both projects are still considered experimental, it is expected some results won't be accurate.

One interesting example of a polymorphic function that is a Gradualizer false negative,
but is properly type checked by ETC is:

```
$ cat t3.erl
-module(t3).

-export([p/2]).

-spec p(A, A) -> A.
p(A, B) -> A + B.

-spec test() -> integer().
test() ->
    p(1, 3.2).
```