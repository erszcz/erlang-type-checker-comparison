# ETC results on Gradualizer tests

Gradualizer is a gradual type checker for Erlang and, thanks to Gradient, also for Elixir.
ETC is a bidirectional typechecker for Erlang.
This gist compares their functionality on the tests accumulated over time in Gradualizer's repository
thanks to the work of many people who volunteered their time and effort to write them.

The setup:

-   upstream Gradualizer
    https://github.com/josefs/Gradualizer/tree/ab75f28f9e6f6195c01ddd6a132cd7a07b7e52a2
    
-   forked ETC - a small fix on top of upstream was necessary to get proper error code reporting to shell
    https://github.com/erszcz/ETC/tree/3836052e3b5c2bb28237a71c557dab3bab56ef20

Commands used to run the tests are in the relevant log files.

etc.gradualizer-tests.ab75f28.log contains `ok | failed` summary for each test file.
etc.gradualizer-tests.ab75f28.long.log contains the actual logs returned from ETC for inspection.

For comparison with Gradualizer please refer to Gradualizer CI results:
- all `should_fail` tests fail type checking
- all `should_pass` tests pass type checking
- `known_problems` are respectively known false negatives and false positives or, in some rare cases, typechecker crashes