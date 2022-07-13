# Comparison of Erlang type checkers: Dialyzer, ETC, and Gradualizer

[Dialyzer](https://www.erlang.org/doc/man/dialyzer.html) is a static analysis tool that identifies software discrepancies, such as definite type errors,
code that has become dead or unreachable because of programming error, and unnecessary tests,
in single Erlang modules or entire (sets of) applications.
It was developerd by Kostis Sagonas and is now maintained by the OTP team.

[ETC](https://github.com/vrnithinkumar/ETC) is a type checker described
in ["Bidirectional typing for Erlang"](https://dl.acm.org/doi/10.1145/3471871.3472966)
by Nithin Rajendrakumar and Annette Bieniusa.
It's the result of a short academic project into higher-rank polymorphism enabled bidirectional type checking of Erlang code.

[Gradualizer](https://github.com/josefs/Gradualizer) is a gradual type checker for Erlang and,
thanks to [Gradient](https://github.com/esl/gradient), also for Elixir.
The project is experimental, but has decent coverage of the Erlang and Elixir syntax.

The assumption of this comparison is to show that Gradualizer, while still experimental,
is already useful in practice thanks to:

-   good enough Erlang syntax coverage; as it turned out it's definitely better then ETC,
    though there are some language constructs which lead to incorrect reports

-   better performance than Dialyzer (significantly shorter run times on the same files,
    no PLT build/check time);
    in fact, thanks to [ErlangLS](https://erlang-ls.github.io/),
    Gradualizer can be used real-time in the background of a programmer's editor

-   easily understandable error messages

This gist compares Dialyzer, ETC, and Gradualizer functionality on the tests accumulated over time
in Gradualizer's repository.
Thanks go to the volunteers who dedicated their time and effort to build the test harness.

The setup:

-   Dialyzer version v4.4.1

-   forked ETC - a small fix on top of upstream was necessary to get proper error code reporting to shell
    https://github.com/erszcz/ETC/commit/677c763d93fae7fdc326cd9e028c0f59f1803037

-   Gradualizer v0.1.3-253-g748cbf8 - https://github.com/josefs/Gradualizer/pull/429

The Elixir script used to generate the TSV results file is also available in this gist.

The tests are grouped into four categories:
- "should pass" tests - we know Gradualizer should pass when run on these
- "should fail" tests - we know Gradualizer should NOT pass when run on
  these, i.e. it should detect and report type errors
- "known problems which should pass" - as the name implies, code examples
  which should not lead to warnings or crashes of Gradualizer
- "known problems which should fail" - negative of the above, i.e. code
  examples with known issues, which should be detected by Gradualizer, but
  are not yet (or Gradualizer crashes on them)

The results are summed up in the attached TSV file.

Dialyzer is quite mature, very stable, and even known by the slogan that "it's never wrong".
ETC and Gradualizer, on the other hand, are still considered experimental.

One interesting example of a polymorphic function that is a Gradualizer false negative
(it does not raise an error though it should), but does not type check with ETC is:

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
