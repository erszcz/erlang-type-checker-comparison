#!/usr/bin/env elixir


# Options

gradualizer_dir = "/Users/erszcz/work/erszcz/gradualizer"

opts = %{
  ## Where to look for Gradualizer tests?
  gradualizer_dir: gradualizer_dir,

  ## How many seconds can each tool run on a single file?
  timeout: 5 |> to_string(),

  ## Please note ETC should be modified to properly return the error code to the shell.
  ## See https://github.com/erszcz/ETC/commit/677c763d93fae7fdc326cd9e028c0f59f1803037
  etc: "/Users/erszcz/work/vrnithinkumar/ETC/etc",

  dialyzer: "dialyzer",
  dialyzer_plt: Path.join(gradualizer_dir, ".dialyzer_plt"),

  gradualizer: Path.join(gradualizer_dir, "bin/gradualizer"),
}


# Non-options

tests = %{
  ## debug only
  #should_pass: [
  #  "/Users/erszcz/work/erszcz/gradualizer/test/should_pass/unary_plus.erl",
  #  "/Users/erszcz/work/erszcz/gradualizer/test/should_pass/user_type_in_pattern_body.erl"
  #],
  should_pass: Path.wildcard("#{gradualizer_dir}/test/should_pass/*.erl"),
  known_problems_should_pass: Path.wildcard("#{gradualizer_dir}/test/known_problems/should_pass/*.erl"),
  should_fail: Path.wildcard("#{gradualizer_dir}/test/should_fail/*.erl"),
  known_problems_should_fail: Path.wildcard("#{gradualizer_dir}/test/known_problems/should_fail/*.erl")
}

meta = %{
  erlang_version: System.cmd("asdf", ["current", "erlang"]) |> elem(0),
  gradualizer_tests: System.cmd("git", ["describe", "--tags"], cd: gradualizer_dir) |> elem(0),
  dialyzer_version: System.cmd(opts.dialyzer, ["--version"]) |> elem(0),
  etc_version: System.cmd("git", ["describe", "--tags", "--always"], cd: Path.dirname(opts.etc)) |> elem(0),
  gradualizer_version: System.cmd(opts.gradualizer, ["--version"]) |> elem(0)
}

IO.inspect(opts, label: "Opts")
IO.inspect(meta, label: "Meta")
#IO.inspect(tests, label: "Tests")


# Let's roll

timeout_args = ["-s", "KILL", opts.timeout]
dialyzer_args = [opts.dialyzer, "--plt", opts.dialyzer_plt]
etc_args = [opts.etc]
gradualizer_args = [opts.gradualizer, "-pa", Path.join(opts.gradualizer_dir, "test_data"), "--"]

check_one = fn args ->
  cmd = fn -> System.cmd("timeout", timeout_args ++ args, stderr_to_stdout: true) end
  case :timer.tc(cmd) do
    {micros, {_, 0}} -> {:ok, micros / 1000.0 / 1000.0}
    {micros, _} -> {:failed, micros / 1000.0 / 1000.0}
  end
end

headers = {"Test type", "Dialyzer", "ETC", "Gradualizer", "Dialyzer time", "ETC time", "Gradualizer time", "Test file"}

check = fn test_type, file ->
  IO.puts file
  {dialyzer_res, dialyzer_time} = check_one.(dialyzer_args ++ [file])
  {etc_res, etc_time} = check_one.(etc_args ++ [file])
  {gradualizer_res, gradualizer_time} = check_one.(gradualizer_args ++ [file])
  {test_type, dialyzer_res, etc_res, gradualizer_res, dialyzer_time, etc_time, gradualizer_time, file}
end

results_should_pass = for file <- tests.should_pass do
  check.(:should_pass, file)
end

results_known_problems_should_pass = for file <- tests.known_problems_should_pass do
  check.(:known_problems_should_pass, file)
end

results_should_fail = for file <- tests.should_fail do
  check.(:should_fail, file)
end

results_known_problems_should_fail = for file <- tests.known_problems_should_fail do
  check.(:known_problems_should_fail, file)
end

results = (
  results_should_pass
  ++ results_known_problems_should_pass
  ++ results_should_fail
  ++ results_known_problems_should_fail
)

#IO.inspect(headers, label: "Headers")
#IO.inspect(results, label: "results", limit: :infinity)

IO.puts "TSV starts here"

headers |> Tuple.to_list() |> Enum.intersperse("\t") |> IO.puts()

for row <- results do
  row
  |> Tuple.to_list()
  |> Enum.map(&to_string/1)
  |> Enum.intersperse("\t")
  |> IO.puts()
end
