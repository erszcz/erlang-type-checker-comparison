#!/usr/bin/env elixir


# Options

gradualizer_dir = "/Users/erszcz/work/erszcz/gradualizer"

opts = %{
  ## Where to look for Gradualizer tests?
  gradualizer_dir: gradualizer_dir,

  ## How many seconds can each tool run on a single file?
  timeout_seconds: 5 |> to_string(),

  ## Please note ETC should be modified to properly return the error code to the shell.
  ## See https://github.com/erszcz/ETC/commit/677c763d93fae7fdc326cd9e028c0f59f1803037
  etc: "/Users/erszcz/work/vrnithinkumar/ETC/etc",

  dialyzer: "dialyzer",
  dialyzer_plt: Path.join(gradualizer_dir, ".dialyzer_plt"),

  gradualizer: Path.join(gradualizer_dir, "bin/gradualizer"),

  etylizer: "/Users/erszcz/work/etylizer/etylizer/ety",

  eqwalizer: "/Users/erszcz/work/erszcz/erlang-type-checker-comparison/elp"
}


# Non-options

tests = %{
  ## debug only
  should_pass: [
    #"/Users/erszcz/work/erszcz/gradualizer/test/should_pass/unary_plus.erl",
    "/Users/erszcz/work/erszcz/gradualizer/test/should_pass/user_type_in_pattern_body.erl"
  ],
  should_fail: [
    "/Users/erszcz/work/erszcz/gradualizer/test/should_fail/case_pattern.erl"
  ]

  #should_pass: Path.wildcard("#{gradualizer_dir}/test/should_pass/*.erl"),
  #known_problems_should_pass: Path.wildcard("#{gradualizer_dir}/test/known_problems/should_pass/*.erl"),
  #should_fail: Path.wildcard("#{gradualizer_dir}/test/should_fail/*.erl"),
  #known_problems_should_fail: Path.wildcard("#{gradualizer_dir}/test/known_problems/should_fail/*.erl")
}

meta = %{
  erlang_version: System.cmd("asdf", ["current", "erlang"]) |> elem(0) |> String.trim(),
  gradualizer_tests: System.cmd("git", ["describe", "--tags"], cd: gradualizer_dir) |> elem(0) |> String.trim(),
}

IO.inspect(opts, label: "Opts")
IO.inspect(meta, label: "Meta")
#IO.inspect(tests, label: "Tests")


# Let's roll

timeout_args = ["-s", "KILL", opts.timeout_seconds]

check_one = fn args ->
  cmd = fn -> System.cmd("timeout", timeout_args ++ args, stderr_to_stdout: true) end
  case :timer.tc(cmd) do
    {micros, {output, 0}} -> {:ok, micros / 1000.0 / 1000.0, output}
    {micros, {output, _}} -> {:failed, micros / 1000.0 / 1000.0, output}
  end
end

tools = [
  #dialyzer: %{
  #  args: [opts.dialyzer, "--plt", opts.dialyzer_plt],
  #  prep: & &1,
  #  exec: check_one,
  #  vsn: System.cmd(opts.dialyzer, ["--version"]) |> elem(0) |> String.trim()
  #},
  gradualizer: %{
    args: [opts.gradualizer, "-pa", Path.join(opts.gradualizer_dir, "test_data"), "--"],
    prep: & &1,
    exec: check_one,
    vsn: System.cmd(opts.gradualizer, ["--version"]) |> elem(0) |> String.trim()
  },
  etylizer: %{
    args: [opts.etylizer],
    prep: & &1,
    exec: check_one,
    vsn: System.cmd("git", ["describe", "--tags", "--always"], cd: Path.dirname(opts.etylizer)) |> elem(0) |> String.trim()
  },
  eqwalizer: %{
    args: [opts.eqwalizer, "eqwalize", "--project", Path.join(opts.gradualizer_dir, "build_info.json")],
    prep: fn args ->
      {args, [module_path]} = :lists.split(length(args) - 1, args)
      module_name = Path.basename(module_path, ".erl")
      args ++ [module_name]
    end,
    exec: check_one,
    vsn: System.cmd("git", ["describe", "--tags", "--always"], cd: Path.dirname(opts.eqwalizer)) |> elem(0) |> String.trim()
  }
]

IO.inspect tools, label: "Tools"

all_tests = Enum.map(tests.should_pass, &{:should_pass, &1})
            ++ Enum.map(tests.should_fail, &{:should_fail, &1})
            #++ Enum.map(tests.known_problems_should_pass, &{:known_problems_should_pass, &1})
            #++ Enum.map(tests.known_problems_should_fail, &{:known_problems_should_fail, &1})

#IO.inspect all_tests, label: "All tests"

results = for {tool, spec} <- tools, into: [] do
  %{prep: prep, exec: exec, args: args} = spec
  {
    tool,
    all_tests
    |> Enum.map(fn {test_type, file} ->
      args = prep.(args ++ [file])
      cmd = args |> Enum.join(" ")
      IO.inspect(cmd, label: "Running")

      {res, time, output} = exec.(args)
      {
        file,
        [
          tool: tool,
          test_type: test_type,
          status: res,
          time: time,
          file: file,
          cmd: cmd,
          output: output
        ]
      }
    end)
    |> Enum.into(%{})
  }
end

#IO.inspect results, label: "Results"

tool_names = tools |> Keyword.keys() |> Enum.map(& &1 |> to_string() |> String.capitalize())

headers = ["Test type"]
          ++ tool_names
          ++ (tool_names |> Enum.map(& &1 <> " time"))
          ++ ["Test file"]

#IO.inspect(headers, label: "Headers")

IO.puts "\nTSV starts here\n"

headers |> Enum.intersperse("\t") |> IO.puts()

rows = for {test_type, file} <- all_tests do
  [test_type]
  ++ for {_tool, tool_results} <- results do
    tool_results[file][:status]
  end
  ++ for {_tool, tool_results} <- results do
    tool_results[file][:time]
  end
  ++ [file]
end

for row <- rows do
  row
  |> Enum.map(&to_string/1)
  |> Enum.intersperse("\t")
  |> IO.puts()
end
