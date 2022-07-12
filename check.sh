#!/usr/bin/env bash

# Options

## Where to look for Gradualizer tests?
GRADUALIZER_DIR=/Users/erszcz/work/erszcz/gradualizer

## How many seconds can each tool run on a single file?
TIMEOUT=5

## Please note ETC should be modified to properly return the error code to the shell.
## See https://github.com/erszcz/ETC/commit/677c763d93fae7fdc326cd9e028c0f59f1803037
ETC=/Users/erszcz/work/vrnithinkumar/ETC/etc

DIALYZER=dialyzer
DIALYZER_PLT=${GRADUALIZER_DIR}/.dialyzer_plt

GRADUALIZER=${GRADUALIZER_DIR}/bin/gradualizer

# Non-options

cd ${GRADUALIZER_DIR}

## debug only
#SHOULD_PASS="\
#        /Users/erszcz/work/erszcz/gradualizer/test/should_pass/unary_plus.erl \
#        /Users/erszcz/work/erszcz/gradualizer/test/should_pass/user_type_in_pattern_body.erl"

SHOULD_PASS=$(find "${GRADUALIZER_DIR}/test/should_pass" -type f -name \*.erl)
KNOWN_PROBLEMS_SHOULD_PASS=$(find "${GRADUALIZER_DIR}/test/known_problems/should_pass" -type f -name \*.erl)
SHOULD_FAIL=$(find "${GRADUALIZER_DIR}/test/should_fail" -type f -name \*.erl)
KNOWN_PROBLEMS_SHOULD_FAIL=$(find "${GRADUALIZER_DIR}/test/known_problems/should_fail" -type f -name \*.erl)

echo "We are running with:"
echo $(asdf current erlang)
echo "gradualizer tests" $(git describe --tags)

${DIALYZER} --version

#${ETC} --version
pushd $(dirname $ETC) 2>&1 >/dev/null
echo "ETC git" $(git describe --tags --always)
popd 2>&1 >/dev/null

${GRADUALIZER} --version

make .dialyzer_plt

echo

for file in $SHOULD_PASS; do
        printf "%s\n" $file

        printf "%14s\t" dialyzer
        timeout -s KILL $TIMEOUT $DIALYZER --plt $DIALYZER_PLT $file 2>&1 >/dev/null \
                && echo ok \
                || echo failed

        printf "%14s\t" etc
        timeout -s KILL $TIMEOUT $ETC $file 2>&1 >/dev/null \
                && echo ok \
                || echo failed

        printf "%14s\t" gradualizer
        timeout -s KILL $TIMEOUT $GRADUALIZER -pa test_data -- $file 2>&1 >/dev/null \
                && echo ok \
                || echo failed

        echo
done
