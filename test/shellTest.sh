# YOU GOTTA RUN THIS FROM MAKE IT WILL NOT WORK OTHERWISE THE PATHS ARE BROKEN
#!/bin/bash

# makes numbers from 0 to n, with a linebreak ater each, formatted with preceeding 0s such that the character count matches for all numbers
makeRepetition() {
  n=$1
  for i in $(seq -w 0 $n); do #bash is so easy oh my word
    echo "$i"
  done
}

# takes 2 strings and a name, saves the 2 strings to temp files, diffs them, and passes if they are equal; shows diff otherwise. then deletes files
diffCheck() {
  expected=$(mktemp)
  output=$(mktemp)

  echo "$1" > "$expected"
  echo "$2" > "$output"

  if diff "$expected" "$output" > /dev/null; then
    echo "$3: Passed"
  else
    echo "$3: Failed"
    echo "Diff:"
    diff -y --suppress-common-lines "$expected" "$output"
  fi

  rm "$expected" "$output"
}

# this is impressively inefficient lmao
run() {
  stack exec brainfuck-interpreter-gui-exe -- -f "$1"
}

test_hello_world() {
  expected="Hello World!"
  output=$(run "./test/helloworld.bf")
  diffCheck "$expected" "$output" "Hello World test"
}

test_0_99() {
  expected=$(makeRepetition 99)
  output=$(run "./test/print0to99.bf")
  diffCheck "$expected" "$output" "0-99 test"
}

test_0_999() {
  expected=$(makeRepetition 999)
  output=$(run "./test/print0-999.bf")
  diffCheck "$expected" "$output" "0-999 test"
}

test_simpleadd() {
  expected='7'
  output=$(run "./test/simpleadd.bf")
  diffCheck "$expected" "$output" "Simple add test"
}

echo "Tests starting..."
test_hello_world
test_0_99
test_0_999
test_simpleadd
echo "Tests finished <3"
