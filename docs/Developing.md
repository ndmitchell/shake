# Developing Shake

I welcome contributions. Generally:

* Don't spend too much time working on something before raising an issue for it. There might be suggestions I can give, or reasons the work might not be appropriate.
* Pull requests are the best way to submit stuff.

### Development Workflow

I generally load Shake up in `ghci`, starting from the root directory, which has a `.ghci` file to set things up. Develop, hit `:r` to reload, then `:test` to run the test suite.

Individual test can be run interactively, too:

    $ :main list-tests
    $ :main TEST_NAME test

Replace `TEST_NAME` with one of the tests listed.

### Sandboxes

The tests do things like recompilation, which isn't particularly Cabal sandbox friendly. You can run some tests in the sandbox by doing:

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal repl shake-test
    $ :main TEST_NAME test --no-report
