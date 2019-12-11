# foogle

# Installation
1. Clone this repo 
2. Install [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
3. Run `$ stack build` in the root.

# Execution
Run `$ stack exec foogle-exe -- [args] filepath` in the root.

Run `$ stack exec foogle-exe -- -h` or `$ stack exec foogle-exe -- --help` to 
get a list of commands.

# Notes
* The parser ignores malformed encodings (looking at you, 
/extras/units/imperial/)

# TODOs
* Change `Main.hs` to actually use an argparser (`optparse-applicative`, perhaps)
  - [ ] Generate DB
  - [ ] Read from DB or directory
  - [x] Min, max number of input stack effects
  - [x] Input stack effect query
  - [x] Min, max number of output stack effects
  - [x] Output stack effect query
  - [x] Function name query
  - [x] Look at infixes
* Serialize to a better format
* Improve search capabilities and speed
* Clean up code
