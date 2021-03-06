## Description

Haskell implementation of Knuth's algorithm to solve MasterMind within 5 guesses.

## TODO

* [x] Generate random secrets
* [x] Implement command line args
* [x] Add option to pass a secret from the command line
* [x] Draw histogram when `-all` is specified
* [x] Parallelise using the [Par monad](https://hackage.haskell.org/package/monad-par) when `-all` is specified
* [ ] Add metrics

## Links

* [Mastermind (board game)](https://en.wikipedia.org/wiki/Mastermind_(board_game))
* [Five-guess algorithm](https://en.wikipedia.org/wiki/Mastermind_(board_game)#Five-guess_algorithm)
* [Knuth's mastermind algorithm](https://math.stackexchange.com/questions/1192961/knuths-mastermind-algorithm)
* [An implementation of Knuth's five-guess algorithm to solve a mastermind code](https://gist.github.com/firebus/2153677)
* [knuth-mastermind.pdf](https://www.cs.uni.edu/~wallingf/teaching/cs3530/resources/knuth-mastermind.pdf)
* [Mastermind using SVG and Vue.js](https://github.com/taylorjg/mastermind-svg-vue)
