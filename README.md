Sliding Puzzle solver
=====================
![Example gif output](http://ksaveljev.github.io/sliding-puzzle.gif)
=====================
Relatively simple solution of Sliding Puzzle game. Mostly developed for 15-Puzzle game, but can be used to solve other puzzles as well (8-Puzzle for example).

This solution is based on the post made by Jeffrey Rosenbluth which can be found here: http://martingalemeasure.wordpress.com/2014/06/24/solving-the-15-puzzle-with-haskell-and-diagrams-10/

The blog post describes the solution using A* search. My solution went a bit other way and solved it using the IDA* search.
=====================
    cabal sandbox init
    cabal build
    cabal run simple-solver input
    cabal run gif-solver input

[SlidingPuzzle.hs](SlidingPuzzle.hs) - the solver

[SimpleSolver.hs](SimpleSolver.hs) - console output of the solution, takes a filename as input param, prints out a list of boards (how we get from input to solution) and number of steps in the solution.

[GifSolver.hs](GifSolver.hs) - gif output of the solution, asks for filename as command line arguments are fed to the diagrams cmdline utility.
