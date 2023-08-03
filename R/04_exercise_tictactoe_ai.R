# This is a continuation of last weeks tictactoe exercise. You can use these functions in order to
# complete this exercise.
#
# To use the functions from last week, copy your solution of last week's tictactoe exercise into
# not_an_exercise_tictactoe.R. (If you did not manage to finish it, you can also copy the model
# solution that is posted after then turn in date of homework 04).
# If you are doing interactive experiments, you can "source" the file:
# > source("R/not_an_exercise_tictactoe.R")
# The evaluate_submission tests will make use of the content of not_an_exercise_tictactoe.R
# automatically.


# Write a function that plays Tic Tac Toe perfectly.
#
# This function should have arguments `position` and `player`,
# that behave just as in ex03HumanPlayer or ex04RandomPlayer from last week,
# and return a move string just as they do.
#
# In this exercise you can use all functions from last week that you copied into
# not_an_exercise_tictactoe.R, and it is probably easiest if you do.
#
# If `position` is an empty matrix (all NA), then it is safe to return
# any (valid) move (because any of them are a draw for perfect opponents),
# but recommended to return a corner-space (A1, A3, C1, or C3)
#
# In any other position, you should be able to:
#  - iterate through all legal moves
#    - create a new position matrix representing the hypothetical position where that move was played
#    - compute what the best move of the opposing player would be at this point, and what the outcome
#      of that would be, by recursively calling ex01TBot itself directly or indirectly for each
#      hypothetical position.
# As a hint, a relatively elegant solution contains a call to ex05Tournament with ex01TBot
# for both players (using `suppressMessages` to keep output in check).
# If you want to read more about the system here (I don't think you have to to solve the exercise,
# but it gives some context): <https://en.wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves>.
#
# When you finish this, you should be able to play Tic Tac Toe against an unbeatable opponent using
# > ex05Tournament(ex03HumanPlayer, ex01TBot)
# (using ex05Tournament from last week's tictactoe exercise).
#
# For Advanced Students:
# The following is not necessary but speeds things up a small bit:
# It is possible to use "memoisation" (as the computer scientists would call it) or a "transposition
# table" (as the chess AI programmers would call it): remember the value / optimal response for each
# position to avoid having to compute it again. You do this by using an "environment":
tbot.cache <- new.env()
# This behaves like a list, but it has "reference semantics", so it can be changed globally from
# within a function. Play around a little with this:
# > tbot.cache <- new.env()
# > f <- function(x) { tbot.cache$k <- x }
# > f(10)
# > print(tbot.cache$k)  # --> 10 --- this would not work if `en` were a list!
# You can use this to make subsequent calls to ex01TBot faster.
#
# ex01TBot should be able to make perfect moves even for 'unreachable' positions, such as
# two "O" and no "X" present, so don't rely on precomputation for this task.
ex01TBot <- function(position, player) {
  # your code
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))

  moves <- apply(expand.grid(LETTERS[1:3], 1:3), 1, paste0, collapse = "")
  if (all(is.na(position))) return("A1")
  drawmove <- NULL
  validmove <- NULL
  for (m in moves) {
    coords <- tryCatch(
      ex02MoveStringToCoordinate(m, position),
      InvalidMoveError = function(e) {
        NULL
      })
    if (is.null(coords)) next
    validmove <- m
    pos.new <- position
    pos.new[coords[[1]], coords[[2]]] <- player
    value <- suppressMessages(ex05Tournament(ex01TBot, ex01TBot, setdiff(c("X", "O"), player), pos.new))
    if (value == player) {
      return(m)
    }
    if (value == "") {
      drawmove <- m
    }
  }
  if (!is.null(drawmove)) {
    drawmove
  } else {
    validmove
  }
}

# -------------------------------------
# Alternative solution: This is a wrapper around 'ex01TBotUncached' that uses the tbot.cache transposition
# table ("Memoization"), but works the same otherwise.
ex01TBot <- function(position, player) {
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))

  hash <- as.character(position)
  hash[is.na(hash)] <- "_"
  hash <- paste0(player, paste(hash, collapse = ""))
  # hash format: [player][A1][A2][A3][B1][B2][B3][C1][C2][C3]
  # we could also use digest::digest, but where is the fun in that
  cached.move <- tbot.cache[[hash]]
  if (!is.null(cached.move)) {
    return(cached.move)
  }
  move <-  ex01TBotUncached(position, player)
  tbot.cache[[hash]] <- move
  move
}


ex01TBotUncached <- function(position, player) {
  moves <- apply(expand.grid(LETTERS[1:3], 1:3), 1, paste0, collapse = "")
  if (all(is.na(position))) return("A1")
  drawmove <- NULL
  validmove <- NULL
  for (m in moves) {
    coords <- tryCatch(
      ex02MoveStringToCoordinate(m, position),
      InvalidMoveError = function(e) {
        NULL
      })
    if (is.null(coords)) next
    validmove <- m
    pos.new <- position
    pos.new[coords[[1]], coords[[2]]] <- player
    value <- suppressMessages(ex05Tournament(ex01TBot, ex01TBot, setdiff(c("X", "O"), player), pos.new))
    if (value == player) {
      return(m)
    }
    if (value == "") {
      drawmove <- m
    }
  }
  if (!is.null(drawmove)) {
    drawmove
  } else {
    validmove
  }
}
