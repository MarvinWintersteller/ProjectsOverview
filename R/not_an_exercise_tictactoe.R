# This file is a copy of 04_exercise_tictactoe.R from homework 04 (Debugging and Modular Software).
# The content should be filled out with your solution to 04_exercise_tictactoe.R and then sourced to
# make exercise 04_exercise_tictactoe_ai.R from *this* homework possible.
# If you did not solve 04_exercise_tictactoe.R last week, you can paste the model solution that is
# posted on monday instead.

# The Game "Tic Tac Toe" is a two player game with players taking turns occupying spaces on a 3x3 grid.
# A player wins by occupying either a full diagonal, a row, or a column of 3 spaces. A game between
# perfect players always ends in a draw.
#
# The goal of this exercise is to write a function that lets someone play 3x3 Tic Tac Toe against another
# human player, or against the computer.
#
# The playing field is a 3x3 matrix, with columns named A..C, and rows numbered 1..3
#
#   | A | B | C |
# --+---+---+---+
# 1 |   |   |   |
# --+---+---+---+
# 2 |   |   |   |
# --+---+---+---+
# 3 |   |   |   |
# --+---+---+---+
#
# Positions are represented by a 3x3-matrix with entries "X", "O" or NA.


# Part 01: Write a function that takes a position matrix and determines if there is a winner, and if
# so, who has won. The function should have a single input `position`; If this is not
# a 3x3 character matrix that may only contain "X", "O" or NA, an error should be thrown.
# The return value should be:
# - "X", if the "X"-player has won
# - "O", if the "O"-player has won
# - "" (empty string), if the game is a draw: no player has won, but there are no more possible moves
# - NA, if no player has won yet
# an error should be thrown if there are "two winners", e.g. a row of "X" and another row of "O",
# since this is an invalid position.
# We explicitly *do* allow positions that have more "X"s than "O"s, i.e. a position with two "X" and
# no "O", to make games possible where one player gets an advantage at the start of the game.
#
# Hint: If you use `assertMatrix(mode = "character")`, you will find that
# `matrix(NA, nrow = 3, ncol = 3)` will trigger an error, because the "mode" of `NA` is `logical`
# by default. To get a character-NA-matrix, use `matrix(NA_character_, nrow = 3, ncol = 3)`. See
# > is.character(NA)  # FALSE
# > is.character(NA_character_)  # TRUE
# There is a reason why the default `NA` is of type `logical`, can you think of it?
ex01Winner <- function(position) {
  # your code
  assertMatrix(position, mode = "character", nrow = 3, ncol = 3)
  assertSubset(position, c("X", "O", NA))
  getWinner <- function(spaces) {
    if (identical(spaces, rep("X", 3))) {
      "X"
    } else if (identical(spaces, rep("O", 3))) {
      "O"
    } else {
      NA_character_
    }
  }
  winners <- c(
    apply(position, 1, getWinner),
    apply(position, 2, getWinner),
    getWinner(diag(position)),
    getWinner(diag(position[, 3:1]))
  )
  winners <- unique(winners[!is.na(winners)])
  if (length(winners) > 1) {
    stop("Bad starting position")
  }
  if (length(winners)) {
    return(winners)
  }
  if (!any(is.na(position))) {
    return("")
  }
  NA
}

# Part 02: Write a function that converts Tic Tac Toe field coordinates and converts them to c(row, column)
# matrix coordinates, checking if they are valid in the process.
#
# Your function should take two inputs: `field`, and `position`.
# - `field` must be checked to be a single string (using checkmate).
# - `position` should be a 3x3 position matrix with same constraints as in ex01Winner.
#   (It is *not* necessary to check whether `position` has a winner, or if there are two winners etc.,
#   but it should be checked for being a character matrix with valid content)
# `field` must be of the form `<LETTER><NUMBER>`, e.g. "A1", or "C2". If it is not of this form, or
# if it contains letters or numbers not present on the field (e.g. A4 or Q2), an error of class
# "InvalidMoveError" should be thrown, for example by using
# `stop(errorCondition(<message>, class = "InvalidMoveError"))`
# Furthermore, it should be checked whether `field` is a valid move for a player to make, i.e. if the
# referenced field is already occupied by an "X" or an "O". If so, an "InvalidMoveError" should be thrown
# as well.
ex02MoveStringToCoordinate <- function(field, position) {
  # your code
  assertMatrix(position, mode = "character", nrow = 3, ncol = 3)
  assertSubset(position, c("X", "O", NA))
  assertString(field)
  # could also use 'expand.grid' here but that is just as long
  moves <- matrix(c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"), nrow = 3)
  if (!field %in% moves) {
    stop(errorCondition(sprintf("Invalid move: %s", field), class = "InvalidMoveError"))
  }
  coord <- c(which(field == moves, arr.ind = TRUE, useNames = FALSE))
  if (!is.na(position[coord[[1]], coord[[2]]])) {
    stop(errorCondition(sprintf("Position %s already occupied", field), class = "InvalidMoveError"))
  }
  coord
}

# Part 03: Write a function that interacts with a human player. Your function should take the arguments
# `position` and `player`.
# - `position` should be a 3x3 position matrix, checked in the same way as in ex02MoveStringToCoordinate.
# - `player` should be checked to be either the string "X" or the string "O".
#
# The function is called whenever the current position is `position`, and player `player` is asked to
# make a move. If the game is already over (a player has won, or there is a draw, as per ex01Winner),
# then an error should be thrown, as the player can no longer make a move.
# Otherwise, the function should prompt the player to make his choice, e.g. by printing the position,
# announcing that player `player` is to play, and getting an input by using the `readline()` function.
# (It is essential that `readline()` is called to prompt for input; this function will be replaced
# in the submission check).
# The user input should be sanitized (removing spaces using `gsub`, converting input to uppercase letters
# using `toupper`, so that an input of "a 3" still counts as valid) and checked:
#  - if the input is "Q", an error should be thrown because the player has ended the game.
#  - if the input is not a valid coordinate as by `ex02MoveStringToCoordinate`, e.g. because the chosen
#    space is occupied or the input is not valid, then an information message should be printed
#    and the user should be prompted *again* for a new input. It is recommended to use the
#    ex02MoveStringToCoordinate function here, in combination with tryCatch, to check user input.
#
# The function should return the (sanitized) string representing the move to be made.
#
# Example interaction with the program:
# > ex03HumanPlayer(matrix(c("X", NA, NA, NA, "O", NA, NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 NA  "O" NA
## 3 NA  NA  NA
## Player X to move. What is your move?
## (<letter><number>, or Q to quit): <INPUT> a9
## Invalid move: A9
## Player X to move. What is your move?
## (<letter><number>, or Q to quit): <INPUT> a1
## Position A1 already occupied
## Player X to move. What is your move?
## (<letter><number>, or Q to quit): <INPUT> c 3
# <RETURN VALUE:> "C3"
#
# > ex03HumanPlayer(matrix(c("X", NA, NA, NA, "O", NA, NA, NA, NA), nrow = 3), "X")
## Position:
##   A   B   C
## 1 "X" NA  NA
## 2 NA  "O" NA
## 3 NA  NA  NA
## Player X to move. What is your move?
## (<letter><number>, or Q to quit): <INPUT> Q
## Error in ex03HumanPlayer(matrix(c("X", NA, NA, NA, "O", NA, NA, NA, NA),  :
##   Player X Quit
#
# The tests do not check if the function prints anything, but it is recommended to give some output
# to make everything nicer.
ex03HumanPlayer <- function(position, player) {
  # your code
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))
  colnames(position) <- LETTERS[1:3]
  rownames(position) <- 1:3
  cat("Position:\n")
  print(position)
  repeat {
    input <- readline(sprintf("Player %s to move. What is your move?\n(<letter><number>, or Q to quit):", player))
    input <- gsub("[[:blank:]]", "", toupper(input))
    if (input == "Q") {
      stop(sprintf("Player %s Quit", player))
    }
    valid <- tryCatch({
      ex02MoveStringToCoordinate(input, position)
      return(input)
    }, InvalidMoveError = function(m) {
      cat(conditionMessage(m))
      cat("\n")
    })
  }
}


# Part 04: Write a function that makes random, but valid, Tic Tac Toe moves
#
# The function should have the same arguments as the human player: `position`, and `player`,
# and they should be checked the same way (validity, and whether the game is already over).
# The function should return a single string, which must a valid move to make.
#
# > ex04RandomPlayer(matrix(c("X", NA, NA, NA, "O", NA, NA, NA, NA), nrow = 3), "X")
# --> "B1" (for example, may be anything valid)
#
# Regardless of the name of the function, the output does not *need* to be randomised, it should
# just be *something* that is not further checked by the submission script besides being a legal move.
#
# Note how both ex03HumanPlayer and ex04RandomPlayer work the same way: they have the same
# inputs and outputs, only one of them works internally while the other asks something of the user.
ex04RandomPlayer <- function(position, player) {
  # your code
  assertMatrix(position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(position, c("X", "O", NA))
  assertChoice(player, c("X", "O"))
  moves <- matrix(c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"), nrow = 3)
  sample(moves[is.na(position)], 1)
}

# Write a function that lets two human players play against each other, or one player play
# against a computer opponent making random moves.
#
# Your function should have three arguments: `playerX`, `playerO`, and `starting.position`.
# - `playerX` should be a *function*. It could, for example, be ex03HumanPlayer, or ex04RandomPlayer.
# - `playerO` should be a function as well, just as playerX.
# - `starting.player` should be a string and must be either "X" or "O". This argument should be optional,
#    defaulting to "X".
# - `starting.position` should be a valid position matrix, as already seen in ex02MoveStringToCoordinate.
#    This argument should be optional, defaulting to `matrix(NA_character_, 3, 3)`.
#
# This function is supposed to host the Tic Tac Toe tournament and should operate in a loop.
# 1. It should be checked whether the game is over. If so, the identifier of the winning player should be returned,
#    "X", "O", or "" (draw). It is recommended to use ex01TWinner for this.
# 2. The next "player" should be asked for his move. Call the playerX or playerO function with the current
#    position (this may be a human player or the random player); the return value of the function is the
#    move to be made.
# 3. check that the move made by the player is valid and throw an error if not, e.g. by using
#    ex02MoveStringToCoordinate.
# 4. update the position matrix by inserting an "X" or an "O" at the correct space.
#
# If the playerX / playerO functions don't return invalid moves, this loop should eventually end and
# return a "X", "O" or "" string. In fact, if the starting position is a win for a player (or a draw), this
# function should return immediately without calling a player function.
#
# It may be useful to have this function generate some output. However,
# if you choose to generate output, you *MUST* use `message()` to communicate with the user. Don't use print(), cat(),
# dput() or similar.
#
# Example calls:
# > ex05Tournament(ex03HumanPlayer, ex04RandomPlayer)
#  (human player playing against random, the "X" player begins on an empty field)
# > ex05Tournament(ex03HumanPlayer, ex03HumanPlayer, "O")
#  (two humans playing against each other, the "O" player starting)
# > ex05Tournament(ex04RandomPlayer, ex04RandomPlayer,
#     starting.position = matrix(sample(c("X", "O", NA), 9, replace = TRUE), nrow = 3))
#  (two random players on a random starting position; enjoy the show. This may give an error if the
#   starting position has "two winners")
ex05Tournament <- function(playerX, playerO, starting.player = "X", starting.position = matrix(NA_character_, 3, 3)) {
  # your code
  assertMatrix(starting.position, mode = "character", nrows = 3, ncols = 3)
  assertSubset(starting.position, c("X", "O", NA))
  assertChoice(starting.player, c("X", "O"))
  assertFunction(playerX)
  assertFunction(playerO)

  position <- starting.position
  cp <- starting.player
  players <- list(X = playerX, O = playerO)
  repeat {
    winner <- ex01Winner(position)
    if (!is.na(winner)) return(winner)
    move <- players[[cp]](position, cp)
    coord <- ex02MoveStringToCoordinate(move, position)
    position[coord[[1]], coord[[2]]] <- cp
    message(sprintf("Player %s made move %s\nposition now:\n%s",
                    cp, move, paste(capture.output(print(position)), collapse = "\n")))

    cp <- setdiff(c("X", "O"), cp)
  }
}
