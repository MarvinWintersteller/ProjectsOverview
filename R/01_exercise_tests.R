
# This exercise will work slightly differently than usual..
#
# Instead of just writing functions that solve a certain problem,
# you are given a few functions, some of which contain bugs.
#
# Your task is two-fold:
# - find the bugs in the functions and fix them. You are free
#   to make small changes in the function, or write the functions
#   from scratch; what counts is that the functions work.
#   The submission script will check that the functions do what
#   they are supposed to do.
# - write tests in the `tests/` directory that check that the
#   functions work. The submission script will check
#   1. that the functions have 100% coverage (using covr)
#   2. that slightly buggy versions of the functions will actually
#      trigger a test failure.
#   Take care that you name the test-***.R-files right (as
#   mentioned in the task description) or the submission script
#   will not notice they are there.
#
# Example: consider the function is.negative that you have seen
# in the slides. The specification for it are
# >> Write a function is.negative that takes a scalar numeric
# >> argument `x`, and returns a scalar logical.
# >> The return value is TRUE if x is negative, FALSE if
# >> x is zero or positive.
# >> An error should be thrown when x is not scalar numeric,
# >> is NaN or is NA.
is.negative <- function(x) {
  assertNumber(x)
  x < 0
}
# The submission script will replace this function by the function
# is.negative <- function(x) { assertNumber(x) ; x > 0 }
# -- which is wrong -- and see that your tests catch this error.
#
# I try to keep the tasks as easy to understand as I can, in return
# I will not write any examples of calls and expected results.
# Try to come up with them yourself in the tests!


# Write a function ex01IsPositive that takes a scalar numeric
# argument `x`, and returns a scalar logical.
# The return value is TRUE if x is positive, FALSE if
# x is zero or negative.
# An error should be thrown when x is not a scalar numeric,
# is NaN or is NA.
#
# The tests you write for this function should be in the `tests/`
# directory and start with `test-ex01IsPositive`,
# so you could have a file `test-ex01IsPositive.R`, but you could
# also have `test-ex01IsPositive01.R`, and `test-ex01IsPositive02.R`
# etc.
ex01IsPositive <- function(x) {
  assertNumber(x, na.ok = FALSE, null.ok = FALSE)
  x > 0
}
# An undirected Graph of n vertices can be represented
# by a symmetric n x n matrix X of values 0 and 1, where the element
# in row a and column b, i.e. X[a, b] is 1 if the edges a and b
# are connected by an edge, and 0 otherwise. (The matrix is symmetric,
# so X[a, b] == X[b, a], since a connection from a to b implies a
# connection from b to a.) This is the so-called "Adjacency matrix".
#
# Example:    0 0 1 0
#             0 0 1 1
#             1 1 0 1
#             0 1 1 0
#
# represents the graph
#
#      (1)  (2)
#       |  / |
#       | /  |
#      (3)--(4)
#
#  which could also be drawn as
#
#             (2)
#            / |
#   (1)---(3)  |
#            \ |
#             (4)
#
# Write a function areConnected that takes an adjacency matrix
# as input `adjacency`, an integer value `beginning` and an
# integer value `end`, and checks whether there is a series of
# edges that connect the `beginning` to the `end` vertex, returning
# TRUE if it is the case and FALSE if not. Errors should be thrown
# if the input is not valid (`adjacency` not a symmetric square matrix
# of 0 and 1, `beginning` or `end` not an integer value between 1 and ncol(adjacency)
# etc. Note: Vertices are always connected to themselves, regardless of
# the value at the diagonal.)
#
# The tests you write for this function should be in the `tests/`
# directory and start with `test-ex02AreConnected`

ex02AreConnected <- function(adjacency, beginning, end) {
  assertMatrix(adjacency, any.missing = FALSE, mode = "numeric")
  if (nrow(adjacency) != ncol(adjacency)) {
    stop("adjacency must be a square matrix")
  }
  assertInt(beginning, lower = 1, upper = nrow(adjacency))
  assertInt(end, lower = 1, upper = nrow(adjacency))
  if (!all(adjacency == t(adjacency))) {
    stop("matrix is not symmetric")
  }
  visited <- numeric(0)
  current.nodes <- beginning
  # if we hit a node we've seen before, then we quit and give up
  while (length(current.nodes)) {
    if (end %in% current.nodes) {
      # we can reach the 'end' from here
      # --> beginning and end are connected
      return(TRUE)
    }
    visited <- c(visited, current.nodes)
    current.nodes <- which(apply(adjacency[current.nodes, , drop = FALSE], 2, sum) > 0)
    # choose a node that was not visited before
    current.nodes <- setdiff(current.nodes, visited)
  }
  FALSE
}

# Write a function that determines if a 9x9-Matrix satisfies the conditions
# of "Sudoku" <https://en.wikipedia.org/wiki/Sudoku>:
# The conditions are:
# - only the digits 1..9 or missing values may be contained in the matrix
# - no digit may occur twice in any row, any column, or in any 3x3 subgrid
#   spanning rows / columns 1..3, 4..6, or 7..9.
# Your function should have one input `mat` and should check whether the
# input fulfills the conditions. If the input is not a 9x9 matrix containing
# numeric values, then an error should be thrown.
# The matrix may contain missing values (`NA`), which should not be counted
# towards any digit. I.e. the matrix made up completely of NAs is a valid
# sudoku, as is a matrix containing the numbers 1, 2, 3, .. 9 in the first
# row and otherwise only NAs, as is the matrix
#
#  5  3 NA | NA  7 NA | NA NA NA
#  6 NA NA |  1  9  5 | NA NA NA
# NA  9  8 | NA NA NA | NA  6 NA
# ---------+----------+---------
#  8 NA NA | NA  6 NA | NA NA  3
#  4 NA NA |  8 NA  3 | NA NA  1
#  7 NA NA | NA  2 NA | NA NA  6
# ---------+----------+---------
# NA  6 NA | NA NA NA |  2  8 NA
# NA NA NA |  4  1  9 | NA NA  5
# NA NA NA | NA  8 NA | NA  7  9
#
# (source: wikipedia)
#
# This matrix would, e.g., /not/ be a valid sudoku if the [1, 1] element
# were a 9 instead of a 5.
#
# The function should *not* check whether the sudoku is solvable or admits
# a unique solution; only check the conditions listed above.
#
# The tests you write for this function should be in the `tests/`
# directory and start with `test-ex03ValidSudoku`
ex03ValidSudoku <- function(mat) {
  assertMatrix(mat, nrow = 9, ncol = 9, mode = "numeric")
  if (!testSubset(mat, c(1:9, NA))) {
    return(FALSE)
  }
  for (i in 1:9) {
    if (anyDuplicated(c(mat[i, ]), incomparables = NA)) {
      return(FALSE)
    }
    if (anyDuplicated(c(mat[, i]), incomparables = NA)) {
      return(FALSE)
    }
  }
  for (subgridrow in 1:3) {
    for (subgridcol in 1:3) {
      if (anyDuplicated(c(mat[3 * (subgridrow - 1) + 1:3, 3 * (subgridcol - 1) + 1:3]), incomparables = NA)) {
        return(FALSE)
      }
    }
  }
  TRUE
}