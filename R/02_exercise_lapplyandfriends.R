# Write a function that collects a bunch of strings into one string.
#
# By default, the function should take multiple arguments that are then collected
# together into one string, quoted with quotation marks and separated by commas.
#
# ex01Collate("a", "b", "c") --> '"a", "b", "c"'
# ex01Collate("x") --> '"x"'
# ex01Collate(" ", "") --> '" ", ""'
# ex01Collate('"') --> '"""'
# ex01Collate() --> ""
#
# The arguments should take a scalar string each.
# ex01Collate(character(0)) --> ERROR
# ex01Collate(c("a", "b", "c")) --> ERROR
# ex01Collate(3) --> ERROR
# ex01Collate(NULL) --> ERROR
#
# The function should have optional parameters 'sep' and 'quote', which change the
# way the strings are concatenated:
# * `quote` is a scalar string, the content is put to the left and to the right
#    of each string being concatenated.
# * `sep` is a scalar string, it is put between each string being concatenated.
# By default, the function behaves like sep = ", " and quote = '"'.
#
# ex01Collate("a", "b", "c", sep = ",") --> '"a","b","c"' # (notice absence of spaces)
# ex01Collate(quote = "\\", "a", "b", "c") --> '\\a\\, \\b\\, \\c\\'
# ex01Collate("x", sep = " --> ", "y", "z", quote = "'") --> "'x' --> 'y' --> 'z'"
# ex01Collate("x", "y", "z", sep = "", quote = "") --> "xyz"
# ex01Collate("x", sep = 1) --> ERROR
ex01Collate <- function(..., sep = ", ", quote = '"') {
  # your code here
  assertString(sep)
  assertString(quote)
  args <- list(...)
  lapply(args, assertString)

  # paste(sprintf("%s%s%s", quote, unlist(args), quote), collapse = sep)

  ###  alternative, works in R 4.0.1 which came out mid-2020:
  # paste0(quote, unlist(args), quote, collapse = sep, recycle0 = TRUE)
  ### alternative, works in older R:
  if (!length(args)) {
    return("")
  }
  paste0(quote, unlist(args), quote, collapse = sep)
}


# Write a function that calls the ex01Collate-function multiple times.
#
# ex02CollateLists(list(c("a", "b"), c("c", "d", "e"))) --> c('"a", "b"', '"c", "d", "e"')
#
# This function should take three arguments:
# * `inputs`: a list of character vectors that are given to the `ex01Collate` function.
#   The ex01Collate function is called with each list element in turn, and the individual
# * `sep` (optional): used as the `sep`-parameter of ex01Collate in every call
# * `quote` (optional): used as the `quote`-parameter of ex01Collate in every call
# The return value should be a character vector of the same length as the `input` list
# Examples:
# ex02CollateLists(list()) --> character(0)
# ex02CollateLists(list(c(" ", ""), '"', character(0), c("a", "b", "c"))) -->
#   c('" ", ""', '"""', "", '"a", "b", "c"')
# ex02CollateLists(list("a", c("b", "c")), sep = " --> ", quote = "") --> c("a", "b --> c")
# ex02CollateLists(list("a", c(x = "b", y = "c"))) --> c('"a"', '"b", "c"')  # names of vector elements are ignored
# ex02CollateLists(list("a", c(quote = "b", sep = "c")))
#   --> c('"a"', '"b", "c"')  # names of vector elements are ignored, *even if* they are 'sep' or 'quote'
# ex02CollateLists(list(c("a", "b"), c(1, 2, 3))) --> ERROR (only character vectors should be given in the list)
# ex02CollateLists(list(list("a", "b", "c"))) --> ERROR (only char vectors in the list)
#
# Of course you can also solve this problem without using `ex01Collate` (we are just testing that
# this function behaves *as if* ex01Collate is being called) but that would probably be more work.
#
# You may find `do.call` to be useful here, as well as `lapply` or `vapply`.
ex02CollateLists <- function(inputs, sep = ", ", quote = '"') {
  # your code here
  assertList(inputs, types = "character", any.missing = FALSE)
  assertString(sep)
  assertString(quote)
  vapply(inputs, function(inp) {
    inp <- as.list(unname(inp))
    inp$sep <- sep
    inp$quote <- quote
    do.call(ex01Collate, inp)
  }, "")
}


# Write a function that determines if a matrix is a Latin Square
#
# A Latin Square is a square matrix of `n` rows and `n` columns,
# where only `n` different values occur, but where there are no
# duplicate values in either rows or columns. We restrict ourselves
# to Latin Squares made up of the numbers 1, 2, ... n.
#
# Your function should have one input:
# * `mat`: a square matrix with positive integer values
# and return either TRUE if the matrix is a Latin Square made up of
# the numbers 1, 2, ..., nrow(mat); it should return FALSE otherwise.
#
# Examples:
# ex03IsLatinSquare(matrix(c(1, 2, 2, 1), nrow = 2)) --> TRUE
# ex03IsLatinSquare(matrix(c(1, 2, 3, 2, 3, 1, 3, 1, 2), nrow = 3)) --> TRUE
# ex03IsLatinSquare(matrix(c(1, 2, 1, 2), nrow = 2)) --> FALSE  # not a latin square, values in rows not unique
# ex03IsLatinSquare(matrix(c(1, 1, 2, 2), nrow = 2)) --> FALSE  # not a latin square, values in columns not unique
# ex03IsLatinSquare(matrix(c(1, 2, 2, 3), nrow = 2)) --> FALSE  # not made up of {1, 2}
# ex03IsLatinSquare(matrix(c(1, 2, 2, 3), nrow = 2)) --> FALSE  # not made up of {1, 2}
# ex03IsLatinSquare(matrix(c(2, 3, 3, 2), nrow = 2)) --> FALSE  # not made up of {1, 2}
# ex03IsLatinSquare(matrix(1)) --> TRUE
# ex03IsLatinSquare(matrix(2)) --> FALSE  # the single element LS matrix can only contain 1
# ex03IsLatinSquare(1) --> ERROR (not a matrix)
# ex03IsLatinSquare(matrix(c(1, 2), nrow = 2)) --> ERROR (not a square matrix)
# ex03IsLatinSquare(matrix(c("1", "2", "2", "1"), nrow = 2)) --> ERROR (matrix of characters, not numbers)
# ex03IsLatinSquare(matrix(c(0, 1, 1, 0), nrow = 2)) --> ERROR (not positive values)
# ex03IsLatinSquare(matrix(c(1, 1.5, 1.5, 1), nrow = 2)) --> ERROR (not integer values)
#
# You may find the `apply` function useful here.
ex03IsLatinSquare <- function(mat) {
  # your code here
  assertMatrix(mat, mode = "integerish", any.missing = FALSE)
  assertIntegerish(mat, lower = 1, tol = 1e-100)
  if (nrow(mat) != ncol(mat)) {
    stop("not a square matrix")
  }
  testSetEqual(mat, seq_len(nrow(mat))) &&
    !any(apply(mat, 1, anyDuplicated)) &&
    !any(apply(mat, 2, anyDuplicated))
}


# write a function that turns a data.frame into a list of rows
#
# Your function should have one argument
# * `df` a data.frame with arbitrary rows and columns
# the return value should be a list of the single rows of `df`
# themselves lists. Example:
# ex04TableTranspose(data.frame(a = c(1, 2), b = c("x", "y"), stringsAsFactors = FALSE))
# --> list(list(a = 1, b = "x"), list(a = 2, b = "y"))
# ex04TableTranspose(matrix(c(1, 2, 2, 1), nrow = 2)) --> ERROR (not a dataframe)
# ex04TableTranspose(iris[1:3, ])
# --> list(list(Sepal.Length = 5.1, Sepal.Width = 3.5, Petal.Length = 1.4, Petal.Width = 0.2,
#               Species = factor("setosa", levels = c("setosa", "versicolor", "virginica"))),
#          list(Sepal.Length = 4.9, Sepal.Width =   3, Petal.Length = 1.4, Petal.Width = 0.2,
#               Species = factor("setosa", levels = c("setosa", "versicolor", "virginica"))),
#          list(Sepal.Length = 4.7, Sepal.Width = 3.2, Petal.Length = 1.3, Petal.Width = 0.2,
#               Species = factor("setosa", levels = c("setosa", "versicolor", "virginica"))))
# ex04TableTranspose(iris[FALSE, ]) --> list()  # zero-row data.frame gives empty list
# ex04TableTranspose(iris[1:3, FALSE]) --> list(list(), list(), list())
# # (zero-column data.frame gives list() for each row)
#
# You could subset each row and use as.list(). An alternative and elegant way to tackle this problem
# involves Map() and do.call().Should work for Zero or more columns.
ex04TableTranspose <- function(df) {
  # your code
  assertDataFrame(df)
  if (!ncol(df)) {
    return(replicate(nrow(df), list(), simplify = FALSE))
  }
  # Map(list, df$column1, df$column2, df$column3, ...)
  #do.call(Map, c(list(list), df))
  ### Alternative:
   lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}
