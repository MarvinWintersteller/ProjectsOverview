
# This exercise tests your knowledge about *basic features of data.table*
# Basically all of these functions should contain a few checkmate assertions,
# followed by very few lines of code.
#
# Use checkmate `assertDataTable` or `assertDataFrame` to make assertions on
# data.table/data.frame arguments (depending on which one is named in the task
# description). Use `assertNames()` to check column names (as in the example
# to follow).
#
# Example: Write a function that removes rows from a data.table. Your function
# has three inputs:
# - `minuend`: a `data.table` with multiple columns, at least one of which is
#   named "index".
# - `subtrahend`: a `data.frame` or a `data.table` with exactly one column
#   named "index".
# - `do.nothing`: a scalar `logical`.
# If `do.nothing` is `TRUE`, then the `minuend` argument should be returned
# as-is. If `do.nothing` is `FALSE`, then a `data.table` should be returned
# with all rows removed that have a value of `"index"` that occurs in the
# `subtrahend` "index" column. Note that row order and duplicate rows of `minuend`
# (with index that does not occur in `subtrahend`) should be preserved.
#
# This could, for example, be solved with the following code:
demoSubtractRows <- function(minuend, subtrahend, do.nothing) {
  assertFlag(do.nothing)
  assertDataTable(minuend)
  assertNames(colnames(minuend), must.include = "index")
  assertDataFrame(subtrahend, ncol = 1)  # assertDataFrame accepts *both* data.table and data.frame, so it is ideal here
  assertNames(colnames(subtrahend), identical.to = "index")
  if (do.nothing) {
    minuend
  } else {
    minuend[!subtrahend, on = "index"]
  }
}

# If you have looked at some of the data.table material then these functions should
# be a breeze for you.


# Write a function that takes a list of named lists as input and creates a `data.table` as
# output, containing the input lists as rows.
# Inputs:
# - lst: `list` of named `lists` containing numeric scalars.
# Output: `data.table`
# The resulting `data.table` should contain the input rows in order and have columns according
# to the names of the input list (ordering of columns does not matter).
#
# Example:
# ex01List2DT(list(list(a = 1, b = 2), list(b = 3, a = 4)))
# --> data.table(a = c(1, 4), b = c(2, 3))
# Some lists do not contain elements for all columns; in that case, a 0 should be used:
# ex01List2DT(list(list(a = 1, b = 2), list(a = 4, c = 5)))
# --> data.table(a = c(1, 4), b = c(2, 0), c = c(0, 5))
# if there are elements in the lists that are not non-NA scalar numerics, an error should be thrown.
#
# You should probably use `rbindlist` and possibly `nafill` / `setnafill` here.
ex01List2DT <- function(lst) {
  # your code
  assertList(lst)
  lapply(lst, function(l) {
    assertList(l, names = "unique")
    lapply(l, assertNumber)
  })

  setnafill(rbindlist(lst, use.names = TRUE, fill = TRUE), fill = 0)[]
}

# Write a function that checks whether one data.table is the row-reordered form of another.
# We call this "equivalent" here. All tables must contain an `id` column (absence of which
# is an error), the content of which should be ignored for the purpose of checking equivalence.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `logical(1)` (i.e. `TRUE` or `FALSE`)
#
# Two tables are equivalent whenever one could reorder one table's rows to get the other table,
# ignoreing the content of the `id` column.
#
# The following should be `TRUE`:
# ex02TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(2, 1, 2), b = c("b", "a", "b"), id = c(4, 5, 6))
# )
#
# The following should be `FALSE`:
# ex02TablesEquivalent(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = c(1, 1, 2), b = c("a", "a", "b"), id = c(4, 5, 6))
# )
#
# Tables having different column names, columns in different order or columns of the
# same name with different types are always not equivalent (even if this concerns the `id` column).
# Tables with different numbers of rows can also not be equivalent.
#
# You can use `fsetequal` or `all.equal` here.
ex02TablesEquivalent <- function(a, b) {
  # your code
  assertDataTable(a, types = "atomic")
  assertNames(colnames(a), must.include = "id")
  assertDataTable(b, types = "atomic")
  assertNames(colnames(b), must.include = "id")
  isTRUE(all.equal(a[FALSE, ], b[FALSE, ])) && fsetequal(a[, id := NULL], b[, id := NULL])
}

# Write a function that returns all rows in `a` that also occur in `b`, ignoring the `id` column.
#
# Inputs:
# - a, b: `data.table` with atomic (non-list) columns
# Output: `data.table`
#
# Similarly to ex02TablesEquivalent, `a` and `b` contain an `id` column that should be ignored for
# the purpose of checking whether rows are the same (but the `id` of the table `a` should be included
# in the output!).
# Also, just like in ex02TablesEquivalent, different column names, order, or types (even regarding
# the `id` col) mean no rows are equal (and an empty version of `a`, e.g. `a[FALSE, ]` should be returned).
#
# Example:
# ex03TablesIntersect(
#   data.table(a = c(1, 2, 2), b = c("a", "b", "b"), id = c(1, 2, 3)),
#   data.table(a = 2, b = "b", id = 10)
# )
# --> data.table(a = c(2, 2), b = c("b", "b"), id = c(2, 3))  # returning rows 2 and 3 of `a`, since they
#                                                             # are also found in `b`
#
# This can be solved using a "join", using `merge()` or `[... on = ...]`. Be careful that, when using
# the letters `a` and `b` within `[ ... ]`, you know whether they reference the function arguments `a`/`b`,
# or the columns within the table that could also be `a` or `b`.
ex03TablesIntersect <- function(a, b) {
  # your code
  assertDataTable(a, types = "atomic")
  assertNames(colnames(a), must.include = "id")
  assertDataTable(b, types = "atomic")
  assertNames(colnames(b), must.include = "id")
  if (!isTRUE(all.equal(a[FALSE, ], b[FALSE, ]))) {
    return(a[FALSE, ])
  }
  b[, id := NULL]
  b <- unique(b)
  a[b, on = colnames(b)]
}


# Write a function that reads in a `data.table` from a file.
#
# Inputs:
# - `fname`: file name (relative to the repository base directory)
# Output: `data.table`
#
# The file format is: The first three rows of the file contain
# comments and should be skipped. They are followed by a row of
# column names, and then data. Columns are separated by semicolon (`;`) and
# should be read either as `numeric` or `character` columns, as appropriate.
# A string `MISSING` indicates the value should be NA.
# If a column data contains at least one value with at least one comma (`,`), this
# column should be a list column with character vectors of possibly variable
# length as values.
#
# Consider the `example.csv` file, which should give rise to the following
# behaviour:
# ex04ReadTable(file.path("R", "example.csv"))
# -->
# data.table(
#   species = c("cat", "dog", "eldritch"),
#   names = list(c("Felix", "Sir Cattington", "Larry"), c("Maggie", "Snuffles", "Klaus"),
#     c("Yaldabaoth", "Behemoth", "The Other")),
#   ages = list(c("3", "7", "4"), c("1", "4", "3"), c("10234", "5231", "13792168024")),
#   "food, class, if known" = c("Cat Food", "Dog Food", NA),
#   "cost, approximate, if known" = c(100, 250, NA)
# )
#
# Note the `names` and `ages` columns contain lists of characters only, while the `cost` column is numeric.
#
# Here you should use `fread` but you also need to do some post-processing.
ex04ReadTable <- function(fname) {
  # your code
  assertString(fname)
  dt <- fread(fname, sep = ";", na.string = "MISSING", skip = 3)
  cols <- colnames(dt)[vapply(dt, function(x) any(grepl(",", x)), logical(1))]
  if (length(cols)) {
    dt[, (cols) := lapply(.SD, strsplit, split = ","), .SDcols = cols]
  }
  dt
}
