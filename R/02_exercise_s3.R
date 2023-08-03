

# The following is a constructor for an S3 representation of a "discrete set"
# (of real numbers).  It represents a set of numbers.
SetDiscrete <- function(content) {
  assertNumeric(content, any.missing = FALSE)
  structure(
    list(content = unique(content)),
    class = c("SetDiscrete", "Set")
  )
}

# For the "Set"-class, we define the following generics:

# Give a string-representation for a set.
# Input:
# - `obj`: the object to represent
# Returns: `character(1)` a string giving a human-readable representation of the
#   object.
repr <- function(obj) {
  UseMethod("repr")
}

# For classes that do not have a repr-function, we return a boring string.
repr.default <- function(obj) {
  "Representation not implemented."
}

# Determine if `object` is a member of `set`.
# Input:
# - `set`: The set that is checked whether `object` is contained.
# - `object`: `numeric(1)`: The object to query.
# Returns: `logical(1)`: `TRUE` if `object` is in `set`, `FALSE` otherwise.
isElement <- function(set, object) {
  assertNumber(object)
  UseMethod("isElement")
}

# The implementation for the SetDiscrete are as follows:

# Note no assert is necessary for `obj`, since this method is only called when
# `obj` is a `SetDiscrete`.
repr.SetDiscrete <- function(obj) {
  paste0("{", paste(obj$content, collapse = ", "), "}")
}

# Note no assert is necessary for `set`, as above. Also no assert is necessary
# for `object`, since it is already asserted in the `isElement` generic
# function.
isElement.SetDiscrete <- function(set, object) {
  object %in% set$content
}

# Using the repr, we can also make a nice printer.
# This is implemented for *all* Set-objects.
print.Set <- function(x, ...) {
  cat(sprintf("A Set:\n%s\n", repr(x)))
  invisible(x)  # print.XXX must always return `invisible(x)`!
}


# If you source the file until here, you should be able to type in the
# following:
# > SetDiscrete(c(3, 1, 2, 3, 2, 1))
# And get the following output:
# #> A Set:
# #> {3, 1, 2}
# Also
# > isElement(SetDiscrete(c(1, 2)), 1)  # TRUE
# > isElement(SetDiscrete(c(1, 2)), 3)  # FALSE

# ------------------------------------------------------------------------------
# Write a constructor, and an implementation, of a `SetInterval` class.
# You will have to implement the functions `SetInterval()`,
# `repr.SetInterval()`, and `isElement.SetInterval()`.

# Exercise 01: SetInterval
#
# `SetInterval()` should work as follows:
# Inputs:
# - `lower`: `numeric(1)`, indicating the lower (inclusive) bound of the
#   interval.
# - `upper`: `numeric(1)`, indicating the upper (inclusive) bound of the
#   interval.
# Returns: An object of class "SetInterval", as well as "Set".
# An error should be given if `lower` is greater than `upper`.
SetInterval <- function(lower, upper) {
  # your code
  assertNumber(lower)
  assertNumber(upper)
  if (lower > upper) stop("'lower' must not be greater than 'upper'")
  structure(
    list(lower = lower, upper = upper),
    class = c("SetInterval", "Set")
  )
}

# Exercise 02: repr.SetInterval
#
# The representation of a `SetInterval` with bounds `lower` and `upper` should
# be of the form `"[lower, upper]".
#
# Examples:
# > repr(SetInterval(-1, 1))
# #> [1] "[-1, 1]"
# > repr(SetInterval(0, 0))
# #> [1] "[0, 0]"
repr.SetInterval <- function(obj) {
  # your code
  sprintf("[%s, %s]", obj$lower, obj$upper)
}

# Exercise 03: isElement.SetInterval
#
# An object is an element of a SetInterval(lower, upper) if it falls between
# these bounds, inclusive.
# > isElement(SetInterval(-1, 1), 0)  # TRUE
# > isElement(SetInterval(-1, 1), -1)  # TRUE
# > isElement(SetInterval(-1, 1), 1)  # TRUE
# > isElement(SetInterval(-1, 1), 1.01)  # FALSE
# > isElement(SetInterval(-1, 1), -100)  # FALSE
isElement.SetInterval <- function(set, object) {
  # your code
  object >= set$lower && object <= set$upper
}
