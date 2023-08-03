
# Write a function that calculates the numeric integral of another function
#
# A simple way to calculate the integral is to use the "Riemann sums",
# where the function is cut up into small intervals, and then the area under
# rectangles with the width of the intervals and the height of the function
# values at the beginning of each interval is calculated. The areas of these
# rectangles are then summed up. A more lengthy description is on wikipedia
# here: <https://en.wikipedia.org/wiki/Riemann_sum>.
#
# Your function should have three arguments:
# * `fun`, a function
# * `lower`, a finite numeric scalar (single element vector)
# * `upper`, a finite numeric scalar greater or equal to `lower`
#
# Your function should evaluate `fun` at equal interval distances, calculate
# the riemann sum integral approximation and return a single numeric value
# representing your approximation of the integral. You should use an interval
# width of at most 1e-4 = 1 / 10000 = "one over ten thousand".
#
# You can use smaller interval width (down to 1e-7, if you go much lower there will
# be rounding errors when adding up too small values and you are losing accuracy)
# if you want but that will slow down the tests. You may also use other numerical
# integration methods if you want, e.g. the trapezoidal rule or Simpson's rule.
#
# Your function should *not* use the `integrate()` method or similar library methods.
#
# Example behaviour:
# input: ex01Integrate(sin, 0, pi)
# output: 2 (or someting greater than 1.99 and smaller than 2.01)
# input: ex01Integrate(function(x) sin(x[[1]]), 0, pi)  # should work even for functions that don't vectorize!
# output: 2 (or someting greater than 1.99 and smaller than 2.01)
# input: ex01Integrate(sin, 0, 2 * pi)
# output: 0 (or something greater than -0.02 and smaller than 0.02)
# input: ex01Integrate(sin, 0, 0)
# output: 0 (or something greater than -0.0001 and smaller than 0.0001)
# input: ex01Integrate(sin, 1, 1)
# output: 0 (or something greater than -0.0001 and smaller than 0.0001)
# input: ex01Integrate(sin, 1, -1)
# --> ERROR because the upper bound is below the lower bound
# input: ex01Integrate(function(x, y) x^2, 0, 1)  # y-parameter is ok here since it is not used
# output: 1/3 (something greater than 0.332 and smaller than 0.334)
ex01Integrate <- function(fun, lower, upper) {
  # your code here
  assertFunction(fun)
  assertNumber(lower, finite = TRUE)
  assertNumber(upper, finite = TRUE, lower = lower)
  evalpoints <- seq(lower, upper, by = 1e-4)
  fvalues <- vapply(evalpoints, fun, 0)
  ### We do the following, but shorten it:
  # sum(fvalues * (upper - lower) / length(fvalues))
  mean(fvalues) * (upper - lower)
}



# Write a function that returns a function that checks a string for conditions
#
# Your function should take one input:
# * `threshold`: a scalar numeric
# Your function should *return a function*. The returned function should have
# one argument: a scalar string, and return either TRUE or FALSE.
#
# The returned function should check whether its input string contains
# at least one number greater than `threshold`.
# You can rely on the fact that numbers in the text are integers and separated from other
# text by spaces; they only consist of the digits 0-9 and possibly a minus sign in front.
#
# Examples
# fun <- ex02StringCondition(threshold = 10)
# fun("1 2 3 4") --> FALSE
# fun("9 10 11") --> TRUE
# fun("There are 10 houses on the street") -> FALSE
# fun("There are 11 houses on the street") -> TRUE
# fun("There are eleven houses on the street") --> FALSE  # string does not contain a number
#
# fun <- ex02StringCondition(threshold = -1)
# fun("There are 10 houses on the street") -> TRUE
# fun("My account balance is EUR -12") --> FALSE
# fun(c("10", "20")) --> ERROR (argument is not a scalar
#
# ex02StringCondition(threshold = "10") --> ERROR (not a numeric)
# ex02StringCondition(threshold = c(1, 2, 3)) --> ERROR (not a scalar)
ex02StringCondition <- function(threshold) {
  # your code
  assertNumber(threshold)
  function(string) {
    assertString(string)
    numberstrings <- strsplit(string, " ")[[1]]
    numbers <- suppressWarnings(as.numeric(numberstrings))
    any(numbers > threshold, na.rm = TRUE)
  }
}




# Write a function that selects out strings based on a condition
#
# Your function should have two arguments
# * `strings`: a character vector
# * `threshold`: a scalar numeric
# Your function should return a character vector containing all
# elements from `strings` that contain at least one number greater
# than `threshold`.
#
# Examples:
# ex03StringThreshold(
#   c("Hi", "1 2 3", "123",
#     "There are 10 houses on the street",
#     "There are 11 houses on the street"),
#   threshold = 10)
# --> c("123", "There are 11 houses on the street")
# ex03StringThreshold(character(0), 10) --> character(0)
# ex03StringThreshold(c("0", "", "Hi"), 10) --> character(0)
# ex03StringThreshold(10, 10) --> ERROR (strings is not a character vector)
# ex03StringThreshold("10", "10") --> ERROR (threshold is not a numeric)
#
# You may want to use `Filter`, and the solution of ex02StringCondition may be useful here.
ex03StringThreshold <- function(strings, threshold) {
  # your code
  assertCharacter(strings, any.missing = FALSE)
  assertNumber(threshold)
  Filter(ex02StringCondition(threshold), strings)
}
