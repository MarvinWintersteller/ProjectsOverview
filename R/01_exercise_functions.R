
# Write a function that converts temperatures between units.
#
# Your function should convert temperatures between the units "celsius",
# "fahrenheit", "kelvin", and "rankine", with the unit conversion rule
# T[celsius]    = T[kelvin] - 273.15
# T[fahrenheit] = T[kelvin] * 9/5 - 459.67
# T[rankine]    = T[kelvin] * 9/5
#
# Your function should take three arguments:
# * `temperature`, a finite numeric vector of arbitrary length
# * `from`, a string (single element character vector) which should be one of
#   "celsius", "fahrenheit", "kelvin", or "rankine"
# * `to`, a sring (single element character vector) which should be one of
#   "celsius", "fahrenheit", "kelvin", or "rankine"
# The `from` argument should be *optional*; if it is not given, conversion should
# be from "celsius".
# The `to` argument should be *optional*; if it is not given, conversion should
# be to "kelvin".
# You may want to edit the `function(...)` line to get default arguments.
#
# A temperature is *invalid* if it is below 0 Kelvin. Your function should give
# an error in that case. You could use `checkmate`, or call the `stop()` function
# to generate an error.
#
# Example behaviour:
# input: ex01TempConversion(0)
# output: 273.15
# input: ex01TempConversion(numeric(0))
# output: numeric(0)
# input: ex01TempConversion(100, "celsius", "fahrenheit")
# output: 212
# input: ex01TempConversion(c(-100, 100), to = "kelvin")
# output: c(173.15, 373.15)
# input: ex01TempConversion(c(1, 2, 3, 4), "rankine", "rankine")
# output: c(1, 2, 3, 4)
# input: ex01TempConversion(c(1, 2, 3, -1), "rankine", "rankine")
# --> ERROR: temperature is below 0 kelvin.
# input: ex01TempConversion(1, "rankine")
# output: 0.5555555555555
ex01TempConversion <- function(temperature, from = "celsius", to = "kelvin") {
  assertNumeric(temperature, any.missing = FALSE)
  # your code here
  assertChoice(from, c("celsius", "fahrenheit", "rankine", "kelvin"))
  assertChoice(to, c("celsius", "fahrenheit", "rankine", "kelvin"))
  temp.kelvin <- switch(from,
    kelvin = temperature,
    rankine = temperature * 5 / 9,
    fahrenheit = (temperature + 459.67) * 5 / 9,
    celsius = temperature + 273.15)
  if (any(temp.kelvin < 0)) {
    stop("temperature is below 0 kelvin.")
  }
  switch(to,
    kelvin = temp.kelvin,
    rankine = temp.kelvin * 9 / 5,
    fahrenheit = temp.kelvin * 9 / 5 - 459.67,
    celsius = temp.kelvin - 273.15)
}

# Write a function that returns the n'th Fibonacci number
#
# The Fibonacci-Numbers are defined as
# F(0) = 0,  F(1) = 1,  F(n) = F(n - 1) + F(n - 2)
#
# Your function should take one parameter:
# * `n` a non-negative scalar integer value
# and should return a scalar value: the n'th Fibonacci number.
#
# Example behaviour:
# ex02Fibonacci(-1) -> ERROR
# ex02Fibonacci(0.5) -> ERROR
# ex02Fibonacci(0) -> 0
# ex02Fibonacci(1) -> 1
# ex02Fibonacci(2) -> 1
# ex02Fibonacci(3) -> 2
# ex02Fibonacci(4) -> 3
# ex02Fibonacci(5) -> 5
# ex02Fibonacci(6) -> 8
# ...
# ex02Fibonacci(11) -> 89
# ... and so on
#
# Think how you can use the definition above and recursion to solve this
# problem in very few lines.
#
# For advanced Students: The above recursive relationship is not very
# computationally efficient, a better recursive relationship to go by is
# F(2 * n - 1) = F(n)^2 + F(n - 1)^2
# F(2 * n)     = (2 * F(n - 1) + F(n)) * F(n)
# Why is this better? Can you write a function that makes use of this
# relationship? For the CompSci students: what is the time complexity of
# these approaches?
ex02Fibonacci <- function(n) {
  # your code here
  assertCount(n, tol = 1e-100)
  if (n <= 1) {
    return(n)
  }
  ex02Fibonacci(n - 1) + ex02Fibonacci(n - 2)
}

# not very efficient:
# f(10) --> f(9) +                      f(8)
#       --> f(8) +        f(7) +        f(7) +        f(6)
#       --> f(7) + f(6) + f(6) + f(5) + f(6) + f(5) + f(5) + f(4)
#       --> .....
# Time this takes: T(n) = T(n - 1) + T(n - 2) + <constant overhead>
#             --> ~ grows like the Fibonacci sequence itself! O(1.62^n)
#
# ( O(2^n) is also correct, because O() gives an upper bound, but O(1.62^n)
# is a tighter bound.)

ex02Fibonacci <- function(n) {
  # your code here
  assertCount(n, tol = 1e-100)
  if (n <= 1) {
    return(n)
  }
  if (n %% 2 == 0) {
    n <- n / 2
    fn <- ex02Fibonacci(n)
    (2 * ex02Fibonacci(n - 1) + fn) * fn
  } else {
    n <- (n + 1) / 2
    ex02Fibonacci(n)^2 + ex02Fibonacci(n - 1)^2
  }
}

# quite fast:
# f(10) --> calls f(5),                   f(4)
#       -->       f(3),       f(2),       f(2),       f(1)
#       --> calls f(2), f(1), f(1), f(0), f(1), f(0)
# Time this takes: T(n) ~ T(n / 2) + T(n / 2 - 1) ~ Linear in n






# ... failing the point of the exercise, but also a solution:
ex02Fibonacci <- function(n) {
  # your code here
  assertCount(n, tol = 1e-100)
  golden.ratio <- (1 + sqrt(5)) / 2
  round(golden.ratio ^ n / sqrt(5))
}
