# In this exercise, you will be trying to make functions faster.
#
# Although performance is a decidedly tertiary goal of almost all code you will
# write, after correctness and understandability, it still helps to get a
# feeling for what operations are slow in R, and which ones are faster, and
# by how much.
#
# In the following exercises, you are given a reference-solution for each
# exercise. Your task is to write a function that behaves the same, but runs
# in less than 1/2 the time for the specified input (median speedup,
# as per `microbenchmark()`).
#
# Many of the functions can easily be sped up much more than by a factor of 2,
# and you are invited to see what methods give the largest gain.
# The speed measurement process with microbenchmark is a bit stochastic,
# however, and the test environment on the server probably also has a different
# CPU than your laptop, so different things may speed up by a different amount.
# The effect of this is not very large, but try to make the functions a bit
# faster than 2x speedup to have a safety buffer for the evaluation.
#
# As in the submissions before, you should only use R, not call any external
# programs or use any libraries except checkmate.
#
# Note: Because the reference functions were written to be deliberately slow,
# they should not serve as inspiration for your own code in the future!
#
# Hint: You probably want to copy the upper part of each function (the asserts)
# since they are usually not the slow part that can be made faster.


# You have taken a few noisy measurements, but you are worried about outlier
# values. You therefore want to remove a few of the largest, and of the smallest
# measurement values you have taken.
#
# Write a function that removes the `n.remove` largest, and the `n.remove`
# smallest values from a vector `x`, returning a vector of length
# `length(x) - 2 * n.remove`.
# Input:
# - `x`: `numeric` vector of values from which to remove outliers. You can
#   assume that `x` does not have duplicate values.
# - `n.remove`: non-negative integer `numeric(1)` indicating the number of
#   outliers to remove.
# Returns: A `numeric` equal to x, with the top `n.remove` and the bottom
# `n.remove` values removed (but with order otherwise preserved).
#
# Your function should have a median runtime 1/2 as much as
# `ex01RemoveOutliersReference` when called with a vector `x` of length
# 1000 and n.remove between 25 and 35.
# However, the correctness of your function is also checked for other input.
ex01RemoveOutliersReference <- function(x, n.remove) {
  assertCount(n.remove, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  for (i in seq_len(n.remove)) {
    x <- x[-which.max(x)]
    x <- x[-which.min(x)]
  }
  x
}

ex01RemoveOutliers <- function(x, n.remove) {
  # your code
  assertCount(n.remove, tol = 1e-100)
  assertNumeric(x, any.missing = FALSE)
  if (n.remove == 0) return(x)
  n.remove.upper <- length(x) - n.remove + 1
  x.partially.sorted <- sort(x, partial = c(n.remove, n.remove.upper))
  x[x > x.partially.sorted[[n.remove]] &
    x < x.partially.sorted[[n.remove.upper]]
  ]
}

# You have to make a decision between two alternative choices, choice A and
# choice B. To help in your decision, you have asked a large panel of experts
# in different fields how much they think choice A and choice B are preferrable.
# The experts evaluated the choices based on different things they consider
# relevant, such as profitability, environmental sustainability,
# effect on public relations, etc., and each expert has given choice A and
# choice B a score between 0 and 1. You will probably consider the experts'
# judgement in more detail, because some experts may be more reliable than
# others or consider more important issues than others. However, there is a
# specific shortcut you can take: If *no* expert has given choice A a *lower*
# score than choice B, but *at least one* expert has given choice A a *higher*
# score than choice B, then choice A "dominates" choice B, and you can disregard
# choice B right away.
# This is the concept of "Pareto Dominance" or "Pareto Efficiency":
# <https://en.wikipedia.org/wiki/Pareto_efficiency>.
#
# Write a function that calculates whether choice A dominates choice B that
# is faster than the following reference implementation.
# Inputs:
# - `scores.a`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice A.
# - `scores.b`: `numeric` with values between 0 and 1 (inclusive), indicating
#   the scores given by each expert to choice B.
# `scores.a` and `scores.b` should have the same length.
# The `i`th component of `scores.a` and `scores.b` are the scores given by
# expert `i`, so they can be compared directly.
#
# Your function should have a median runtime 1/2 as much as
# `ex02DominatesReference` on input for `scores.a` and `scores.b` generated
# as follows:
# scores.a <- runif(200, 0.478, 1)
# scores.b <- runif(200, 0, 0.522)
# (I.e. evaluations from 400 experts, choice A dominates choice B in
# approximately 50% of cases)
# However, the correctness of your function is also checked for other input.
ex02DominatesReference <- function(scores.a, scores.b) {
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))

  a.can.dominate <- FALSE
  a.can.not.dominate <- FALSE
  for (i in seq_along(scores.a)) {
    if (scores.a[[i]] < scores.b[[i]]) a.can.not.dominate <- TRUE
    if (scores.a[[i]] > scores.b[[i]]) a.can.dominate <- TRUE
  }
  if (a.can.not.dominate) return(FALSE)
  if (a.can.dominate) return(TRUE)
  FALSE
}

ex02Dominates <- function(scores.a, scores.b) {
  # your code
  assertNumeric(scores.a, any.missing = FALSE, lower = 0, upper = 1)
  assertNumeric(scores.b, any.missing = FALSE, lower = 0, upper = 1, len = length(scores.a))

  !any(scores.a < scores.b) && any(scores.a > scores.b)
}

# Consider a very simple ecological model that predicts how many individuals
# of a given species are alive in time `t`. The system starts out with
# `x(1)` individuals at time 1. In every discrete time step, the population
# changes because of two effects:
# - reproduction with reproduction rate `qr`: After every time step `t`,
#   `(qr - 1) * x(t)` individuals are added.
# - starvation with carrying capacity `G`: The *proportion* of individuals that
#   die and are removed after time step `t` is `x(t) / G` (and therefore depends
#   on the quantity of individuals `x(t)`, i.e. the more individuals are alive,
#   the greater the fraction of individuals that die). The *absolute quantity*
#   of individuals that are removed is therefore `x(t) * x(t) / G`.
# With the two effects, the quantity of alive individuals at time step `t + 1`
# is therefore
#   x(t + 1) = x(t) + (qr - 1) * x(t)   - x(t) * x(t) / G
#            = qr * x(t)                - x(t) * x(t) / G
#            = (1 / G) * x(t) * qr * G  - (1 / G) * x(t) * x(t)
#            = (1 / G) * x(t) * (qr * G - x(t))
# (we are not doing rounding here and are working with continuous quantities.)
#
#
# Write a function that calculates the sequence of quantities `x(t)` for `t`
# between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(1)` indicating `x(1)`, at least 0
# - `qr`: `numeric(1)` the reproduction rate, at least 1.
# - `g`: `numeric(1)` the carrying capacity, at least 0.
# - `t.max`: positive integer `numeric(1)` indicating the length of the result.
# Returns: a `numeric` vector giving the `x(t)` for `t` in 1 .. `t.max`.
#
# Your function should have a median runtime 1/2 as much as
# `ex03SimpleEcologyReference` for values of `t.max` of 1000, `qr` between 1
# and 4, `g` between 0 and 100, and x1 between 0 and `qr * g`.
#
# (If you are interested: This is the "logistic map" and plays a role in chaos
# theory: <https://en.wikipedia.org/wiki/Logistic_map>. To get from our formula
# to the one in Wikipedia, set `qr` to `r` and `G` to `1 / r`.)
ex03SimpleEcologyReference <- function(x1, qr, g, t.max) {
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- x1
  xt <- x1
  for (t in seq_len(t.max - 1)) {
    xt.next <- 1 / g * xt * (qr * g - xt)
    result <- append(result, xt.next)
    xt <- xt.next
  }
  result
}

ex03SimpleEcology <- function(x1, qr, g, t.max) {
  # your code
  assertNumber(x1, lower = 0)
  assertNumber(qr, lower = 1)
  assertNumber(g, lower = 0)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- x1
  xt <- x1
  g.inverse <- 1 / g
  qr.g <- qr * g
  for (t in seq_len(t.max)[-1]) {
    xt.next <- g.inverse * xt * (qr.g - xt)
    xt <- xt.next
    result[[t]] <- xt.next
  }
  result
}

# A more complex ecological system, made up of two species, is described by the
# "Lotka-Volterra" equations. It is made up of the "predator" and the "prey"
# species, where the quantity of prey grows by a constant factor and gets
# reduced proportionally to how many predators are present, and the quantity of
# predators grows proportionally to how many prey are present and gets reduced
# by a constant factor. The actual Lotka-Volterra equations are differential
# equations, but here we will use a discretization of the model:
#   - prey(t + 1)     = prey(t) * qr.prey         - prey(t) * predator(t) * qi.prey
#   - predator(t + 1) = predator(t) * qr.predator + predator(t) * prey(t) * qi.predator
#   - `prey` and `predator` never become negative. When any of the equations
#     above has a negative result, then the values are set to 0.
# Where `qr.prey` >= 1 and 0 <= `qr.predator` <= 1: With no predators present,
# the prey would grow exponentially, and with no prey present, the predators
# would die out.
#
# Write a function that calculates the sequence of quantities `prey(t)` and
# `predator(t)` for `t` between 1 and `t.max` (inclusive).
# Inputs:
# - `x1`: `numeric(2)` indicating `prey(1)` in component 1 and `predator(1)`
#   in component 2, both at least 0.
# - `qr`: `numeric(2)` indicating the natural growth rates in the absence of
#   the other species: `qr[1]` is `qr.prey` (at least 1) and `qr[2]` is
#   `qr.predator` (between 0 and 1, inclusive).
# - `qi`: `numeric(2)` indicating the effect of growth rates of the species on
#   each other. `qi[1]` is `qi.prey` and `qi[2]` is `qi.predator`, both at least
#   0.
# - `t.max`: positive integer `numeric(1)` indicating the number of entries in
#   the result.
# Returns: a `matrix` with two columns and `max.t` rows, where columns are named
# `prey` and `predator` containing the quantities of both at each time between
# 1 and `max.t`.
#
# Your function should have a median runtime 1/2 as much as
# `ex04ComplexEcologyReference` for values of `t.max` of 100, `qr[1]` between
# 1 and 1.2, `qr[2]` between 0.8 and 1, and `qi` between 0 and 0.2. Note that `t.max`
# is less than in ex03!
ex04ComplexEcologyReference <- function(x1, qr, qi, t.max) {
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(numeric(0), ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  for (t in seq_len(t.max)) {
    result <- rbind(result, xt, deparse.level = 0)
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    xt <- pmax(xt, 0)  # set values that are < 0 to 0
  }
  result
}

ex04ComplexEcology <- function(x1, qr, qi, t.max) {
  # your code
  assertNumeric(x1, len = 2, lower = 0, any.missing = FALSE)
  assertNumeric(qr, len = 2, any.missing = FALSE)
  if (qr[[1]] < 1 || qr[[2]] < 0 || qr[[2]] > 1) stop("qr[1] must be at least 1 and qr[2] must be between 0 and 1.")
  assertNumeric(qi, len = 2, lower = 0, upper = 1, any.missing = FALSE)
  assertInt(t.max, lower = 1, tol = 1e-100)
  result <- matrix(0, nrow = t.max, ncol = 2, dimnames = list(NULL, c("prey", "predator")))
  xt <- x1
  result[1, ] <- xt
  for (t in seq_len(t.max)[-1]) {
    xt <- xt * qr + c(-1, 1) * xt[[1]] * xt[[2]] * qi
    # optimization 1: pmax is surprisingly slow, replace it with the following
    xt[xt < 0] <- 0
    # optimization 2 (not necessary to pass the test): use pre-allocated result instead of rbind.
    result[t, ] <- xt
  }
  result
}
