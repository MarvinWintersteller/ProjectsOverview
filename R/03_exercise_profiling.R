# Complex Fractals
#
# R can represent /complex numbers/, which can be written by using the letter i
# as the imaginary unit. The imaginary unit i is defined by (1i)^2 == -1.
# > x <- 1 + 2i.
# > x^2
# --> -3 + 4i
# (because: (1 + 2i) * (1 + 2i) == 1*1 + 1*2i + 2i*1 + 2i*2i == 1 + 2i + 2i + 4*(1i^2) == 1 + 4i + 4*(-1))
#
# Consider the sequence `Z[n] = Z[n-1]^2 + c`, with two parameters: the starting value `Z[0]`, and the
# offset `c`. For some values of `Z[0]` and `c`, this sequence diverges
# (with absolute value of Z[n] -> Inf as n -> Inf),
# for example for `c = 1`, `Z[0] = 1`, we have `Z[1] = 2`, `Z[2] = 5`, `Z[3] = 26`, ... -> Inf
# for others it does not,
# for example `c = -1/2`, `Z[0] = 0` --> `Z[1] = -0.5`, `Z[2] = -0.25`, `Z[3] = -0.4375` ... -> -0.366
#
# The sequence gets interesting when you start considering /complex numbers/ for `c` and `Z[0]`.
# The set of complex numbers `X`, for which this series does not diverge when having `Z[0] = c = X` is
# known as the *Mandelbrot Set*. The set of complex numbers `X`, for which this series does
# not diverge when having `Z[0] = X` and a given `c` is known as the *Julia Set* of `c`.
#
# The following function ex01FractalReference creates pictures of the Mandelbrot Set or Julia Set. It checks for
# divergence by applying the formula and checking if Z has reached an absolute value > 2.
# The function takes arguments `xrange` (a 2 element numeric), `yrange` (a 2 element numeric), and `resolution`
# (a numeric scalar) and creates a coordinate grid using
# `seq(from = <lower bound>, to = <upper bound>, by = 1 / resolution)`. Then a matrix is created representing
# complex numbers on this coordinate grid. In "Mandelbrot"-mode, the matrix coordinate is used as the
# value for `c`; in "Julia"-mode, the matrix-coordinate is used as the initial value of Z, and the
# `c` value is taken as a function argument. Each matrix element receives the iteration, at which the
# recurrence formula gave rise to an absolute value > 2 for the first time.
#
# You can look at the results using the `image()` function:
# > res <- ex01FractalReference(30, 300)
# > image(t(res))
#
# Recommended calls:
#
# Mandelbrot Set:
# > ex01FractalReference(30, 300)
# > ex01FractalReference(100, 1000, xrange = c(-.3, .1), yrange = c(.8, 1.25))
# > ex01FractalReference(200, 10000, xrange = c(-0.8, -0.73), yrange = c(0.07, 0.18))  # SLOW!
#   ( Inspired by Wikipedia, <https://en.wikipedia.org/wiki/File:Mandel_zoom_02_seehorse_valley.jpg> )
# Julia Sets:
# > ex01FractalReference(300, 200, xrange = c(-1.5, 1.5), yrange = c(-1, 1),
#     picture = "Julia", c.param = c(-0.512, 0.521))
#   ( Inspired by Wikipedia, <https://en.wikipedia.org/wiki/File:Julia_set,_plotted_with_Matplotlib.svg> )
# > ex01FractalReference(30, 400, xrange = c(-1.5, 1.5), yrange = c(-1.5, 1.5),
#     picture = "Julia", c.param = c(0, 1))
#   ( "dendrite fractal", <https://mathworld.wolfram.com/DendriteFractal.html> )
# > ex01FractalReference(30, 400, xrange = c(-1.5, 1.5), yrange = c(-1.5, 1.5),
#     picture = "Julia", c.param = c(-0.123, 0.745))
#   ( "Douady's Rabbit Fractal", <https://mathworld.wolfram.com/DouadysRabbitFractal.html> )
#
# The function given here is fully functional, but a bit slow. Your task is to *write the same function,
# but faster*. Write a function `ex01Fractal` that gives the same output and behaviour as the function
# ex01FractalReference. Your function should run in less than 1/2 of the time as the reference function,
# on a test-battery similar to the calls listed above (in particular, the 6 calls above with xrange and
# yrange close to the ones given here, with "resolution" 1/4 of the one given here.)
#
# As in the submissions before, you should only use R, not call any external programs or use any libraries
# except checkmate.
#
# Hint: before running the benchmark, make sure that your function is at least a little fast using the
# calls above.
# Hint: You probably want to copy the upper part of this function, in particular the part calculating
# the exact coordinates at which the matrix output is being sampled; this is helps to make sure the
# output of your ex01Fractal and the reference match well.
ex01FractalReference <- function(iters, resolution, xrange = c(-2, 0.5), yrange = c(-1.25, 1.25),
  picture = "Mandelbrot", c.param = c(0, 0)) {
  assertCount(iters, positive = TRUE)
  assertNumber(resolution, lower = 1, finite = TRUE)
  assertNumeric(xrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(yrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(c.param, any.missing = FALSE, finite = TRUE, len = 2)
  assertChoice(picture, c("Mandelbrot", "Julia"))
  if (xrange[[2]] < xrange[[1]]) {
    stop("xrange[[2]] must be >= xrange[[1]]")
  }
  if (yrange[[2]] < yrange[[1]]) {
    stop("yrange[[2]] must be >= yrange[[1]]")
  }
  xvals <- seq(xrange[[1]], xrange[[2]], by = 1 / resolution)
  yvals <- seq(yrange[[1]], yrange[[2]], by = 1 / resolution)
  result <- matrix(0, nrow = length(yvals), ncol = length(xvals))
  for (row in seq_along(yvals)) {
    for (col in seq_along(xvals)) {
      current.complex <- xvals[[col]] + (1i) * yvals[[row]]
      Z <- current.complex
      if (picture == "Mandelbrot") {
        c.val <- current.complex
      } else {
        c.val <- c.param[[1]] + 1i * c.param[[2]]
      }
      for (j in seq_len(iters)) {
        if (abs(Z) > 2) break
        Z <- Z^2 + c.val
     }
      result[row, col] <- j
    }
  }
  result
}
#r <- ex01Fractal(100, 1000, xrange = c(-.3, .1), yrange = c(.8, 1.25))
#r <- ex01Fractal(200, 10000, xrange = c(-0.8, -0.73), yrange = c(0.07, 0.18))  # SLOW!

# I will display a ranking of the fastest functions that pass the test in a public GitHub repository
# for educational purposes (and as a small competition ;-) ) There will not be any reference to your name
# or your GitHub Username, unless you put these in a comment in the function, which I encourage you to do.
# However, if you really don't want me to publish your code / solution, please contact me by email or Mattermost
# -- Martin
  
ex01Fractal <- function(iters, resolution, xrange = c(-2, 0.5), yrange = c(-1.25, 1.25),
                        picture = "Mandelbrot", c.param = c(0, 0)) {
  # Submission by: <your name / gh username, optional>
  assertCount(iters, positive = TRUE)
  assertNumber(resolution, lower = 1, finite = TRUE)
  assertNumeric(xrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(yrange, any.missing = FALSE, finite = TRUE, len = 2)
  assertNumeric(c.param, any.missing = FALSE, finite = TRUE, len = 2)
  assertChoice(picture, c("Mandelbrot", "Julia"))
  if (xrange[[2]] < xrange[[1]]) {
    stop("xrange[[2]] must be >= xrange[[1]]")
  }
  if (yrange[[2]] < yrange[[1]]) {
    stop("yrange[[2]] must be >= yrange[[1]]")
  }
  xvals <- seq(xrange[[1]], xrange[[2]], by = 1 / resolution)
  yvals <- seq(yrange[[1]], yrange[[2]], by = 1 / resolution)
  result <- matrix(0, nrow = length(yvals), ncol = length(xvals))
  current.complex <- matrix(xvals, nrow = length(yvals), ncol = length(xvals), byrow = TRUE) +
        1i * matrix(yvals, nrow = length(yvals), ncol = length(xvals))
  Z <- current.complex
  if (picture == "Mandelbrot") {
    c.val <- current.complex
  } else {
    c.val <- c.param[[1]] + 1i * c.param[[2]]
    c.val <- matrix(c.val, nrow = length(yvals), ncol = length(xvals))
  }
  limit <- matrix(2, nrow = length(yvals), ncol = length(xvals))
  for (j in seq_len(iters)) {
    current <- !result
    zvc <- current.complex[current]
    result[current] <- (abs(zvc) > limit[current]) * j
    current.complex[current] <- zvc^2 + c.val[current]
  }
  result[current] <- j
  result
}
