
# This is just for you to play around with, since it is in the slides.

estimatePi <- function(n) {

  isInCircle <- function(point) {
    if (point[[1]]^2 + point[[2]]^2 < 1) {
      TRUE
    } else {
      FALSE
    }
  }

  points <- lapply(seq_len(n), function(dummy) {
    c(x = runif(n = 1, min = -1, max = 1),
      y = runif(n = 1, min = -1, max = 1))
  })

  total.in.circle <- 0
  for (point in points) {
    if (isInCircle(point)) {
      total.in.circle <- total.in.circle + 1
    }
  }

  4 * total.in.circle / n
}

myPrint <- function(x) {
  x
  cat("Pi is about... ")
  assertAtomic(x)
  print(x)
}

calcPi <- function() {
  myPrint(estimatePi(10000000))
}
