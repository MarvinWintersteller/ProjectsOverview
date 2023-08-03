# This exercise builds on top of the 02_S3.R file.
# To evaluate the examples here, you need to source 02_S3.R first.

# Write a constructor, and implementation, of a `SetUnion`, and a
# `SetIntersection` class.  These represent sets that are unions / intersections
# of other sets, which need to be contained in the `SetUnion` /
# `SetIntersection` objects.  The `repr()` and `isElement()` implementations for
# these classes need to call `repr()` and `isElement()` on the contained sets to
# produce their results.

# Exercise 01: SetUnion
#
# `SetUnion()` should work as follows:
# Inputs:
# - `set1`: an object of class `Set`. You can use `assertClass()` here.
# - `set2`: an object of class `Set`.
# Returns: An object of class "SetUnion", as well as "Set".
SetUnion <- function(set1, set2) {
  # your code
  assertClass(set1, "Set")
  assertClass(set2, "Set")
  structure(
    list(set1 = set1, set2 = set2),
    class = c("SetUnion", "Set")
  )
}

# Exercise 02: repr.SetUnion
#
# The representation of a `SetUnion` with sets `set1` and `set2` should be of
# the form `"(set1 U set2)".
#
# You will have to call `repr()` on the objects in your set for this.
#
# Examples:
# > repr(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> [1] "({1, 2} U {1, 3})"
# > repr(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 0)
#   ))
# #> [1] "(({1, 2} U {1, 3}) U [-1, 0])"
repr.SetUnion <- function(obj) {
  # your code
  sprintf("(%s U %s)", repr(obj$set1), repr(obj$set2))
}

# Exercise 03: isElement.SetUnion
#
# An object is an element of a SetUnion(set1, set2) if it is an element of *at
# least one* of the sets contained in it.
#
# You will have to call `isElement` on the object in your set for this.
#
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 1)  # TRUE
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 3)  # TRUE
# > isElement(SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 4)  # FALSE
# > isElement(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), -0.5)  # TRUE
# > isElement(SetUnion(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), -1.5)  # FALSE
isElement.SetUnion <- function(set, object) {
  # your code
  isElement(set$set1, object) || isElement(set$set2, object)
}

# SetIntersection() is very similar to SetUnion, except for the fact that an
# an object is only element of an intersection if it contained in *both* sets.

# Exercise 04: SetIntersection
#
# `SetIntersection()` should work as follows:
# Inputs:
# - `set1`: an object of class `Set`. You can use `assertClass()` here.
# - `set2`: an object of class `Set`.
# Returns: An object of class "SetInterval", as well as "Set".
SetIntersection <- function(set1, set2) {
  # your code
  assertClass(set1, "Set")
  assertClass(set2, "Set")
  structure(
    list(set1 = set1, set2 = set2),
    class = c("SetIntersection", "Set")
  )
}

# Exercise 05: repr.SetIntersection
#
# The representation of a `SetIntersection` with sets `set1` and `set2` should
# be of the form `"set1 /\\ set2".
#
# Examples:
# > repr(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> [1] "{1, 2} /\\ {1, 3}"
# > repr(SetIntersection(
#     SetUnion(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 0)
#   ))
# #> [1] "(({1, 2} U {1, 3}) /\\ [-1, 0])"
#
# Note that we are using two backslashes here, because the backslash is an
# escape character.
#
# Like this, we should get:
# > print(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))))
# #> {1, 2} /\ {1, 3}
repr.SetIntersection <- function(obj) {
  # your code
  sprintf("(%s /\\ %s)", repr(obj$set1), repr(obj$set2))
}

# Exercise 06: isElement.SetIntersection
#
# An object is an element of a SetIntersection(set1, set2) if it is an element
# of *both* of the sets contained in it:
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 1)  # TRUE
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 3)  # FALSE
# > isElement(SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))), 4)  # FALSE
# > isElement(SetIntersection(
#     SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), 1)  # TRUE
# > isElement(SetIntersection(
#     SetIntersection(SetDiscrete(c(1, 2)), SetDiscrete(c(1, 3))),
#     SetInterval(-1, 1)
#   ), 0)  # FALSE
isElement.SetIntersection <- function(set, object) {
  # your code
  isElement(set$set1, object) && isElement(set$set2, object)
}
