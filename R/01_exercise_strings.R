# This exercise is concerned with string operations. Use the base R string operations
# (grep, gsub, paste0, substr, strsplit, sprintf, nchar, ...) to solve these.
# There are other R-libraries that also offer string operations, but they should not
# be used here.

# Write a function that finds all URLs in a text. Input is a long string `input`
# containing text, some parts of which are URLs.
#
# A URL is the sequence 'http://' or 'https://' (where the http/https-part indicates the PROTOCOL)
# if not immediately preceded by another letter ("abchttp://" does not start a url, but "abc http://" may),
# followed by the DOMAIN NAME (containing two or more "labels" separated by a dot,
# where each label consists of letters, numbers or minus-signs but does not start with a minus-sign;
# labels contain at least one character), followed by "/", followed by a PATH. We limit ourselves to
# PATHs that contain only latin letters, numbers, minus-signs and "/"s (slashes). All these rules
# are case-insensitive, so a URL may start with HTTP or hTTp.
#
# Given an input String, return a `data.frame` with the character columns "protocol", "domainname" and
# "path", containing the information about all the URLs found *in the order they were found*. (The same URL may
# occur multiple times and should then be listed in the data frame multiple times). Make sure you don't include
# things that are not URLs because they don't satisfy all the rules.
#
# Input:
# - `input`: A `character(1)` containing the input string.
#
# Example input:
# > "https://www.google.com/ is probably the most popular url, sometimes used ashttp://www.google.com/ if one forgets
#    to use the more secure https:// protocol. Another URL that is popular is http://youtube.com which is owned
#    by the same company. On some computers, it is possible to find a website at http://localhost/ if the
#    computer runs a webserver. This exercise can be found at HTTPS://GITHUB.COM/PROGR-2020/02_STRUCTURED_PROGRAMMING."
# Output:
# > data.frame(protocol = c("https", "HTTPS"), domainname = c("www.google.com", "GITHUB.COM"),
#     path = c("", "PROGR-2020/02"), stringsAsFactors = FALSE)
# Example input:
# > "this text does not contain a url."
# Output:
# > data.frame(protocol = character(0), domainname = character(0),
#     path = character(0), stringsAsFactors = FALSE)
# Notes: many of the other occurrences of http.... do not count because they are either preceded directly by
# a letter, have no "/" following the domain name, or have a domain name with less than two labels.
# The path of the last URL is cut off early because we don't consider underscores as parts of the path.
#
# `stringsAsFactors = FALSE` is only necessary in old R versions (before 4.0) to prevent R from converting
# the data frame columns to factors.
#
# You should probably look into `regexpr`, `gregexpr`, `regexec` and similar to solve this problem.
ex01UrlFinder <- function(input) {
  # your code
  assertString(input)
  rx <- "(?<![[:alpha:]])(https?)://([[:alnum:]][-[:alnum:]]*(?:\\.[[:alnum:]][-[:alnum:]]*)+)/([-a-z[:digit:]/]*)"

  matches <- gregexpr(rx, input, perl = TRUE, ignore.case = TRUE)
  urls <- regmatches(input, matches)[[1]]

  m <- regexec(rx, urls, perl = TRUE, ignore.case = TRUE)
  urlparts <- regmatches(urls, m)
  res <- t(vapply(urlparts, identity, character(4), USE.NAMES = FALSE))
  res <- res[, -1]
  res <- as.data.frame(res, stringsAsFactors = FALSE)
  colnames(res) <- c("protocol", "domainname", "path")
  res
}

input <- ("HTTPS://GITHUB.COM/PROGR-2020/02_STRUCTURED_PROGRAMMING")
# This function gets two arguments: `parent` (character vector length 1, i.e. string))
# and `children` (character vector of arbitrary length).
# Return a single string describing this family in human words.
# E.g. parent = "Eric", children = c("Bob", "Helga") --> `Eric has 2 children: "Bob" and "Helga".`
#      parent = "Herbert", children = "Klaus Dieter" --> `Herbert has 1 child: "Klaus Dieter".`
#      parent = "Hildegard", children = character(0) --> `Hildegard has no children.`
#      parent = "Y", children = c("A", "B", "C")     --> `Y has 3 children: "A", "B" and "C".`
# Watch out for punctuation (comma, quotation around children but not parent, period at the end),
# singular vs. plural-form of 'children' and the special case of 0 children.
ex02Children <- function(parent, children) {
  # your code
  assertString(parent)
  assertCharacter(children, any.missing = FALSE)

  if (length(children) == 0) {
    return(paste(parent, "has no children."))
  }

  childx <- if (length(children) == 1) "child" else "children"

  children <- sprintf('"%s"', children)

  if (length(children) == 1) {
    rest <- children
  } else {
    rest <- sprintf("%s and %s",
      paste0(head(children, -1), collapse = ", "),
      tail(children, 1))
  }

  sprintf("%s has %s %s: %s.",
    parent, length(children), childx, rest)
}

# Now reverse the above:
# Given a string `sentence`, extract the `children` argument
# from above. However, sometimes the number of children is wrong, in that case
# the funtion should throw an error.
# Input:
# - `sentence`: A `character(1)` string containing a sentence to analyse.
# E.g. `Eric has 2 children: "Bob" and "Helga".` --> c("Bob", "Helga")
#      `Herbert has 1 child: "Klaus Dieter".`    --> "Klaus Dieter"
#      `Hilde,gard has 2 children: "01 0" and ",,,".`    --> c("01 0", ",,,")
#      `Y has 10 children: "A", "B" and "C".`    --> error
# (You can rely on the given sentence structure. There is always at least 1 child.
# The name of the parent does not contain spaces but may contain any other character.
# The name of children does not contain quotation marks but may contain any other character.)
# The order of children in the result does not matter
ex03ChildrenInverse <- function(sentence) {
  # your code
  assertString(sentence)
  rx <- "[^ ]+ has ([^ ]*) child(?:ren)?: (.*)"
  matches <- regmatches(sentence, regexec(rx, sentence))[[1]]
  childr <- matches[[3]]

  expected.children <- as.numeric(matches[[2]])

  children <- gsub('"', "",
    regmatches(childr, gregexpr('"[^"]*"', childr))[[1]]
  )

  if (length(children) != expected.children) {
    stop(sprintf("Expected %s children, but got %s.", expected.children, length(children)))
  }
  children
}
