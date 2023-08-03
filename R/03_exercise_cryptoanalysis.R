# This exercise concerns itself with breaking the Vignere Cipher that you
# have gotten to know in 02_exercise_cryptography.R. This means we are writing
# a function that, when given a long enough encrypted text, can infer the
# encryption key and decrypt the message.
#
# You can not finish 03_exercise_cryptoanalysis.R if you have not finished
# ex02VignereCipher in 02_exercise_cryptography.R, so solve the other sheet
# first!

# To efficiently break encryption, we need an indicator that tells us whether
# the decrypted text is, in fact, a plain text that would be interesting.
#
# Write a function that estimates the log-likelihood that a given text is, in
# fact, a non-encrypted plain text, using the distribution of letters.
# Input:
# - text: A `character(1)` string made up of upper case letters and space
# Return: a scalar `numeric` giving the log likelihood of a given text. It can
# be calculated as the sum of the log likelihoods of individual letters in
# the text. The likelihoods of individual letters for the english language is
# given in the following table (based on an average word length of 4.79,
# as well as on the table in <https://en.wikipedia.org/wiki/Letter_frequency>)::
letterfrequencies <- 1 / 100 * c(
  A = 6.756, B = 1.234, C = 2.302, D = 3.518, E = 10.508, F = 1.843, G = 1.667,
  H = 5.041, I = 5.763, J = 0.127, K = 0.639, L = 3.330, M = 1.990, N = 5.583,
  O = 6.210, P = 1.596, Q = 0.079, R = 4.953, S = 5.234, T = 7.492, U = 2.282,
  V = 0.809, W = 1.952, X = 0.124, Y = 1.633, Z = 0.061, ` ` = 17.272)
# Example results of this are:
# ex01TextLikelihood("COME LETS EAT GRANDPA")
# #> -59.58358
# ex01TextLikelihood("DQPFBOFVVAGDUBJSCQERD")
# #> -86.87443
# Note that the log likelihood is larger (i.e. less negative) for the true
# english language text, and smaller for the encrypted text.
ex01TextLikelihood <- function(text) {
  # your code
  assertString(text, pattern = "^[A-Z ]+$")
  sum(log(letterfrequencies[strsplit(text, "")[[1]]]))
}


# A simple variant of the Vignere cipher is the "Caesar cipher", where every
# letter is shifted by the same amount. It corresponds to the Vignere cipher
# with key length of one letter. An example encrypted text:
#
# plaintext = "YOU MUST WORK TWENTY FOUR HOUR SHIFTS THIS MONTH"
# key = "P"
# ciphertext = "NDJPBJHIPLDG PILUCINPVDJGPXDJGPHXYVIHPIXYHPBDCIX"
#
# Write a function that estimates the most likely key for a given ciphertext.
# This is the key that generates a text that is most likely according to
# ex01TextLikelihood. The possible keys are the 26 letters as well as the space
# (`" "` -- this one does not change the text). You can make use of your
# `ex02VignereCipher()` function of Exercise 02 with `decrypt = TRUE` to find out the
# decrypted text for a key.
#
# Input:
# - `ciphertext`: A `character(1)` string made up of upper case letters and space
# Return: a list with two entries:
# - `key`: `character(1)` giving an upper case letter or space.
# - `log.likelihood`: `numeric(1)` giving the log likelihood of the text when
#   decrypting with this key.
# The result for the ciphertext given above should, for example, be
# #> list(key = "P", log.likelihood = -142.08608554750788)
ex02BreakCaesar <- function(ciphertext) {
  # your code
  assertString(ciphertext, pattern = "^[A-Z ]+$")
  keys <- c(LETTERS, " ")
  result <- vapply(keys, function(k) {
    ex01TextLikelihood(ex02VignereCipher(ciphertext, k, TRUE))
  }, numeric(1))
  list(key = names(which.max(result)), log.likelihood = max(result))
}

# The Vignere cipher corresponds to repeated application of the Caesar cipher
# on every Nth letter of the plain text, where N is the key length. The example
# from above:
#
# plaintext:  COME LETS EAT GRANDPA
# key:        ABC
# ciphertext: DQPFBOFVVAGDUBJSCQERD
#
# Can be represented as
#
# COME LETS EAT GRANDPA
# ---------------------
# C  E  E     T  R  D   -- KEY A --> D  F  F  A  U  S  E
#  O     T  E     A  P  -- KEY B -->  Q  B  V  G  B  C  R
#   M  L  S  A  G  N  A -- KEY C -->   P  O  V  D  J  Q  D
#                                    ---------------------
#                                    DQPFBOFVVAGDUBJSCQERD
#
# So given the known key length of 3, the ciphertext can be decomposed into the
# three ciphertexts
# "DFFAUSE", "QBVGBCR", and "POVDJQD", and the respective keys can be recovered.
#
# Write a function that, given a ciphertext and the keylength, recover the most
# likely key for that ciphertext.
# Inputs:
# - ciphertext: A `character(1)`  string made up of upper case letters and space
# - keylength: A scalar integer `numeric`, between 1 and the number of
#   characters in `ciphertext`.
# Return: a list with two entries:
# - `key`: `character(1)` giving the most likely key as an upper case string.
# - `log.likelihood`: `numeric(1)` giving the log likelihood of the text when
#   decrypting with this key, which is the sum of the likelihoods of the single
#   letter keys on their respective substrings.
#
# The example above, "DQPFBOFVVAGDUBJSCQERD", unfortunately only recovers the
# key "ABV", because the ciphertext is too short.
# The result when calling
# #> ex03BreakVignereWithKeylength("DQPFBOFVVAGDUBJSCQERD", 3)
# should be
# #> list(key = "ABV", log.likelihood = -58.376379229174)
ex03BreakVignereWithKeylength <- function(ciphertext, keylength) {
  # your code
  assertString(ciphertext, pattern = "^[A-Z ]+$")
  assertInt(keylength, lower = 1, upper = nchar(ciphertext))
  ctsplit <- strsplit(ciphertext, "")[[1]]
  results <- rbindlist(lapply(seq_len(keylength), function(start) {
    subs <- paste(ctsplit[seq(start, length(ctsplit), by = keylength)],
      collapse = "")
    ex02BreakCaesar(subs)
  }))
  list(
    key = paste(unlist(results[[1]]), collapse = ""),
    log.likelihood = sum(unlist(results[[2]]))
  )
}

# What is left is the estimation of the key length. We are doing that here by
# trying different key lengths and choosing the length that gives the highest
# maximum likelihood.
#
# This method is not entirely optimal because it disregards the fact that there
# are more degrees of freedom in long keys -- the overall maximum likelihood key
# like would always be the same length as the ciphertext and decrypt it to
# a sequence of spaces (" "), which is the highest likelihood character.
# We are therefore going to *penalize* the likelihood of long keys by
# subtracting `sqrt(8 * ciphertextlength * keylength)` from the likelihood of
# each key, thus favouring short keys.
#
# (This is based on an approximation of the distribution of likelihoods for
# truly random ciphertext; the factor of 8 is empirical here.)
#
# We are furthermore going to limit key
# lengths to 1/5th of the ciphertext length, rounded down, because keys that are
# longer are harder to break. (Consider that a truly random key with the same
# length as the ciphertext would be unbreakable.)
#
# Write a function that, given a ciphertext, recovers the key and plain text
# (using the `ex02VignereCipher()` function from the last Exercise) from it.
# Inputs:
# - ciphertext: A `character(1)`  string made up of upper case letters and space
# Return: A list with two entries:
# - `key`: `character(1)` giving the most likely key as an upper case setring.
# - `plaintext`: the decrypted text, according to that key.
# The key should be chosen using a key given by ex03BreakVignereWithKeylength
# from trying all keylengths from 1 to 1/5th the length of the ciphertext
# (rounded down), such that
# <<the key's log likelihood>> - sqrt(8 * <<ciphertextlength>> * <<keylength>>)
# is maximal.
#
# The ciphertexts you have seen so far are hard to decrypt because they are
# often too short in relation to the key length (as they were chosen for
# demonstration). Your function should be able to decipher the following text:
example.cipher <- paste("VZJANDYOSWKJZKXAIJCGVFMJBJJFYVAVNKSWBSWFKCJYREKTBVJDBTY NGWHCLXFKOQLLESWGRTGD",
  "WGGTFEUZTSDVRUNWWQCKNPYVBVNYZJBBJZKGNUUYZECJEUS GGJJIJBUMPC WGTTIEFWRPYNBYOY",
  "TPRCPEMIMHSUYKFSACCXAZIRGGRFCW I BEVJE UIPMI VJRKQCQPGDWNNYWBV")
ex04BreakVignere <- function(ciphertext) {
  # your code
  assertString(ciphertext, pattern = "^[A-Z ]+$")
  results <- rbindlist(lapply(seq_len(nchar(ciphertext) / 5),
    ex03BreakVignereWithKeylength, ciphertext = ciphertext))
  key <- results[which.max(
    log.likelihood - sqrt(8 * nchar(ciphertext) * seq_len(nrow(results)))
  ), key]
  plaintext <- ex02VignereCipher(ciphertext, key, decrypt = TRUE)
  list(key = key, plaintext = plaintext)
}
