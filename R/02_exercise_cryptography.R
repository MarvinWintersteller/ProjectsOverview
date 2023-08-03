# Palindromes are words or sentences that, when read backwards, give the same sentence,
# ignoring punctuation, spacing, or capitalization. Examples are
# > "Was it a car or a cat I saw?"
# > "Go hang a salami, I'm a lasagna hog"
# Write a function that returns TRUE if a given string is a palindrome, FALSE otherwise.
# Input arguments:
# - `input`: A `character(1)`.
# The input value is a character vector with one element (written `character(1)` and what we
# call a "string"). You can rely on there only being latin letters, punctuation marks and
# spaces being present, and that the string contains at least one letter.
ex01Palindrome <- function(input) {
  # your code
  assertString(input)
  chars <- strsplit(input, NULL)[[1]]
  cleanchars <- grep("[[:punct:][:space:]]",
    chars, value = TRUE, invert = TRUE)
  cleanchars <- tolower(cleanchars)
  all(cleanchars == rev(cleanchars))

}

# The "Vignere" Cipher is a method of encrypting text that has once been widely used..
# It uses a `plaintext` and a `key` and generates encrypted text. This works by first
# repeating the `key` until one gets a string with the same length as the plaintext.
# Then each letter is converted to a number corresponding to its position in the alphabet.
# The letter values of the repeated key and the plaintext are added and converted back
# to letters (modulo the number of letters). Decryption can be done by subtracting,
# instead of adding, the key.
#
# We are working with the alphabet of the space character " " (value 0) and the 26 capital
# letters of the latin alphabet (contained in the R variable `LETTERS`, numbered 1..26.)
#
# Example:
# plaintext     = I LOVE MY PARENTS KYLIE MINOGUE AND KERMIT THE FROG
# key           = LEMON
# repeated key  = LEMONLEMONLEMONLEMONLEMONLEMONLEMONLEMONLEMONLEMONL
# plaintext converted to numbers:
#               = c( 9, 0, 12, 15, 22,  5, 0, 13, 25,  0, 16, 1, 18,  5, 14, 20, 19,  0, .....
# repeated key converted to numbers:
#               = c(12, 5, 13, 15, 14, 12, 5, 13, 15, 14, 12, 5, 13, 15, 14, 12,  5, 13, .....
# sum of these two
#               = c(21, 5, 25, 30, 36, 17, 5, 26, 40, 14, 28, 6, 31, 20, 28, 32, 24, 13, .....
# some of the values are larger than 26, so we have to wrap them back around (modulo 27 -- note
# how we have 27 letters: 26 in the alphabet plus the space!):
# sum of plaintext and key with modulo:
#               = c(21, 5, 25,  3,  9, 17, 5, 26, 13, 14,  1,  6, 4, 20,  1,  5, 24, 13, .....
# converted back to letters:
#               = UEYCIQEZMNAFDTAEXMZLXNRO USAVHQENBRLPRF UYMHVQESFBS
#
# A few more examples:
# plaintext: COME LETS EAT GRANDPA
# key:       ABC
# result:    DQPFBOFVVAGDUBJSCQERD
# plaintext: I LIKE COOKING MY FRIENDS AND MY FAMILY
# key:       " " (space)
# result:    I LIKE COOKING MY FRIENDS AND MY FAMILY
#            (no encryption because " " corresponds to 0, so values are not changed)
# Implement a function that performs Vignere encryption or decryption, taking one
# `plaintext` and one `key` parameter. Both are Strings.
# Input:
# - `plaintext`: `character(1)`
# - `key`: `character(1)`
# - `decrypt`: `logical(1)` with *default value* `FALSE`.
# You can assume both only consist of uppercase letters and the space character.
# Also your function should take a logical `decrypt` argument. If `decrypt` is TRUE,
# then decryption should be performed instead of encryption (subtraction instead of addition).
# Default should be FALSE.
# You may find the `match()` function and the modulo operator %% useful.
#
# Be aware that this cipher is very insecure and you should not use it to actually hide information
# (see 03_exercise_cryptoanalysis.R!).
# You can read more about the cipher at Wikipedia: <https://en.wikipedia.org/wiki/Vigen%C3%A8re_cipher>
ex02VignereCipher <- function(plaintext, key, decrypt = FALSE) {
  # your code
  assertString(plaintext)
  assertString(key)
  assertLogical(decrypt)

  codebook <- 0:26
  names(codebook) <- c(" ", LETTERS)
  plaintext <- strsplit(plaintext, NULL)[[1]]
  key <- strsplit(key, NULL)[[1]]
  key <- head(rep(key, ceiling(length(plaintext) / length(key))), length(plaintext))

  if (decrypt) {
    code <- codebook[plaintext] - codebook[key]
  } else {
    code <- codebook[plaintext] + codebook[key]
  }

  code <- code %% 27

  paste(names(codebook)[code + 1], collapse = "")
}
