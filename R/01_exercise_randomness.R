
# Write a function that simulates a "broken" coin:
# With probability 0.7, the coin lands on "heads".
# With probability 0.2, the coin lands on "tails".
# With probability 0.1, the coin lands on its "edge".
#
# Input: No input.
# Returns: a `character(1)` one of "heads", "tails", or "edge", with the
#   probabilities listed above.
ex01BrokenCoin <- function() {
  # your code
  sample(c("heads", "tails", "edge"), size = 1, prob = c(0.7, 0.2, 0.1))
}


# In the game of Roulette (https://en.wikipedia.org/wiki/Roulette), players may
# take bets on what number(s) will come up when a roulette-wheel, containing the
# numbers 0, 1, ... 36, is spun.  Players with successful bets are paid out a
# certain amount of payout, players with unsuccessful bets lose their stake.
#
# One kind of bet players can take is betting on "even" or "odd" numbers: These
# win, if the resulting number is a positive even / odd number -- coming on 0
# loses both these bets. Payout for these bets is 1 to 1, i.e. the player
# doubles his stake if successful and loses his stake if not.
#
# A "Martingale" betting strategy works as follows: A player bets a certain
# amount, e.g. EUR 1, on "even".  If the bet is successful (number 2, 4, 6, ..
# 36 comes up), the player quits, having made EUR 1 of profit.  If the bet is
# unsuccessful (number 0, 1, 3, 5, 7, .. 35 comes up), the player plays again,
# betting EUR 2 this time.  Winning the second bet would give the player EUR 2,
# covering his loss of EUR 1 before, earning him EUR 1 of profit.  Whenever the
# player loses, he doubles his stake and bets again. As soon as the player wins,
# he quits, having made EUR 1 of profit overall.  Should the player not have
# enough money to make a bet following this rule, he also quits.
#
# Write a function that simulates the martingale strategy.
# Input:
# - `bankroll`: Non-negative integer `numeric(1)` indicating the initial amount
#   of money the player has.
# - `rounds.max`: Non-negative integer `numeric(1)`, indicating the number of
#   rounds after which the player should quit playing, even if he has not won a
#   bet yet but would have enough money to continue following the strategy.
# Returns: An integer `numeric(1)` indicating the amount of money owned by the
#   player after he quits.
#
# Your function should simulate the martingale strategy by repeatedly drawing an
# integer number between 0 and 36, determining the amount of money owned by the
# player afterwards, and deciding whether the player would continue.
#
# Example Invocations:
# > ex01Martingale(100, 0)
# #> [1] 100               # quit immediately
# > ex01Martingale(100, 1)
# #> [1] 101
#  -- or --
# #> [1] 99
# (win with probability 18 / 37, otherwise lose)
# > ex01Martingale(1, 1000)
# #> [1] 2
#  -- or --
# #> [1] 0
# > ex01Martingale(10000, 1000)
# #> [1] 10001
# (with very high probability. 1809 is also possible, but rare.)
#
# Sidenote: The Martingale strategy is interesting because it looks like an easy
# way to make money, since the probability of going bancrupt can get arbitrarily
# small if one has enough money to start with. However, the expected value of
# the profit from the martingale strategy is negative, since the loss in case of
# bancruptcy is then very large.
ex02Martingale <- function(bankroll, rounds.max) {
  # your code
  assertCount(bankroll)
  assertCount(rounds.max)

  betsize <- 1

  for (i in seq_len(rounds.max)) {
    if (betsize > bankroll) return(bankroll)
    wheel <- sample(0:36, 1)  # 0, ..., 36
    if (wheel > 0 && wheel %% 2 == 0) {
      # won the "even" bet: even number > 0 --> get winnings and end.
      bankroll <- bankroll + betsize
      return(bankroll)
    }
    # lost the "even" bet --> lose money, increase bet size
    bankroll <- bankroll - betsize

    betsize <- betsize * 2
  }
  bankroll
}
