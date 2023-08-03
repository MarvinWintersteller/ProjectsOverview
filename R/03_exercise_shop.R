# This exercise concerns itself with data in a certain format.
#
# A friend asked you to help him with his small corner store for various
# items. Because his shop is both a physical store where customers can
# take items, as well as an online shop that ships items, he has separate
# prices for items with and without delivery to reflect the different
# realities of the two markets.
# Your friend changes his inventory and prices monthly, and keeps a record of all
# sales made in that month. He wants to have a few functions that summarize
# the sales and revenue made in a month.
# His price-list could, for example, look like the following:
itemshop.prices <- rbindlist(list(
    list(item = NULL,             price.onsite = NULL, price.online = NULL),
    list("Healing Potion",        9.99,                12.99),
    list("Staff of Illusion",     18.95,               20.00),
    list("Lesser Stone of Mana",  2.60,                4.00),
    list("Greater Stone of Mana", 7.50,                9.99),
    list("Sword of Clarity +2",   21.50,               22.99)
))
# (the columns are constant, but actual datasets may have more or fewer rows
# with different items).
# Furthermore, your friend keeps a record of items sold in a table.
# The sales record has the following format:
itemshop.sales <- rbindlist(list(
    list(item = NULL,             channel = NULL),
    list("Healing Potion",        "online"),
    list("Sword of Clarity +2",   "onsite"),
    list("Sword of Clarity +2",   "online"),
    list("Sword of Clarity +2",   "onsite"),
    list("Greater Stone of Mana", "onsite")
))
# (Again, actual datasets may have more or fewer rows.)
#
# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.


# Write a function that counts the number of items sold for each item type.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item` and `count`. Items
# with no sales should appear with a count of 0. The items
# should be in the same order as they appear in the `prices` table. The
# output with the two example datasets would be
itemshop.salescount <- rbindlist(list(
    list(item = NULL,             count = NULL),
    list("Healing Potion",        1),
    list("Staff of Illusion",     0),
    list("Lesser Stone of Mana",  0),
    list("Greater Stone of Mana", 1),
    list("Sword of Clarity +2",   3)
))
# You can use aggregation with `[... by ...]` here, and the special value `.N`
# may be useful. To get the same order as the `prices` table, a join could
# be useful.
ex01CountSales <- function(prices, sales) {
  # your code
  assertDataTable(prices)
  assertDataTable(sales)
  sales[, .(count = .N), keyby = "item"][prices$item, .(item, count = nafill(count, fill = 0))]
}

# Write a function that counts the number of items sold for each type, and
# for each sales channel.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output should be a `data.table` with columns `item`, `count.onsite`, and
# `count.online`. Items with no sales should appear with a count of 0.
# The items should be in the same order as they appear in the `prices` table.
# The output with the two example datasets would be
itemshop.saleschannel.count <- rbindlist(list(
    list(item = NULL,             count.onsite = NULL, count.online = NULL),
    list("Healing Potion",        0,                   1),
    list("Staff of Illusion",     0,                   0),
    list("Lesser Stone of Mana",  0,                   0),
    list("Greater Stone of Mana", 1,                   0),
    list("Sword of Clarity +2",   2,                   1)
))
# This can be solved with aggregation similar to ex01CountSales, but `dcast()` (followed by a join) also works.
ex02CountSalesChannels <- function(prices, sales) {
  # your code
  assertDataTable(prices)
  assertDataTable(sales)
  sales[, channel := paste0("count.", channel)]
  res <- dcast(sales, item ~ channel, fun.aggregate = length)[prices$item]
  res <- rbind(data.table(item = character(0), count.onsite = numeric(0), count.online = numeric(0)), res, fill = TRUE)
  setnafill(res, fill = 0, cols = 2:3)
}
# SOLUTION
# alternatively:
ex02CountSalesChannels <- function(prices, sales) {
  assertDataTable(prices)
  assertDataTable(sales)
  linesales <- sales[channel == "online", .(count.online = .N), keyb = "item"]
  sitesales <- sales[channel == "onsite", .(count.onsite = .N), keyby = "item"]
  res <- sitesales[linesales[prices$item]]
  setnafill(res, fill = 0, cols = c("count.online", "count.onsite"))
}

# Write a function that calculates the total revenue received by the shop.
# Inputs:
# - `prices`: a `data.table` in the format of the `itemshop.prices` example
# - `sales`: a `data.table` in the format of the `itemshop.sales` example
# Output: a `numeric(1)`, summing up the total revenue as given by the price
# of each sold item for the respective channel.
# For the example dataset, the result would be `86.48`.
ex03Revenue <- function(prices, sales) {
  # your code
  assertDataTable(prices)
  assertDataTable(sales)
  sum(prices[sales, ifelse(channel == "online", price.online, price.onsite), on = "item"])
}
