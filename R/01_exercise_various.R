
# Write a function that `cbind`s two data.tables, avoiding duplicate column
# names in a certain way.
# Input:
# - table.a: `data.table`
# - table.b: `data.table`
# Output: Return a `data.table` that contains all the columns of `table.a`,
# followed by the columns of `table.b`, in order. However, should a column
# name occur in `table.b` that is already in `table.a`, then the column
# should be suffixed by `_copy`. If that *also* leads to a name collision,
# it should be suffixed instead by `_copy.1` (or `_copy.2` etc.)
# You can rely on `table.a` and `table.b` for themselves having unique column
# names, but some of their column names may *already* end on `_copy` or
# `_copy.#`.
#
# If `table.a`. and `table.b` have a different number of rows, an error
# should be thrown.
# Example input:
table.a.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4)
table.b.example <- data.table(z = 100, x = 200, y = 300, y_copy.1 = 400, y_copy.2 = 500)
# This should give the output
table.ab.example <- data.table(x = 1, y = 2, y_copy = 3, y_copy.1 = 4,
  z = 100, x_copy = 200, y_copy.3 = 300, y_copy.1_copy = 400, y_copy.2 = 500)
# (note that `y` gets renamed to `y_copy.3` because `_copy`, `_copy.1` and
# `_copy.2` are already taken; `y_copy.1` of `table.b` turns into
# `y_copy.1_copy`, because `y_copy.1` is already present in `table.a`.)
ex01CbindNameClash <- function(table.a, table.b) {
  # your code
  assertDataTable(table.a)
  assertDataTable(table.b)
  if (nrow(table.a) != nrow(table.b)) {
    stop("Different number of rows")
  }

  allnames <- union(colnames(table.a), colnames(table.b))

  renames <- vapply(colnames(table.b), function(name) {
    if (!name %in% colnames(table.a)) {
      return(name)
    }
    basename <- name <- paste0(name, "_copy")
    counter <- 1
    while (name %in% allnames) {
      name <- paste0(basename, ".", counter)
      counter <- counter + 1
    }
    name
  }, character(1))

  colnames(table.b) <- renames  # could also use setnames() here
  cbind(table.a, table.b)
}

# Write a function that removes duplicate entries according to some columns.
# Input:
# - `table`: a `data.table` with the columns `year`, `month`, `day`, and
#   arbitrarily many more columns additionally to that.
# Output: The input `data.table` where duplicate entries according to the
# `year`, `month`, `day` columns are removed. I.e. for each such indicated date,
# the resulting table should only contain the *last* line with that date.
# The lines that are not removed should remain in order.
dup.table <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
    list(2009,        4,            13,         "ae8f43b4b8",     6054),
    list(2009,        4,            14,         "e0e57942dd",     3453),
    list(2009,        4,            13,         "d63a61d9fc",     1470)
))
# Here the first line should be skipped because the third line has an identical
# date:
deduped.table <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, reference = NULL, id = NULL),
    list(2009,        4,            14,         "e0e57942dd",     3453),
    list(2009,        4,            13,         "d63a61d9fc",     1470)
))

ex02DedupTable <- function(table) {
  # your code
  assertDataTable(table)
  unique(table, by = c("year", "month", "day"), fromLast = TRUE)
}


# You are looking for a new flat. For this, you query a database for monthly
# rent prices of available flats, which returns data in the following format:
flat.prices <- rbindlist(list(
    list(address = NULL, prices = NULL),
    list("134 Charles St", list(list(c(2340, 2193), NULL, 4023, NULL, NULL, c(10234, 9203)))),
    list("12 East 69th St", list(list(2493, NULL, NULL, NULL))),
    list("2 Park Pl", list(list(NULL, NULL, 1924, 3921))),
    list("172 Madison Ave", list(list(NULL, NULL))),
    list("25 Columbus Circle", list(list(10234)))
))
# This lists the adresses where the agency manages flats, and for each address
# the column `prices` lists the flat prices. This column contains a list for
# each address, where the first entry lists all prices of all available flats
# on the first floor, the second entry lists all prices of available flats on
# the second floor etc. In this example, the `"134 Charles St"` building has
# two flats available on the ground floor (one for 2340, one for 2193), one flat
# for 4023 on the second floor, and two flats (10234 and 9203) on the fifth
# floor.
#
# You plan to look at all available addresses individually, but within each
# address you only consider the *cheapest flat that is not on the ground floor*.
# You therefore need to write a function that lists the address, the price of
# the cheapest apartment that is not on the ground floor, and the floor of that
# aparment. If there are no apartments available that are not on the ground
# floor, the address should be absent. The result for the input above should
# therefore be
flat.choices <- rbindlist(list(
    list(address = NULL, price = NULL, floor = NULL),
    list("134 Charles St", 4023, 2),
    list("2 Park Pl", 1924, 2)
))
# Input:
# - `prices`: a `data.table` with columns `address`, `prices`, as described
#   above
# Output:
# A `data.table with columns `address`, `price`, `floor`, as shown above,
# with arbitrary order of rows.
ex03FlatPrices <- function(prices) {
  # your code
  assertDataTable(prices)
  # add column "floormins", containing the minimum price for each floor
  suppressWarnings(prices[, floormins := lapply(prices, function(x) vapply(x, min, numeric(1)))])
  # generate the new columns from `min(floormins[-1])` and `which.min(floormins[-1])`
  prices <- suppressWarnings(prices[, .(
    address,
    price = vapply(floormins, function(x) min(x[-1]), numeric(1)),
    floor = vapply(floormins, function(x) which.min(c(x[-1], Inf)), numeric(1)))])
  # select the rows where there actually was anything in `floormins[-1]`, i.e. where
  # there are flats available in the not-ground-floor. ==> `price` must be finite
  prices[is.finite(price)]
}



# You are organising a small hackathon for R developers working on a specific
# project. Everyone should come to a place over the weekend to code. You want
# to order food and drinks for everyone, as well as reserve a hotel for those
# who don't live nearby or sleep over at friends' places. For this you have
# set up a Google Forms Survey, where those interested in coming should answer
# questions, for example about their dietary needs and whether they need a
# hotel. However, some participants may forget to input some information, or
# may change their mind about certain things later. In that case, they just
# submit a new response in the survey, updating the old information. The data
# you get could, for example, have the following format:
participants.response <- rbindlist(list(
    list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL, submission.date = NULL),
    list("Donald Knuth",     TRUE,          TRUE,         "dessert first",     3),
    list("Ross Ihaka",       NA,            TRUE,         NA,                  6),
    list("Ross Ihaka",       TRUE,          FALSE,        NA,                  4),
    list("Vladimir Vapnik",  TRUE,          FALSE,        "",                  5),
    list("Donald Knuth",     FALSE,         NA,           NA,                  5),
    list("Robert Gentleman", TRUE,          TRUE,         NA,                  3),
    list("Ross Ihaka",       NA,            NA,           "vegetarian",        5)
))
# Here the `submission.date` column indicates the number of days since the form
# was set up. In this example, Donald Knuth at first signed up to come but then
# later had to cancel. Ross Ihaka at first did not provide information
# about his dietary requirements, which he then set to `"vegetarian"`. He
# furthermore updated that he did want to stay at a hotel after all.
# Robert Gentleman and Vladimir Vapnik both indicated they would be coming and
# provided information, although Robert Gentleman's dietary requirements stay
# unknown.
# (*This data is made up. Please do not use the dietary requirement data if you
# are actually hosting someone from this list. Please email me if you happen
# to know the dietary requirements of someone on this list)
#
# Write a function that takes data of this form and constructs the final
# response for each participant, by taking the last value, according to the
# submission date that is not missing.
# Input: `response`, a `data.table` with the columns `name`, `submission.date`,
# and multiple other columns of any type. The order of the columns, as well as
# the other columns present could be arbitrary.
# Return: The result `data.table` should contain all columns except the
# `submission.date` column, and the columns should be in the order they were
# given. There should be one row for each participant (according to `name`)
# with the most recently given response for each other column.
# The response for the data above could, for example, be:
participants.response.final <- rbindlist(list(
    list(name = NULL,        coming = NULL, hotel = NULL, dietary.reqs = NULL),
    list("Donald Knuth",     FALSE,         TRUE,         "dessert first"),
    list("Ross Ihaka",       TRUE,          TRUE,         "vegetarian"),
    list("Vladimir Vapnik",  TRUE,          FALSE,        ""),
    list("Robert Gentleman", TRUE,          TRUE,         NA)
))
ex04LastResponse <- function(response) {
  # your code
  assertDataTable(response)
  response[order(submission.date),
    lapply(.SD, function(x) last(na.omit(x))), by = name][,
      submission.date := NULL][]
}

# You are given data about the organizational hierarchy of a small company. It
# tracks, for every employee, who their supervisor / immediate superior is.
# An example of this data would be
org.table <- rbindlist(list(
    list(name = NULL,               superior = NULL),
    list("Gottfried Ofers",         ""),
    list("Kunigunde von Lauerberg", "Gottfried Ofers"),
    list("Agata Weyen",             "Kaspar Vinken"),
    list("Kaspar Vinken",           "Gottfried Ofers")
))
# (The CEO has his superior listed as `""`)
# The company needs to track which employee is supervising other employees for
# accounting purposes.
# Write a function that adds a `logical` column `has.inferior` which is `TRUE`
# for every employee that has inferiors.
# Input: `org`, a `data.table` as in the format above
# Output: The `data.table` with an additional column `has.inferior`. The example
# above should return
org.table.augmented <- rbindlist(list(
    list(name = NULL,               superior = NULL,   has.inferior = NULL),
    list("Gottfried Ofers",         "",                TRUE),
    list("Kunigunde von Lauerberg", "Gottfried Ofers", FALSE),
    list("Kaspar Vinken",           "Gottfried Ofers", TRUE),
    list("Agata Weyen",             "Kaspar Vinken",   FALSE)
))
ex05OrgHasInf <- function(org) {
  # your code
  assertDataTable(org)
  org[, has.inferior := name %in% org$superior][]
}


# You are analysing the hold times of a national package delivery company.
#
# Customers deliver packages to one of many storage warehouses, where they are
# accepted and processed. A fleet of transport vehicles travels between the
# warehouses every day to bring packages either to the final recipient, or to
# other storage warehouses, should the recipient be too far away. Packages that
# are received by a storage warehouse are kept there at least until the next
# morning, but because of limited fleet capacity or other circumstances they may
# remain there for longer.
# You are given the record of package deliveries between customers (identified
# as `"CUSTOMER"`), storage warehouses (identified by unique string ID), and
# recipients (identified as `"RECIPIENT"`). Every package has a shipment ID,
# and the data of the transaction is recorded as the number of days since
# 1970-01-01. An example record could look like the following:
package.transactions <- rbindlist(list(
    list(shipment.id = NULL, from.id = NULL, to.id = NULL, date = NULL),
    list("2458B9",           "CUSTOMER",     "xknq",       17571),
    list("2458B9",           "xknq",         "hlcf",       17572),
    list("587FB8",           "CUSTOMER",     "hlcf",       17573),
    list("2458B9",           "hlcf",         "RECIPIENT",  17574),
    list("3B444F",           "CUSTOMER",     "hlcf",       17574),
    list("587FB8",           "hlcf",         "xknq",       17575),
    list("587FB8",           "xknq",         "hlcf",       17576),
    list("587FB8",           "hlcf",         "RECIPIENT",  17577),
    list("3B444F",           "hlcf",         "RECIPIENT",  17577)
))
# In this example, the "2458B9" package was received by "xknq" (first entry),
# forwarded to "hlcf" after one day (second entry), and delivered to the
# recipient after two more days (fourth entry).
# The "587FB8" package was received by "hlcf" (third line), forwarded to
# "xknq" after two days (6th line) and returned back to "hlcf" after one more
# day (7th line), possibly because the original recipient cancelled their order.
# The package was then finally delivered to a recipient from "hlcf" (8th line).
#
# Note that, for real data, many deliveries for many storage warehouses are
# conducted on the same day, but that each package always stays at a warehouse
# for at least one day. Some packages may go through some storage houses
# multiple times (since customers still have the option to update shipping
# information after giving up their package). The data will always be given
# sorted by the `date` column.
#
# Write a function that takes the record of package shipments as presented and
# returns a table of average hold times for each warehouse.
# Input: `transactions`, a `data.table` as shown above.
# Return: a `data.table`, with columns `warehouse` (the warehouse ID) and
# `mean.hold.time`, the mean hold time, in days. A return value for the data
# presented above could, for example, be:
package.holdtimes <- rbindlist(list(
    list(warehouse = NULL, mean.hold.time = NULL),
    list("hlcf",           2),
    list("xknq",           1)
))
# (This is because packages "2458B9" and "587FB8" both spend one day at "xknq".
# Package "2458B9" is at "hlcf" for 2 days, package "3B444F" is at "hlcf" for 3
# days, and "587FB8" is at "hlcf" twice: once for 2 days, then again for 1 day,
# making for a mean handling time of 2).
#
# As an intermediate result, you probably want to have a table that lists for
# each transaction "arrival" record / date the corresponding "departure" / date,
# from which you can then calculate the hold time and then do aggregation.
# One possible way to do this is to use a rolling join (on the table itself):
# https://www.gormanalysis.com/blog/r-data-table-rolling-joins/
# You can also have a look at 02_exercise_bank.R:ex03Balance first, which
# does rolling joins in a similar (but maybe more intuitive) way.
ex06PackageHoldTimes <- function(transactions) {
  # your code
  assertDataTable(transactions)
  # We do a rolling self-join!
  # For a transaction from A to B, we know the exit date of A. But we
  # need to find out when the package arrived at A. A first possible merge
  # would be:
  # transactions[transactions, .(x.date, i.from.id, i.date, i.to.id), on = .(to.id = from.id, shipment.id)]
  # This merges the tables "x" (the outer table) and "i" (the inner table) when the outer `to.id` equals the
  # inner `from.id` (i.e. `x.to.id == i.from.id`), and when furthermore the shipment ID is the same.
  # Now the "arrival" date at the `x.to.id` resp. `i.from.id` shipment station is `x.date`, and the departure
  # date is `i.date`.
  # The problem here is that a shipment could cross the same station multiple times, and as it stands the merge
  # would not know which arrival at a station to match with which departure (and in fact just matches all of them).
  # Therefore we also have to match on `date`, and do a (forward) rolling merge: the (outer) `x.date` matches to
  # the closest (inner) `i.date` that comes afterwards.
  # We furthermore set `nomatch = 0` so that rows of the inner `transactions` have no corresponding match are
  # removed (otherwise they would be present with NA values for the `x.***` columns).
  #
  # transactions[transactions, .(x.date, x.to.id, i.date, i.to.id),
  #  on = .(to.id = from.id, shipment.id, date), roll = TRUE, nomatch = 0]
  #
  # this gives us a table that has all the correct info to calculate the holding time for each transaction.
  # We get one line per transaction that goes from `x.to.id` (where the package arrived on `x.date`) to
  # `i.to.id` and is performed on date `i.date`. We therefore had a holding time of `i.date - x.date` at warehouse
  # `x.to.id`. We can put these calculations into the `.( ... )` directly:
  holdtimes <- transactions[transactions, .(warehouse = x.to.id, hold.time = i.date - x.date),
     on = .(to.id = from.id, shipment.id, date), roll = TRUE, nomatch = 0]
  # we now do aggregation by warehouse id and mean the hold time:
  holdtimes[, .(mean.hold.time = mean(hold.time)), by = warehouse]
}
