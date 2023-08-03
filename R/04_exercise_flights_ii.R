
# This exercise concerns itself with the *same* flight data as
# `03_exercise_flights_i.R`. Read the first paragraph in that R-file for more
# information about the format. Otherwise, these exercises are independent from
# the exercises in `03_exercise_flights_i.R` and can be solved on their own.


# Write a function that returns the rows of maximum delay, for each route.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table`: a subset of the `flights` input data, containing the rows
# that are the flights with the highest arrival delay for each route, in any
# order. If there are multiple rows in a route with delay equal to the maximum
# of that route, any single of these rows should be returned. No rows should be
# returned for a connection that was not serviced in the data.
# An example result for the `flights.data` would be
flights.maxdelays <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, dep_delay = NULL, arr_delay = NULL, carrier = NULL, origin = NULL,
                                                                                              dest = NULL, hour = NULL),
    list(2014,        12,           28,         1165,             1187,             "AA",           "DEN",
                                                                                              "DFW",       13),
    list(2014,        8,            11,         883,              877,              "F9",           "DFW",
                                                                                              "DEN",       6),
    list(2014,        10,           20,         1529,             1524,             "AA",           "ATL",
                                                                                              "DFW",       15),
    list(2014,        6,            12,         855,              842,              "DL",           "DFW",
                                                                                              "ATL",       7),
    list(2014,        11,           12,         854,              890,              "AA",           "DEN",
                                                                                              "ORD",       18),
    list(2014,        6,            21,         435,              433,              "AA",           "ORD",
                                                                                              "DEN",       14),
    list(2014,        8,            1,          710,              712,              "AA",           "DFW",
                                                                                              "ORD",       22),
    list(2014,        6,            18,         429,              422,              "AA",           "ORD",
                                                                                              "DFW",       18),
    list(2014,        1,            3,          559,              558,              "DL",           "DEN",
                                                                                              "ATL",       9),
    list(2014,        8,            23,         508,              501,              "OO",           "ATL",
                                                                                              "ORD",       16),
    list(2014,        6,            18,         1092,             1107,             "DL",           "ORD",
                                                                                              "ATL",       19),
    list(2014,        1,            29,         814,              840,              "F9",           "ATL",
                                                                                              "DEN",       8)
))
# Note how every pair of airports occurs in both orders.
ex01MaxDelay <- function(flights) {
  # your code
  assertDataTable(flights)

  flights[flights[, .I[which.max(arr_delay)], by = c("origin", "dest")]$V1]
}


# Write a function that returns the rows of *median* delay.
# Input:
# - `flights`: `data.table` in the format of the example `flights.data` given
# Output:
# `data.table`: a subset of the `flights` input data, similar to `ex02MaxDelay`.
# However, the rows should be the rows with *median* (arrival) delay for each
# route, with the lower value selected if there is an even number of rows
# for that route. This corresponds to the `quantile(vect, .5, type = 1)`
# In case of ties, the *chronologically earliest* row
# should be returned. Note this exercise also considers 0-delay flight in the
# median.
# E.g. if a route had delays `c(0, 4, 2, 4, 2, 5)` in chronological order, then
# the `median()` would be `3`, but the lower end of the .5-quantile is 2, so the
# 3rd row is returned (tie breaking the two `2`s with earliest).
# If there is no entry for a route in the database, no row for that route should
# be returned.
# An example result for the `flights.data` would be
flights.mediandelays <- rbindlist(list(
    list(year = NULL, month = NULL, day = NULL, dep_delay = NULL, arr_delay = NULL, carrier = NULL, origin = NULL,
                                                                                              dest = NULL, hour = NULL),
    list(2014,        1,            1,          0,                0,                "DL",           "DEN",
                                                                                              "ATL",       1),
    list(2014,        1,            1,          4,                2,                "AA",           "DFW",
                                                                                              "ORD",       9),
    list(2014,        1,            1,          0,                0,                "AA",           "DEN",
                                                                                              "DFW",       6),
    list(2014,        1,            3,          16,               1,                "AA",           "ORD",
                                                                                              "DFW",       8),
    list(2014,        1,            3,          0,                0,                "DL",           "ORD",
                                                                                              "ATL",       6),
    list(2014,        1,            2,          0,                2,                "UA",           "DEN",
                                                                                              "ORD",       20),
    list(2014,        1,            1,          0,                0,                "AA",           "DFW",
                                                                                              "DEN",       7),
    list(2014,        1,            1,          0,                0,                "AA",           "ATL",
                                                                                              "DFW",       9),
    list(2014,        1,            1,          0,                0,                "DL",           "DFW",
                                                                                              "ATL",       7),
    list(2014,        1,            1,          0,                0,                "DL",           "ATL",
                                                                                              "ORD",       9),
    list(2014,        1,            5,          1,                3,                "UA",           "ORD",
                                                                                              "DEN",       12),
    list(2014,        1,            1,          0,                0,                "DL",           "ATL",
                                                                                              "DEN",       12)
))
# Note how many medians are 0 and because of the chronological tie breaking the
# flights are early in the dataset's range.
ex02MedianDelay <- function(flights) {
  # your code
  assertDataTable(flights)

  flights <- flights[order(year, month, day, hour)]
  flights[flights[, .I[arr_delay == quantile(arr_delay, .5, type = 1)][[1]],
    by = c("origin", "dest")]$V1]
}


# You are doing market research on the profitability of restaurants and shops at
# airport departure levels. You assume that the number of sales at a shop is
# proportional to the number of flights leaving from the airport, given that the
# shop is open at that time.
# Write a function that lists, for each month in the flights data in
# in chronological order, the number of flights that departed from a given
# airport during a given set of opening times.
# Input:
# - `flights`: a `data.table` in the format of the example `flights.data` given
# - `airport`: a `character(1)` indicating an airport of departure to consider.
# - `opening.times`: a `data.table` with columns `open`, `close`, with rows
#   indicating periods of time during which a given shop is open. Both columns
#   are integer numerics between 0 and 24 inclusive; a shop is open during
#   midnight if `close` is a number smaller or equal to `open`. An example input
#   for a shop that is open for flights leaving at 8, as well as for flights
#   that leave 14-17 and 20-3 (through midnight) would be (note that the upper
#   bounds are exclusive, since a shop that closes at 9:00 will not catch customers
#   from flights that leave at 9:<something>):
openings <- rbindlist(list(
    list(open = NULL, close = NULL),
    list(20,          5),
    list(14,          19),
    list(8,           9)
))
#   For this example, a flight that departed at `hour == 4` should be counted,
#   just as a flight that left at `hour == 8`, but a flight
#   leaving at `hour == 7` or `hour == 5` does not count.
#   You can rely on opening times never overlapping.
#   Ignore `dep_delay` (since we don't know whether the passengers spent the
#   delay time inside a plane, in a queue in the boarding area, etc.).
# Output:
# `data.table`: a table with columns `year`, `month`, `count`, indicating the
# number of flights that left the given `airport` during the `opening.times`
# at each month in the dataset.
#
# If `airport` is not among the values in the `flights` `origin` column, then
# naturally the number should be 0 for all months occurring in `flights`.
#
# The dataset contains both `hour == 0` and `hour == 24` flights. These should
# be treated the same; i.e. a flight leaving at `hour == 24` should count for
# a shop open with `open == 0`.
#
# The result for the example flights data, as well as the `openings` example
# above for the departure airport `DEN` could be:
shop.activity <- rbindlist(list(
    list(year = NULL, month = NULL, count = NULL),
    list(2014,        1,            437),
    list(2014,        10,           604),
    list(2014,        11,           548),
    list(2014,        12,           584),
    list(2014,        2,            395),
    list(2014,        3,            528),
    list(2014,        4,            460),
    list(2014,        5,            498),
    list(2014,        6,            508),
    list(2014,        7,            554),
    list(2014,        8,            540),
    list(2014,        9,            535)
))
#
# Hint: use the `inrange()` function (or `%inrange%` operator); You will have to
# modify the `opening.times` list to split times that go through midnight
# into two rows, i.e. `open == 20, close == 4` --> `open == 20, close == 24` and
# `open == 0, close == 4`.
ex03ShopActivity <- function(flights, airport, opening.times) {
  # your code
  assertDataTable(flights)
  assertDataTable(opening.times)
  assertString(airport)

  opening.times[open == 24, open := 0]
  opening.times[close == 0, close := 24]
  flights[hour == 24, hour := 0]
  opening.times <- rbind(opening.times[open < close],
    opening.times[open >= close, .(open, close = 24)],
    opening.times[open >= close, .(open = 0, close)])

  opening.times[, close := close - 1]

  flights[, .(count = sum(origin == airport & hour %inrange% opening.times)), by = c("year", "month")]
}
