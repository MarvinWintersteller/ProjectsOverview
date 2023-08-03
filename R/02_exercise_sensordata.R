# This exercise concerns itself with data in a certain format.
#
# Setting: You are a data science consultant, hired to help the WidgetCorp (R) corporation
# analyse its production facilities for Widgets (TM). WidgetCorp (R) has a number of
# Widget (TM) production machines that are lovingly called Machine01, Machine02, Machine03, etc.
# Each of these machines has the same set of Sensors, measuring temperature, humidity,
# electricity consumption, water inflow rate, water purity, and various other metrics.
# Because the particulars of the quantities being measured are not relevant, these sensors
# are just enumerated as sensor01, sensor02, sensor03, etc.
# You are given a dataset that records, for each produced Widget (TM), (1) the Machine that
# produced it, (2) the quality of the Widget (TM), on a scale from 0 to 100, as assessed by
# certified Widget (TM) Quality Assessment Professionals, and (3) the values of the various
# sensor readings when the Widget (TM) was produced.
# However, the sensors have a limited measurement range that sometimes gets undercut. E.g.
# if the electricity consumption is below a certain threshold, the sensor is not able to
# record that value and just records NA.
# Example datasets could look like the following.
# (Note the `rbindlist`-layout is only for readability here. You can run the following code and
# will get a proper `data.table` from it.)
widget.corp.data <- rbindlist(list(
    list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
    list("Machine01",    78,             23,              28.6,            -23,             NA),
    list("Machine02",    28,             41,              77.8,            NA,              27),
    list("Machine03",    32,             57,              91.6,            -29,             10),
    list("Machine03",    80,             NA,              32.3,            NA,              NA),
    list("Machine03",    58,             10,              77.8,            3,               NA),
    list("Machine02",    74,             NA,              24.5,            -18,             3),
    list("Machine01",    46,             81,              NA,              NA,              NA),
    list("Machine01",    24,             43,              13.3,            -22,             NA),
    list("Machine02",    7,              96,              96.0,            0,               NA),
    list("Machine01",    22,             107,             23.5,            7,               8),
    list("Machine03",    98,             NA,              NA,              11,              NA)
))
# In the following tasks you should write functions that handle data just like this. However, you may get
# a dataset with more or fewer machines, and with more or fewer sensors.

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.

# Hint:
# You will probably write some functions that modify the input data in-place. This means that calling
# one of your functions may actually cause the input value to be different after the call. Be aware of this
# if you try out your functions on the `widget.corp.data` example data. E.g. if you do
# > result <- ex04CleanTable(widget.corp.data)
# then `widget.corp.data` itself may have changed and could give different results in future experiments.
# You should therefore always run your functions on a `copy` of the input data, like so:
# > result <- ex04CleanTable(copy(widget.corp.data))
# Alternatively, you can just execute the above code-snippet (or source this .R-file) again after
# each experiment.

# ** Warm up **
# Write a function that accepts one `data.table` argument `data` and sorts the given data according to
# the number of missing values in the `sensorXX` columns, in descending order. Ties should be broken
# by the `quality`, descending. Your function should return the `data.table` with rows sorted.
# The output for the example data would therefore be:
widget.corp.data.sorted <- rbindlist(list(
    list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
    list("Machine03",    98,             NA,              NA,              11,              NA),
    list("Machine03",    80,             NA,              32.3,            NA,              NA),
    list("Machine01",    46,             81,              NA,              NA,              NA),
    list("Machine01",    78,             23,              28.6,            -23,             NA),
    list("Machine02",    74,             NA,              24.5,            -18,             3),
    list("Machine03",    58,             10,              77.8,            3,               NA),
    list("Machine02",    28,             41,              77.8,            NA,              27),
    list("Machine01",    24,             43,              13.3,            -22,             NA),
    list("Machine02",    7,              96,              96.0,            0,               NA),
    list("Machine03",    32,             57,              91.6,            -29,             10),
    list("Machine01",    22,             107,             23.5,            7,               8)
))
ex01SortTable <- function(data) {
  # your code
  assertDataTable(data)
  missings <- data[, rowSums(is.na(.SD)), .SDcols = grep("^sensor", colnames(data), value = TRUE)]
  data[order(missings, quality, decreasing = TRUE)]
}

# ** Warm up **
# Write a function that accepts one `data.table` argument `data` and collects all the sensor data into
# a single list column. This means, the resulting `data.table` should have columns `machine`, `quality`,
# `sensor`, and the `sensor` column should be a `list` column containing a named `numeric` vector
# containing all the non-`NA` sensor values with appropriate names. The order of the table should not be changed.
# The output for the example data would therefore be:
widget.corp.data.list <- rbindlist(list(
    list(machine = NULL, quality = NULL, sensor = NULL),
    list("Machine01",    78,             list(c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23))),
    list("Machine02",    28,             list(c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27))),
    list("Machine03",    32,             list(c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10))),
    list("Machine03",    80,             list(c(sensor02 = 32.3))),
    list("Machine03",    58,             list(c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3))),
    list("Machine02",    74,             list(c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3))),
    list("Machine01",    46,             list(c(sensor01 = 81))),
    list("Machine01",    24,             list(c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22))),
    list("Machine02",    7,              list(c(sensor01 = 96, sensor02 = 96.0, sensor03 = 0))),
    list("Machine01",    22,             list(c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8))),
    list("Machine03",    98,             list(c(sensor03 = 11)))
))
# (Just to make clear what a `list` column is -- another way to write this would be the following:)
widget.corp.data.list <- data.table(
  machine = c("Machine01", "Machine02", "Machine03", "Machine03", "Machine03", "Machine02", "Machine01",
    "Machine01", "Machine02", "Machine01", "Machine03"),
  quality = c(78, 28, 32, 80, 58, 74, 46, 24, 7, 22, 98),
  sensor = list(
    c(sensor01 = 23, sensor02 = 28.6, sensor03 = -23),
    c(sensor01 = 41, sensor02 = 77.8, sensor04 = 27),
    c(sensor01 = 57, sensor02 = 91.6, sensor03 = -29, sensor04 = 10),
    c(sensor02 = 32.3),
    c(sensor01 = 10, sensor02 = 77.8, sensor03 = 3),
    c(sensor02 = 24.5, sensor03 = -18, sensor04 = 3),
    c(sensor01 = 81),
    c(sensor01 = 43, sensor02 = 13.3, sensor03 = -22),
    c(sensor01 = 96, sensor02 = 96, sensor03 = 0),
    c(sensor01 = 107, sensor02 = 23.5, sensor03 = 7, sensor04 = 8),
    c(sensor03 = 11)
  )
)
# Be aware that some rows may not have any non-missing sensor data, in which case the `sensor` value for the
# corresponding result row should be an empty `numeric`.
ex02ListTable <- function(data) {
  # your code
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  data[, sensor := list(list(c(na.omit(unlist(.SD))))), .SDcols = sensorcols, by = seq_len(nrow(data))]
  data[, c("machine", "quality", "sensor")]
}

# ** Actual analysis **
# You want to analyse the effect of measured sensor data on the quality of widgets, for each machine
# separately. However, you are worried that too much missing data will bring on misleading results.
# Therefore, you plan to remove columns with too much missing data.
#
# Write a function that removes all `sensorXX` columns which have
# 50% or more missing data for at least one machine. Your function should accept one `data.table` argument
# `data` and return the modified `data.table`. For the example `widget.corp.data` dataset,
# that would be `sensor01` (50% missing for "Machine03") and `sensor04` (75% missing for
# both "Machine01" and "Machine03"). The resulting dataset would therefore be
widget.corp.data.fsel <- rbindlist(list(
    list(machine = NULL, quality = NULL, sensor02 = NULL, sensor03 = NULL),
    list("Machine01",    78,             28.6,            -23),
    list("Machine02",    28,             77.8,            NA),
    list("Machine03",    32,             91.6,            -29),
    list("Machine03",    80,             32.3,            NA),
    list("Machine03",    58,             77.8,            3),
    list("Machine02",    74,             24.5,            -18),
    list("Machine01",    46,             NA,              NA),
    list("Machine01",    24,             13.3,            -22),
    list("Machine02",    7,              96.0,            0),
    list("Machine01",    22,             23.5,            7),
    list("Machine03",    98,             NA,              11)
))
ex03CleanTable <- function(data) {
  # your code
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  sensor.nas <- data[, as.list(colMeans(is.na(.SD))), .SDcols = sensorcols, by = machine][, machine := NULL]
  keep <- names(sensor.nas)[vapply(sensor.nas, max, numeric(1)) < 0.5]
  data[, c("machine", "quality", keep), with = FALSE]
}

# ** Actual analysis II **
# Even after removing columns with 50% or more missing data for any machine, you are still left with
# a dataset with missing values. You don't want to remove all columns with missing data (which would,
# in most cases, leave you with no data at all), so instead you choose to "impute" missing values.
# You know that values are missing because they are below the detection threshold of a sensor.
# You don't know the detection thesholds, but you know that for each sensor they are the same on
# different machines. Therefore, you *estimate* the threshold as the *minimum non-missing value* for each sensor.
#
# Write a function that imputes all missing values for sensor columns as the minimum of that column.
# The function takes one `data.table` argument `data` and should return the modified `data.table`.
# You can rely on at least one non-missing value being present in each sensor-column. The result
# for the `widget.corp.data` example data would be
widget.corp.data.imputed <- rbindlist(list(
    list(machine = NULL, quality = NULL, sensor01 = NULL, sensor02 = NULL, sensor03 = NULL, sensor04 = NULL),
    list("Machine01",    78,             23,              28.6,            -23,             3),
    list("Machine02",    28,             41,              77.8,            -29,              27),
    list("Machine03",    32,             57,              91.6,            -29,             10),
    list("Machine03",    80,             10,              32.3,            -29,             3),
    list("Machine03",    58,             10,              77.8,            3,               3),
    list("Machine02",    74,             10,              24.5,            -18,             3),
    list("Machine01",    46,             81,              13.3,            -29,             3),
    list("Machine01",    24,             43,              13.3,            -22,             3),
    list("Machine02",    7,              96,              96.0,            0,               3),
    list("Machine01",    22,             107,             23.5,            7,               8),
    list("Machine03",    98,             10,              13.3,            11,              3)
))
ex04ImputeTable <- function(data) {
  # your code
  assertDataTable(data)
  sensorcols <- grep("^sensor[0-9]+", colnames(data), value = TRUE)
  data[, (sensorcols) := lapply(.SD, function(x) nafill(x, fill = min(x, na.rm = TRUE))), .SDcols = sensorcols][]
}

# ** Actual analysis **
# You now have functions that get your data in a state fit for modelling, so you want to fit the actual models.
#
# For your analysis, you want to fit "linear models" using the `lm()` function that comes with `R`. If you
# had just a `data.table` of sensor data for a single machine, for example
widget.corp.small.example <- data.table(
  quality = c(78, 46, 24, 22),
  sensor02 = c(28.6, 13.3, 13.3, 23.5),
  sensor03 = c(-23, -29, -22, 7))
# then the linear model would be the result of calling `lm(quality ~ ., data = widget.corp.small.example)`.
# What is happening here is that the `quality ~ .` formula tells `lm()` to model the `quality`-column against
# all other values.
# You want to fit a linear model for each machine, and put the resulting linear models into a `data.table` for
# further analysis.
#
# Write a function that takes a `data.table` argument `data`. The function should use `ex03CleanTable()`
# to remove columns with at least 50% missing values, and then `ex04ImputeTable()`  to impute missing values
# in all remaining columns (This way you won't be fitting models on columns where most data is actually just
# imputed).
# The return value should be a `data.table` with columns `machine` (a `character` column, enumerating the machines
# in any order), and `model` (a `list` column, containing models). The models should be the result of `lm()`
# called on the data for each machine, as above.
# The result when called with the `widget.corp.data` example dataset could be
widget.corp.data.models <- data.table(
  machine = c("Machine01", "Machine02", "Machine03"),
  model = list(
    lm(quality ~ ., data.table(
      quality = c(78, 46, 24, 22),
      sensor02 = c(28.6, 13.3, 13.3, 23.5),
      sensor03 = c(-23, -29, -22, 7))),
    lm(quality ~ ., data.table(
      quality = c(28, 74, 7),
      sensor02 = c(77.8, 24.5, 96),
      sensor03 = c(-29, -18, 0))),
    lm(quality ~ ., data.table(
      quality = c(32, 80, 58, 98),
      sensor02 = c(91.6, 32.3, 77.8, 13.3),
      sensor03 = c(-29, -29, 3, 11)))
  )
)
# (but the ordering of rows does not matter)
ex05ModelTable <- function(data) {
  # your code
  assertDataTable(data)
  data <- ex04ImputeTable(ex03CleanTable(data))
  data[, .(model = list(lm(quality ~ ., .SD))), by = machine]
}
