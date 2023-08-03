# This exercise concerns itself with data in a certain format.
#
# You are advising the education board of a city on grading outcomes. They collect data
# on student's grades and need to put the data into different formats.
#
# The data they collect is a listing of students. For each student, they have a unique `student.id` (a random
# but unique character string which anonymizes the student's name), the student's `school`, and
# a `list` column `grades` containing named `numeric` elements indicating the grades which the student
# got during the last semester. The names are the subjects, and the values the grades ranging
# from 1 ("Very Good") to 6 ("Insufficient").
#
# An example dataset could look like the following. (See the notes in 02_exercise_sensordata.R
# on the format and other hints).
student.grade.data <- rbindlist(list(
    list(student.id = NULL, school = NULL,       grades = NULL),
    list("xzqk",            "North High School", list(c(maths = 2.0, physics = 1.3, chemistry = 1.7))),
    list("hffi",            "South Academy",     list(c(maths = 2.3, physics = 1.0, biology = 2.0, compsci = 1.0))),
    list("hwdp",            "South Academy",     list(c(maths = 3.3, chemistry = 3.0, biology = 2.7, compsci = 1.7))),
    list("flzb",            "North High School", list(c(maths = 1.7, physics = 2.0, chemistry = 2.0, biology = 2.0))),
    list("txyb",            "North High School", list(c(chemistry = 1.0, biology = 1.3))),
    list("fjea",            "South Academy",     list(c(chemistry = 2.0, physics = 2.3, compsci = 1.0)))
))

# All your functions should `assertDataTable` the input value, but do not need to make any further assertions
# regarding the format of `data.table` arguments here.


# To be able to analyze the data better you should start by bringing them into "long format".
# You want a table that has the columns `student.id`, `school`, `subject`, and `grade`,
# where there are multiple lines for each student (one for each subject in which he or she was
# graded).
#
# Write a function that brings the input data into "long format". The function should have
# an argument `data` (a `data.table`) and return a `data.table` in long format.
# A possible result of the data above would be:
student.grade.data.long <- rbindlist(list(
    list(student.id = NULL, school = NULL,       subject = NULL, grade = NULL),
    list("xzqk",            "North High School", "maths",        2.0),
    list("xzqk",            "North High School", "physics",      1.3),
    list("xzqk",            "North High School", "chemistry",    1.7),
    list("hffi",            "South Academy",     "maths",        2.3),
    list("hffi",            "South Academy",     "physics",      1.0),
    list("hffi",            "South Academy",     "biology",      2.0),
    list("hffi",            "South Academy",     "compsci",      1.0),
    list("hwdp",            "South Academy",     "maths",        3.3),
    list("hwdp",            "South Academy",     "chemistry",    3.0),
    list("hwdp",            "South Academy",     "biology",      2.7),
    list("hwdp",            "South Academy",     "compsci",      1.7),
    list("flzb",            "North High School", "maths",        1.7),
    list("flzb",            "North High School", "physics",      2.0),
    list("flzb",            "North High School", "chemistry",    2.0),
    list("flzb",            "North High School", "biology",      2.0),
    list("txyb",            "North High School", "chemistry",    1.0),
    list("txyb",            "North High School", "biology",      1.3),
    list("fjea",            "South Academy",     "chemistry",    2.0),
    list("fjea",            "South Academy",     "physics",      2.3),
    list("fjea",            "South Academy",     "compsci",      1.0)
))
# The ordering of rows does not matter.
ex01DataToLong <- function(data) {
  # your code
  assertDataTable(data)
  data[, .(school = school, subject = names(grades[[1]]), grade = grades[[1]]), by = student.id]
}
# ALTERNATIVE
# rbindlist in 2wide

# You want to present some statistics on the grade distribution.
# In particular you want to give the `mean()`, as well as the `sd()` of grades
# - within schools
# - within subjects
# - within both schools *and* subjects
#
# Write a function that accepts two arguments
# - `data`: a `data.table` in the format of `student.grade.data` at the top
# - `group.by`: a `character(1)` being either `"subject"`, `"school"`, or `"both"`.
# The return value should be a `data.table` with the columns
# - `school` (if `group.by` is either `"school"` or `"both"`)
# - `subject` (if `group.by` is either `"subject"` or `"both"`)
# - `mean`
# - `sd` (note: just like the `sd()` function, this value should be `NA` if only
#   one datapoint fro a grade is avilable.)
# The result of that function with `group.by = "school"` would be (up to 6 digits)
student.grade.data.byschool <- rbindlist(list(
    list(school = NULL,        mean = NULL, sd = NULL),
    list("North High School", 1.6666667,    0.3807887),
    list("South Academy",     2.027273,     0.8026094)
))
# The result with `group.by = "subject"` would be
student.grade.data.bysubject <- rbindlist(list(
    list(subject = NULL, mean = NULL, sd = NULL),
    list("maths",        2.325,       0.6946222),
    list("physics",      1.65,        0.6027714),
    list("chemistry",    1.94,        0.7197222),
    list("biology",      2.0,         0.5715476),
    list("compsci",      1.233333,    0.4041452)
))
# The result with `group.by = "both"` would be
student.grade.data.byboth <- rbindlist(list(
    list(school = NULL,       subject = NULL, mean = NULL, sd = NULL),
    list("North High School", "maths",        1.85,        0.2121320),
    list("North High School", "physics",      1.65,        0.4949747),
    list("North High School", "chemistry",    1.566667,    0.5131601),
    list("North High School", "biology",      1.65,        0.4949747),
    list("South Academy",     "maths",        2.8,         0.7071068),
    list("South Academy",     "physics",      1.65,        0.9192388),
    list("South Academy",     "chemistry",    2.5,         0.7071068),
    list("South Academy",     "biology",      2.35,        0.4949747),
    list("South Academy",     "compsci",      1.233333,    0.4041452)
))
# (where the ordering of rows does not matter)
#
# It is highly recommended that you make use of `ex01DataToLong()` here and then do
# aggregation with `[ ... by = ... ]`
ex02DataStats <- function(data, group.by) {
  # your code
  assertDataTable(data)
  assertChoice(group.by, c("school", "subject", "both"))
  group.by <- c(if (group.by != "subject") "school", if (group.by != "school") "subject")
  ex01DataToLong(data)[, .(mean = mean(grade), sd = sd(grade)), by = group.by]
}

# You want to look at the number of students taught by each school in each subject.
#
# Write a function that takes a `data.table` argument `data` and returns a `data.table`
# with the columns `school`, `subject`, `students`, where `students` is a `numeric` column
# listing the number of students in the `subject` at that `school`. Not all subjects get
# taught at all schools, in which case the table should contain a row for the
# respective `school` / `subject` where `students` is `0`. (`compsci` is not taught at
# `"North High School"`, in the example)
#
# The result for the example dataset above would be (with row order irrelevant)
student.grade.data.count <- rbindlist(list(
    list(school = NULL,       subject = NULL, students = NULL),
    list("North High School", "maths",        2),
    list("North High School", "physics",      2),
    list("North High School", "chemistry",    3),
    list("North High School", "biology",      2),
    list("North High School", "compsci",      0),
    list("South Academy",     "maths",        2),
    list("South Academy",     "physics",      2),
    list("South Academy",     "chemistry",    2),
    list("South Academy",     "biology",      2),
    list("South Academy",     "compsci",      3)
))
# It is pobably easiest to use `ex01DataToLong()` to get the table into long format first.
# You can use the `CJ()` function to get the first two columns, and
# joins and aggregation to get the third column.
ex03DataCount <- function(data) {
  # your code
  assertDataTable(data)
  data <- ex01DataToLong(data)
  aggr <- data[, .(students = length(student.id)), by = c("school", "subject")]
  aggr <- aggr[CJ(school, subject, unique = TRUE), on = c("school", "subject")]
  aggr$students <- nafill(aggr$students, fill = 0)
  aggr
}

# Write a function that gets the data into "wide" format: The input `data` is a `data.table`
# in the format of `student.grade.data`, which should be turned into a `data.table` with
# one row per student, and columns `student.id` and `school`, followed (for example) by columns
# `grade.maths`, `grade.chemistry`, `grade.XXX` -- i.e. one of the subject names each (in any order) prefixed
# with `grade.`.
# The result of the example dataset above could be
student.grade.data.wide <- rbindlist(list(
    list(student.id = NULL, school = NULL,
      grade.maths = NULL, grade.physics = NULL, grade.chemistry = NULL, grade.biology = NULL, grade.compsci = NULL),
    list("xzqk",            "North High School",
      2.0,                1.3,                  1.7,                    NA,                   NA),
    list("hffi",            "South Academy",
      2.3,                1.0,                  NA,                     2.0,                  1.0),
    list("hwdp",            "South Academy",
      3.3,                NA,                   3.0,                    2.7,                  1.7),
    list("flzb",            "North High School",
      1.7,                2.0,                  2.0,                    2.0,                  NA),
    list("txyb",            "North High School",
      NA,                 NA,                   1.0,                    1.3,                  NA),
    list("fjea",            "South Academy",
      NA,                 2.3,                  2.0,                    NA,                   1.0)
))
# (but the `grade.maths` could also come after the `grade.physics` column for example). Note the `NA`s in places where
# a grade is not given. The ordering of rows should be the same as in the input data.
#
# This can be solved by using `ex01DataToLong()` followed by the `dcast()` method.
ex04DataWide <- function(data) {
  # your code
  assertDataTable(data)
  dcast(ex01DataToLong(data)[, subject := paste0("grade.", subject)],
    student.id + school ~ subject, value.var = "grade")[data$student.id, on = "student.id"]
}
