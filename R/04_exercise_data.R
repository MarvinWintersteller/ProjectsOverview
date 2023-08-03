# Write a function that does ProgR-course homework grading
#
# The function takes two arguments
# * `problems`: a character vector listing the available exercise problems.
#
#   Exercise problems given in the format "rfile.R:ex01FunctionName" -- the R-file,
#   followed by a colon, followed by the function name of the exercise. The '.R'
#   part is non-essential here, but the colon is, separating the "problem set"
#   from the "problem".
#
#   The `problems`-vector for the second homework would for example be:
problems.first.homework <- c(
  "01_exercise_getting_started.R:ex01Divide",
  "01_exercise_getting_started.R:ex02DivideVectors",
  "01_exercise_getting_started.R:ex03VectorType",
  "01_exercise_getting_started.R:ex04Even",
  "02_exercise_vectors.R:ex01BinCounting",
  "02_exercise_vectors.R:ex02Binning",
  "02_exercise_vectors.R:ex03FizzBuzz",
  "03_exercise_matrices_and_dataframes.R:ex01Counterdiagonal",
  "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMax",
  "03_exercise_matrices_and_dataframes.R:ex03BagOfWords",
  "03_exercise_matrices_and_dataframes.R:ex04SelectDF",
  "03_exercise_matrices_and_dataframes.R:ex05Imputation",
  "04_control_structures.R:ex01SemanticNetwork",
  "04_control_structures.R:ex02CellularAutomaton"
)
# * `completion`: a `data.frame` with two columns
#   - `student`: a character column indicating the student
#   - `solved`: a character column indicating the problems
#     completed successfully by the student.
#   The `completion` parameter contains one row per student and completed
#   exercise. If "studentA" has solved all problems from 01_exercise_vectors.R
#   and the first three problems of 02_exercise_matrices_and_dataframes.R, while
#   "studentB" has only solved the Counterdiagonal and the CellularAutomaton problem
#   then the input `completion` argument could look like this:
completion.example <- data.frame(
    student = c("studentA", "studentA", "studentB", "studentA", "studentA", "studentA",
      "studentA", "studentA", "studentB", "studentA", "studentA", "studentA", "studentA"),
    solved = c("01_exercise_getting_started.R:ex02DivideVectors",
      "02_exercise_vectors.R:ex03FizzBuzz",
      "04_control_structures.R:ex02CellularAutomaton",
      "01_exercise_getting_started.R:ex01Divide",
      "03_exercise_matrices_and_dataframes.R:ex03BagOfWords",
      "01_exercise_getting_started.R:ex03VectorType",
      "01_exercise_getting_started.R:ex04Even",
      "02_exercise_vectors.R:ex02Binning",
      "03_exercise_matrices_and_dataframes.R:ex01Counterdiagonal",
      "03_exercise_matrices_and_dataframes.R:ex02MatrixWhichMax",
      "03_exercise_matrices_and_dataframes.R:ex01Counterdiagonal",
      "02_exercise_vectors.R:ex01BinCounting",
      "01_exercise_getting_started.R:ex03VectorType"),
    stringsAsFactors = FALSE
)
# The function should return a data.frame with two columns:
# * `student` - name of the student. Ordering of the students does not matter.
# * `score` - final score of the student
# Scoring works as follows: Each student gets 0.25 points for each problem set (i.e. .R-file) that
# they solved completely. In the example above, "studentA" would get 0.5 points because the
# 01_exercise_getting_started.R-set and the 02_exercise_vectors.R-set are complete.
# "studentB" would get 0 points. The return for this example should therefore be
# ex01ProgrGrading(problems = problems.first.homework, completion = completion.example)
# --> data.frame(student = c("studentA", "studentB"), score = c(0.5, 0), stringsAsFactors = FALSE)
# Other example calls:
# ex01ProgrGrading(
#   problems = problems.first.homework,
#   data.frame(student = character(0), solved = character(0), stringsAsFactors = FALSE)
# ) --> data.frame(student = character(0), score = numeric(0), stringsAsFactors = FALSE)
# ex01ProgrGrading(
#   problems = problems.first.homework,
#   data.frame(student = "studentA", solved = "01_exercise_vectors.R:ex01Multiplication", stringsAsFactors = FALSE)
# ) --> ERROR (because "01_exercise_vectors.R:ex01Multiplication" is not one of the problems)
# ex01ProgrGrading(
#   problems = c("A:one", "B:one"),
#   data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> data.frame(student = "studentA", score = 0.25, stringsAsFactors = FALSE)  solved the "A" problem set completely
# ex01ProgrGrading(
#   problems = c("A:one", "B"),
#   data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there is no ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B::one"),
#   data.frame(student = "studentA", solved = "A:one", stringsAsFactors = FALSE)
# ) --> ERROR ('problems' list is badly formatted, because there are two ':' in problem "B")
# ex01ProgrGrading(
#   problems = c("A:one", "B:one", "A:two", "C:three"),
#   data.frame(
#     student = c("studentA", "studentB", "studentC", "studentB", "studentC", "studentA"),
#     solved = c("C:three", "A:two", "C:three", "A:one", "B:one", "B:one"),
#     stringsAsFactors = FALSE)
# ) --> data.frame(student = c("studentB", "studentC", "studentA"), score = c(0.25, 0.5, 0.5), stringsAsFactors = FALSE)
# You may find the `tapply()` function useful here.
ex01ProgrGrading <- function(problems, completion) {
  # your code
  assertCharacter(problems, any.missing = FALSE)
  assertDataFrame(completion, ncol = 2)
  assertSubset(colnames(completion), c("student", "solved"))
  problems.split <- strsplit(problems, ":")
  if (any(vapply(problems.split, length, 0) != 2)) {
    stop("problem names malformed")
  }
  assertSubset(completion$solved, problems)
  if (!nrow(completion)) {
    return(data.frame(student = character(0), score = numeric(0), stringsAsFactors = FALSE))
  }

  problems.sets <- vapply(problems.split, `[[`, 1, FUN.VALUE = character(1))

  resultvect <- tapply(completion$solved, completion$student, function(completed) {
    uncompleted.sets <- problems.sets[!problems %in% completed]
    completed.sets <- setdiff(problems.sets, uncompleted.sets)
    length(completed.sets) * .25
  })
  data.frame(student = names(resultvect), score = unname(resultvect), stringsAsFactors = FALSE)
}
