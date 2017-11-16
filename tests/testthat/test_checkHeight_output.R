

test_that("Input and output has the same size", {
  expect_equal(nrow(students),nrow(checkHeight(students)))
})

# Check if sex.specific works
test_that("Input parameter sex.specific works correctly", {
  new_students = students
  new_students$height = c(1.3,1.9,1.75,1.15,1.7,1.8,2.0,1.5)
  expect_equal(checkHeight(new_students, FALSE),checkHeight(new_students, TRUE))
})

# Check for error mesaage object not found
test_that("Error message 'object '<object name>' not found' is correct", {
  expect_error(checkHeight(students.input = a), "object 'a' not found")
})

# Check if asserts are working correclty
test_that("Check if assert warnings are working correctly", {
  expect_error(checkHeight(students,sex.specific = 1),"Assertion on 'sex.specific' failed: Must be of type 'logical', not 'double'.")
  expect_error(checkHeight(students,print.statement = 1),"Assertion on 'print.statement' failed: Must be of type 'logical', not 'double'.")
  expect_error(checkHeight(students[1:3]),"Assertion on 'students.input' failed: Must have exactly 5 cols, but has 3 cols.")
})

test_that("Check if print.statement works correctly", {
  expect_that(checkHeight(students,print.statement = TRUE), prints_text("Yippie, I calculated the mean difference!"))
})
