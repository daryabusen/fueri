test_that("Input dataframe has the same size as the output dataframe", {
  expect_equal(nrow(students),nrow(checkHeight(students)))
})


test_that("Functioning of the argument check ", {
  #test if sex.specific is boolean
  expect_error(checkHeight(students, sex.specific = 5))
  #test if print.statemnet is boolean
  expect_error(checkHeight(students, print.statement = 5))
  #test if data frame is long enough
  expect_error(checkHeight(students[1:3]))
  #test if persons not in range are registered
  students_test <- students
  students_test[1,3] <- 5.80
  expect_error(checkHeight(students_test))
  #test if persons wrong gender is registered
  students_test <- students
  levels(students_test[,4]) <- c("F", "M", "C")
  expect_error(checkHeight(students_test))
  #test mean function
  expect_error(compareheight:::mean(NaN))
})

test_that(desc = "The Object can't be found if a  name is wrong", {
  test_students = students
  colnames(test_students)[3] = "servus"
  expect_that(checkHeight(test_students), throws_error() )
})


students.subset1 = students[1,]
students.subset2 = students[1:5,]
students.subset3 = students[-4,]
test_that("checkHeight outputs correct df dimension", {
  expect_data_frame(checkHeight(students.subset1), nrows = nrow(students.subset1))
  expect_data_frame(checkHeight(students.subset2), nrows = nrow(students.subset2))
  expect_data_frame(checkHeight(students.subset3), nrows = nrow(students.subset3))

})

test_that("print checking", {
  expect_output(checkHeight(students, print.statement = TRUE), "Yippie, I calculated the mean difference!")
})

