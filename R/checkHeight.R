#'@import dplyr
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my first R-package and thanks for using it, muchacho!")
}
#' Compare the height of persons in a dataset with the mean of the height of the other persons
#' @param students.input The original students data.frame
#' @param sex.specific Sex specific mean or mean of population
#' @param print.statement Print a message after the calculation
#' @return Return the newly created data.frame, difference in height to gender specific mean
#' @import checkmate
#'
#' @export
checkHeight = function(students.input = students, sex.spezific = TRUE, print.statement = FALSE){

  #Do check:
  #sex.specific is a logical value
  assertLogical(sex.specific)
  #print.statement is a logical value
  assertLogical(print.statement)
  #the Data Frame with a minimum of 4 rows and exactly 5 columns without any missing values
  assertDataFrame(students.input, min.rows = 4, ncols = 5, any.missing = FALSE)
  #third column is numeric and has values between 1.30 and 2.40
  assertNumeric(students.input[,3], lower = 1.29, upper = 2.41)
  #4th column is of type factor and with maximum two levels "M" and "F"
  assertFactor(students.input[,4], levels = c("M","F"))

  if(sex.specific == TRUE){
    #Calculate the sex specific mean in height
  male.mean = as.numeric(students.input %>%
                           group_by(sex) %>%
                           summarise(compareheight:::mean(height)) %>%
                           filter(sex == "M") %>%
                           select("compareheight:::mean(height)"))

  female.mean = as.numeric(students.input %>%
                             group_by(sex) %>%
                             summarise(compareheight:::mean(height)) %>%
                             filter(sex == "F") %>%
                             select("compareheight:::mean(height)"))

  # Create new list saving all the sex specific height differences
  height.list = apply(students.input, MARGIN = 1,
                      FUN = function(student){
                        (if (student["sex"] == "M") male.mean - as.numeric(student["height"])*100
                         else female.mean - as.numeric(student["height"]))*100
                      })

  # Create a new dataframe containing only the names and the height differences
  output.df = data.frame("name" = students.input$name, "sexspec_height_diff" = unlist(height.list))

  # Print success message

  if (print.statement) print("Yippie, I calculated the mean difference!")

  # Return new created dataframe
  return(output.df)
}

