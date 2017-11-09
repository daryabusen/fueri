library(dplyr)
# Implements the method checkHeight, to calculate the
#height difference of students from the general or a sex specific mean

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my first R-package and thanks for using it, muchacho!")
}

# Dataframe attributes
age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

# Create dataframe
students = data.frame(cbind(age, weight, height, sex))

# Change dataframe datatypes
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

# Add names
students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

#' Create a dataframe with names and difference in height from sex specific mean
#' @param students.input [\code{data.frame}]\cr
#'   The original students dataframe
#'   Default is students
#' @return [\code{data.frame}]\cr
#'   Return the newly created data.frame
checkHeight = function(students.input = students){
  # Calculate the sex specific mean in height
  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))

  # Create new list saving all the sex specific height differences
  height.list = apply(students.input, MARGIN = 1,
                      FUN = function(student){
                        (if (student["sex"] == "M") male.mean - as.numeric(student["height"])
                         else female.mean - as.numeric(student["height"]))
                      })

  # Create a new dataframe containing only the names and the height differences
  output.df = data.frame("name" = students.input$name, "sexspec_height_diff" = unlist(height.list))

  # Return new created dataframe
  return(output.df)
}

# Call method
checkHeight(students.input = students)
