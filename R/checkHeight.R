# Autors Darya Busen, Michael Graber

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to my first R-package and thanks for using it, muchacho!")
}
#' Compare the height of persons in a dataset with the mean of the height of the other persons
#' @param students.input The original students data.frame
#' @param sex.specific Sex specific mean or mean of population
#' @param print.statement Print a message after the calculation
#' @return Return the newly created data.frame, difference in height to gender specific mean
#' @export
#' @importFrom magrittr %>%
#' @import checkmate
#' @import dplyr
checkHeight = function(students.input, sex.specific = TRUE, print.statement = FALSE){

  #Do check:
  #sex.specific is a logical value
  assertLogical(sex.specific)
  #print.statement is a logical value
  assertLogical(print.statement)
  #the Data Frame with a minimum of 4 rows and exactly 5 columns without any missing values
  assertDataFrame(students.input, types = c("numeric", "numeric", "numeric", "factor", "character"))
  #third column is numeric and has values between 1.30 and 2.40
  assertNumeric(students.input[, "height"], lower = 1.3, upper = 2.40)
  #4th column is of type factor and with maximum two levels "M" and "F"
  assertFactor(students.input[, "sex"], levels = c("M","F"))

  if(sex.specific == TRUE){
    #Calculate the sex specific mean in height
  male.mean = students.input[,"height"][students.input[,"sex"] == "M"] %>%
    mean %>% as.numeric

  female.mean = students.input[,"height"][students.input[,"sex"] == "F"] %>%
    mean %>% as.numeric

  # Create new list saving all the sex specific height differences
  height.list = apply(students.input, MARGIN = 1,
                      FUN = function(student){
                        #substract the gender specific means from the individuals to get height differnces
                        (if (student["sex"] == "M") male.mean - as.numeric(student["height"])
                         else female.mean - as.numeric(student["height"]) )
                      } )

  # Create a new dataframe containing only the names and the height differences
  output.df = data.frame("name" = students.input$name, "sexspec_height_diff" = height.list)

  #create the final dataframe containing name od the students and the height differnces
  #multiple height differences by 100 to get values in cm
  } else{
    #calculate the mean height of the whole population
    mean_height = students.input[,"height"] %>% mean %>% as.numeric
    #apply a function to the rows of the input dataframe
    height_vector = apply(students.input, MARGIN = 1,
                          FUN = function(student){
                            #substract the gender specific means from the individuals to get height differnces
                            mean_height - as.numeric(student["height"])
                          } )
    #create the final dataframe containing name od the students and the height differnces
    #multiple height differences by 100 to get values in cm
    output.df= data.frame("name" = students.input$name, "sexspec_height_diff" = height.list)
  }

  # Print success message

  if (print.statement == TRUE){
    print("Yippie, I calculated the mean difference!")
  }

  # Return new created dataframe
  return(output.df)

}

