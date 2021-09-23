

### required packages
library(tidyverse)
library(emojifont)
library(purrr)
library(magrittr)
library(crayon)
library(roxygen2)
##########################




#' Test to check whether two values are equal.
#'
#'
#' The way this function works it will allow for some digits to be rounded off in case
#' numbers with minor differences
#'
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
#' @param test_value The other end of the comparison - This is the tested value
#' @param rounding_digits This is the number of rounding up digits to be used when comparing. Default is 1 digit
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#' test_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
#'
test_equal = function(descr, source_value, test_value, rounding_digits=1, ...){

  output = NULL

  if(is.numeric(source_value) & is.numeric(test_value)){



    r_sval = round(source_value, digits = rounding_digits)
    r_tval = round(test_value, digits = rounding_digits)


    if(r_sval == r_tval){ output = T}
    else if(r_sval != r_tval){ output = F}





  }else {stop("Values must be numeric. To test for text values use test_match() instead.")}


  # assign global variables to keep track between different checks etc
  assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )

  # browser()
  # return(output)


}




#' Test to check whether two values are identical
#'
#'
#' This function will test for strict equality between a source and a test value
#'
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
#' @param test_value The other end of the comparison - This is the tested value
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#' test_identical(descr = "test description text", 12,12.1)
#'
test_identical = function(descr, source_value, test_value, ...){

  output = NULL

  if(is.numeric(source_value) & is.numeric(test_value)){

    if(source_value == test_value){ output = T}
    else if(source_value != test_value){ output = F}

  }else {stop("Values must be numeric. To test for text values use test_match() instead.")}



  # assign global variables to keep track between different checks etc
  assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )

  # return(output)

}


#' Test to match whether a string matches a given source value
#'
#'
#' This functioasn will test to see if two strings are identical
#'
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
#' @param test_value The other end of the comparison - This is the tested value
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#' test_match(descr = "checking that two test strings match", "DFDF","dfdf")
#'

test_match = function(descr,source_value, test_value, ...){

  output = NULL

  if(is.character(source_value) & is.character(test_value)){

    if(source_value == test_value){ output = T}
    else if(source_value != test_value){ output = F}

  }else {stop("Values must be characters. To test for numeric values use test_equal() or test_identical() instead.")}

  # assign global variables to keep track between different checks etc
  assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )
  # return(output)

}




#' Combining all tests
#'
#'
#' This will combine all tests into a `check` as a way of colecting all values for subsequent display
#'
#'
#' @param check_desc A text string to provide a description for the colelction of tests
#' @param code The collection of code blockes to run
#'
#'
#' @return This function will return `global_output` to the `.GlobalEnv` for use in other functions of the package. This is a dataframe with details about the results fo the `c`
#'
#' @examples
#'
#' output = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
#'                                                              test_equal(descr = "test description text", 12,12)
#'                                                            test_identical(descr = "test description text", 12,12.1)})
#'



check_that = function(check_desc,code, ...){

  # initialise global container variable
  global_output = tibble(test_description = character(), outcome = logical())

  # assign and create a global version of the variable
  assign("global_output", global_output, envir = .GlobalEnv)

  #evaluate the tests
  eval(code)

  #save to global envir and updawte with the check description
  .GlobalEnv$global_output %<>% add_column(check_description = check_desc, .before = "test_description")


  # housekeeping and presentation

  if(sum(.GlobalEnv$global_output$outcome) == dim(.GlobalEnv$global_output)[1]){
    cat(paste0("\n Checking that: ", check_desc, " .... PASS ", sample(emoji(search_emoji('smile')), 1, replace = T )))
  }else{
    not_pass = .GlobalEnv$global_output %>% filter(outcome == F)
    cat(paste0("Checking that: ", check_desc, " ... FAIL \u274c ", "\n"))

  }


  cat("\n Summary of checks:-------------------------------------", "\n")
  cat(paste0(.GlobalEnv$global_output$test_description, " \t ---->",if_else(.GlobalEnv$global_output$outcome, paste0("\t PASS ", green("\u2714"),"\n"),
                                                                            "\t FAIL \u274c \n") ) )
  cat("-------------------------------------------------------")




  ## create another global storage in a "QA_that" level so as to store overall QA level results

  #
  # # to bind to a globeal oveall variable combining all checks in a file
  # assign("global_overall_output", rbind(.GlobalEnv$global_overall_output, .GlobalEnv$global_output), envir = .GlobalEnv)
  #

  return(.GlobalEnv$global_output)


}












use_QA = function(QAname){

  # browser()
  root = getwd()
  QA_folder_path = paste0(root, "/QA_that")
  QA_file_w_path = paste0(QA_folder_path,"/","QA_that_",QAname,".R")
  QA_file_name = paste0("QA_that_",QAname,".R")

  log_dir = paste0(root,"/QA_that/QA_logs") # log files directory


  ## create a QA folder to house the tests etc

  if(dir.exists(QA_folder_path)){
    stop(paste0("directory ", "`QA_that`", " alrady exists. Consider renaming it to keep existing tests.
             Ideally one such QA folder should ezxist per project to avoid confusion."))
  }else{

    dir.create(QA_folder_path)
    cat(paste0("\n", "directory ","`QA_that`", " was created ----> ", green("\u2714")))

  }

  ## to srite the first file

  if(file.exists(QA_file_w_path)){
    stop("file exists in destination directory. Consider renaming.")
  }else{
    file.create(QA_file_w_path)
    cat(paste0("\n", "file ",QA_file_name, " was created ----> ", green("\u2714")))

  }



  ## adding global container variable to collect results from all `check_that`sections - Disontinued as it would be better to have each QA stream separate
  ## and collect results in each

  # global_check_that = tibble(check_description = character(), test_description = character(), outcome = logical())


  # to get the path of the current file if needed
  # full_script_path = rstudioapi::getSourceEditorContext()$path

  # current_script = str_extract(full_script_path, "QA_that_.+\\.R$")

  # create folder to save the log file (s)


  if(dir.exists(log_dir)){
    stop("QA logs directoryexists. Consider renaming.")
  }else{
    dir.create( log_dir )
    cat(paste0("\n", "QA_logs directory was created ----> ", green("\u2714")))

  }

}












map_write_to_log = function(check_d, log_df, log_file){

  # browser()


  # check_d = log_df$check_description
  test_d = log_df$test_description
  test_outcome = log_df$outcome %>% if_else("\u2714", "\u274c") # tick or else a cross





  cat(paste0("\n Checking that: ", check_d[1], " ############################### \n \n"), file = log_file)

  cat(paste0("\t \t", test_d," ----------> ", test_outcome), file = log_file, sep = "\n" )


  if(!is.character(check_d)){
    cat( "\t  Check Description is not a character string... Make sure the log data frame is structured properly \n", file = log_file, sep = "\n" )
  } else if(is.null(check_d)){
    cat("\t  The check descriptiopn is null (empty) \n", file = log_file, sep = "\n" )
  } else if(is.null(log_df)){
    cat("\t  The log data frame is null (empty) \n")
  }else {

    cat("\t -------------------------------------------------------------", file = log_file, sep = "\n")
    cat("\t  log data frame and check descriptions look OK", file = log_file, sep = "\n"  )
    cat("\t -------------------------------------------------------------", file = log_file, sep = "\n")

  }



  # Check Description has multiple entries for a single group

  return(TRUE)


}




summary_QA = function(..., log_file_name = NULL, del.flag = T){

  # browser()

  dots = list(...)

  log_df = map_dfr(dots, rbind)

  # get the file name of the current script
  # create a directory for the logs


  root = getwd() # get root
  log_dir = paste0(root,"/QA_that/QA_logs") # log files directory
  log_fname_w_path = paste0(log_dir,"/", log_file_name)


  if(is.null(log_file_name)){
    stop("log file name is NULL. Consider naming the log file to be created.")
  }else if(del.flag){
    unlink(log_fname_w_path)
    cat("Deleting file from previous run --> del.flag==T")
  }



  log_file = file(log_fname_w_path, open = "a") #open a  file and have it ready to append text to it


  ### to get the file name just right here -  it has .r at the end
  # browser()

  if(!dir.exists(log_dir)){
    dir.create(paste0(root,"/QA_that/QA_logs"))
    cat(paste("creating QA_logs directory in `QA_that` folder"))
  }



  # group the log data frame
  log_df_grouped = log_df %>% group_by(check_description) %>% nest()


  # now use a map to paste to the log file for each category

  map2( log_df_grouped$check_description, log_df_grouped$data, map_write_to_log, log_file = log_file)


  ## START HERE


  return(log_df)



}















