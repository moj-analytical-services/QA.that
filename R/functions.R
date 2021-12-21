

### required packages
require(dplyr)
require(emojifont)
require(purrr)
require(magrittr)
require(crayon)
require(roxygen2)
##########################



#
# #' Test to check whether two numeric values are equal.
# #'
# #'
# #' The way this function works it will allow for some digits to be rounded off in case there are
# #' numbers with minor differences.
# #'
# #' This function is meant to run in the context of a `check` and not on its own. You can run it in stand alone mode to test the output but
# #' the corresponding global container will be deleted when a `check` is ran.
# #'
# #'
# #' @param descr A text string to explain what this test is about
# #' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
# #' @param test_value The other end of the comparison - This is the tested value
# #' @param rounding_digits This is the number of rounding up digits to be used when comparing. Default is 1 digit
# #'
# #'
# #' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`.This is a
# #'              binary TRUE or FALSE result for each test.
# #'
# #' @examples
# #'
# #' ## You can choose to use the function on its own but is best suited for use within a `check` and
# #' ## this within a `QA_that` environment
# #'
# #' output44 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
# #'  test_equal(descr = "test description text", 12,12)
# #'  test_identical(descr = "test description text", 12,12.1)
# #'  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})
# #'
# test_equal = function(descr, source_value, test_value, rounding_digits=1, ...){
#
#   output = NULL
#
#   # check whether .GlobalEnv$global_output exists - otherwise the function will not run
#
#   if(!exists("global_output", envir = .GlobalEnv)){
#
#     warning("Global container not initialised - It is recommended to run this  function as part of an associated check.
#             The container for a stand alone run will be overwritten. Output is set to 'global_output' variable")
#
#     .GlobalEnv$global_output = tibble(test_description = character(), outcome = logical()) }
#
#
#   if(is.numeric(source_value) & is.numeric(test_value)){
#
#
#
#     r_sval = round(source_value, digits = rounding_digits)
#     r_tval = round(test_value, digits = rounding_digits)
#
#
#     if(r_sval == r_tval){ output = T}
#     else if(r_sval != r_tval){ output = F}
#
#
#
#
#
#   }else {stop("Values must be numeric. To test for text values use test_match() instead.")}
#
#
#   # assign global variables to keep track between different checks etc
#   assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )
#
#   # browser()
#   # return(output)
#
#
# }
#



#' Test to check whether two numeric vectors are equal.
#'
#'
#' The way this function works it will allow for some digits to be rounded off in case
#' numbers with minor differences
#'
#' This function is meant to run in the context of a `check` and not on its own. You can run it in stand alone mode to test the output but
#' the corresponding global container will be deleted when a `check` is ran.
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to. In this function it has to be a
#'                      vector of numeric values.
#' @param test_value The other end of the comparison - This is the tested value. In this function it has to be a vector of numeric values.
#' @param rounding_digits This is the number of rounding up digits to be used when comparing. Default is 1 digit
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`. This is a
#'              binary TRUE or FALSE result for each test.
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#'
#' a = c(1,2,3)
#' b = c(1,2,4)
#'
#' output44 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
#'  test_vector_equal(descr = "test description text", a,b)
#'                                                             })
#'




test_is_equal = function(descr, source_value, test_value, rounding_digits=1, ...){

  output = NULL

  # check whether .GlobalEnv$global_output exists - otherwise the function will not run

  if(!exists("global_output", envir = .GlobalEnv)){

    warning("Global container not initialised - It is recommended to run this  function as part of an associated check.
            The container for a stand alone run will be overwritten. Output is set to 'global_output' variable")

    .GlobalEnv$global_output = tibble(test_description = character(), outcome = logical()) }


  if(is.numeric(source_value) & is.numeric(test_value) &   (length(source_value) ==  length(test_value))  ){



    r_sval = round(source_value, digits = rounding_digits)
    r_tval = round(test_value, digits = rounding_digits)

    rndd <- r_sval == r_tval

    # if NA element it counts as 0 - catch when all of the elements are T or F
    if(all(rndd)){ output = T}
    else if(!all(rndd)){ output = F}
    else{stop("values are out of bounds - neither TRUE or FALSE. Check if vectors have NA values.")}





  }else {stop("Values must be numeric. To test for text values use test_match() instead.")}


  # assign global variables to keep track between different checks etc
  assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )

  # browser()
  # return(output)


}







#' Test to check whether two numeric vectors are identical
#'
#'
#' This function will test for strict equality between a source and a test value
#'
#' This function is meant to run in the context of a `check` and not on its own. You can run it in stand alone mode to test the output but
#' the corresponding global container will be deleted when a `check` is ran.
#'
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
#' @param test_value The other end of the comparison - This is the tested value
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`.This is a
#'              binary TRUE or FALSE result for each test.
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#' output44 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
#'  test_equal(descr = "test description text", 12,12)
#'  test_identical(descr = "test description text", 12,12.1)
#'  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})


test_identical = function(descr, source_value, test_value, ...){


  # check whether .GlobalEnv$global_output exists - otherwise the function will not run

  if(!exists("global_output", envir = .GlobalEnv)){

    warning("Global container not initialised - It is recommended to run this  function as part of an associated check.
            The container for a stand alone run will be overwritten. Output is set to 'global_output' variable")

    .GlobalEnv$global_output = tibble(test_description = character(), outcome = logical()) }



  output = NULL

  if(is.numeric(source_value) & is.numeric(test_value)  & (length(source_value) ==  length(test_value))){

    all_equal <- source_value == test_value


    if(all(all_equal)){ output = T}
    else if(!all(all_equal)){ output = F}
    else{stop("values are out of bounds - neither TRUE or FALSE. Check if vectors have NA values.")}



  # assign global variables to keep track between different checks etc
  assign("global_output", add_row(.GlobalEnv$global_output, test_description = descr, outcome = output), envir = .GlobalEnv )

  # return(output)

}

}

#' Test to check whether two character vectors are identical
#'
#'
#' This function is meant to run in the context of a `check` and not on its own. You can run it in stand alone mode to test the output but
#' the corresponding global container will be deleted when a `check` is ran.
#'
#' @param descr A text string to explain what this test is about
#' @param source_value The one end of the comaprison - This is the source/origin you are comapring to
#' @param test_value The other end of the comparison - This is the tested value
#'
#'
#' @return All  test functions will assign their output to a global variable so as to enable reusing them in multiple `tests` and `checks`.This is a
#'              binary TRUE or FALSE result for each test.
#'
#' @examples
#'
#' ## You can choose to use the function on its own but is best suited for use within a `check` and
#' ## this within a `QA_that` environment
#'
#' output44 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
#'  test_equal(descr = "test description text", 12,12)
#'  test_identical(descr = "test description text", 12,12.1)
#'  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})

test_match = function(descr,source_value, test_value, ...){

  output = NULL

  # check whether .GlobalEnv$global_output exists - otherwise the function will not run

  if(!exists("global_output", envir = .GlobalEnv)){

    warning("Global container not initialised - It is recommended to run this  function as part of an associated check.
            The container for a stand alone run will be overwritten. Output is set to 'global_output' variable")

    .GlobalEnv$global_output = tibble(test_description = character(), outcome = logical()) }


  if (length(source_value) !=  length(test_value)) stop("source and test values are of unequal length")

  if(is.character(source_value) & is.character(test_value)){

    all_match <- source_value == test_value

    if(all(all_match)){ output = T}
    else if(!all(all_match)){ output = F}
    else{stop("values are out of bounds - neither TRUE or FALSE. Check if vectors have NA values.")}

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
#' This functiosn is meant to run with the associated `test` functions and not on its own.
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




#' Creating folder structure for QA
#'
#'
#' This will create a folder structure to use with the `QA.that` package. The important thing to note here is that, by default, the function needs to be used from
#' the root working directory. If it runs from a script, it will be created in the directory where the corresponding script is located. The folder name is set to be `QA_that` and is supposed to be static
#' so as to enable picking it up from otehr functions for later porcessing.
#'
#' Alternatively the `QA_that` folder can be created ina custom location. See details below.
#'
#' @param QA_dir The complete path of the custon location to create the QA direcory. If set to NULL the current working directory will be used.
#'
#'
#' @param QA_file_name The name of the first QA file to be created. This will be an `R` file and it is  required to supply the suffix `.R` of the file in the definition.

#'
#'
#' @return The function will return a folder structure in the root of the main workign directory and a first QA file to get things started. In addition, a `QA_logs` folder
#' will also be created to store the corresponding logs from the QA process.
#'
#' @examples
#'
#' useQA("QA_that_table_match_table_B") ## using current workign directory
#'
#' use_QA(QA_dir = file.path(getwd(),"test1/test2","test3") ,  "table_A_match_table_B") ## using custom folder location
#'
#'
#'
#'
#'
#'
#'


use_QA = function(QA_dir = NULL, QA_file_name){

  # browser()
  require(dplyr)
  require(emojifont)
  require(purrr)
  require(magrittr)
  require(crayon)



  ## GP addition  here - manually specify  the QA directory if option is NOT NULL

  if(is.null(QA_dir)){

    root = getwd()

  }else{

    root = QA_dir

  }



  QA_folder_path =  file.path(root,"QA_that")
  QA_file_name = paste0("QA_that_",QA_file_name)
  QA_file_w_path =   file.path(QA_folder_path,QA_file_name)

  log_dir = file.path(root, "QA_that","QA_logs")         # log files directory


  ## create a QA folder to house the tests etc

  if(dir.exists(QA_folder_path)){
    stop(paste0("directory ", "`QA_that`", " alrady exists. Consider renaming it to keep existing tests.
             Ideally one such QA folder should ezxist per project to avoid confusion."))
  }else{

    dir.create(QA_folder_path,recursive = TRUE, showWarnings = TRUE) ## also checks if directrory already exists and puts out a warning if it does - keep this in mind
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


  ## set up output list with file and path info

  #output list containing for later use
  file_info = list(QA_logs_dir = log_dir,
                   QA_folder_path = QA_folder_path,
                   QA_file_w_path = QA_file_w_path,
                   QA_file_name = QA_file_name,
                   root_used = root)



return(file_info)




}






#' Map helper function for use with log creation
#'
#'
#' Supplementary function to assist with the main map used to create the logs. This is an internal function and there should be no need to access it directly.
#'
#'
#'
#'
#' @param check_d A description of the `check` to be added to the list
#'
#' @param log_df A dataframe corresponding to the log entries for the asociated `check`
#'
#'@param log_file The log file to be used in the process of binding all together.
#'
#'
#' @return This function will process each of the entries in teh corresponding collectioan of `checks`.
#'
#' @examples
#'
#'
#'
#'
#'
#'

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






#' Summary function for data collection
#'
#'
#' An important function to collect all information from relative `checks` and then display it in a log file for dissemination purposes.'
#'
#'
#'
#' @param ... A collection of all the different dataframes coming from the corrsponding `checks`
#'
#' @param log_file_name A name for the log file to be used to store the summary. This should be a character string with the suffix. Ex. "a_log_file.txt"
#'
#'@param del.flag A binary flag to indicate whethe to delete the log file after consequetive runs. Set to `TRUE` by default will remove the existing file of the function is run
#'a second time in a row.
#'
#'
#' @return This function will return a dataframe containing information from all the `checks`. In additon it will create a log file indicating the results of each `test`.
#'
#' @examples
#'
#'## putting it all together, assuming you already have a `QA_that` folder in the root of you project, otherwise please run `useQA` first
#'
#'## useQA("random_QA")
#'
#'
#'output1 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
#'  test_equal(descr = "test description text", 12,12)
#'  test_identical(descr = "test description text", 12,12.1)})
#'
#'
#'output2 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})
#'
#'
#'
#'output3 = check_that(check_desc = "test round 3", {test_equal(descr = "test rounding equal 11 with 11.3 ", 11,11.3, rounding_digits = 0)
#'  test_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
#'  test_identical(descr = "test description text", 12,12.1)
#'  test_identical(descr = "check identical  that 10 i s equal to 10.2 ", 10,10.1)
#'  test_match(descr = "checking that two test strings match", "DFDF","dfdf")
#'
#'})
#'
#'
#'
#'
#'bb = summary_QA(output1,output2, output3, log_file_name = "log that other file.txt")
#'
#'
#'
#'

summary_QA = function(..., QA_logs_dir = NULL, log_file_name = NULL, del.flag = T){

  require(purrr)
  require(dplyr)

  # browser()

  dots = list(...)

  log_df = map_dfr(dots, rbind)

  # create a directory for the logs


#
#
#   if(!dir.exists(log_dir)){
#     dir.create(paste0(root,"/QA_that/QA_logs"))
#     cat(paste("creating QA_logs directory in `QA_that` folder"))
#   }
#
#   if(!dir.exists(log_dir)){
#     dir.create(QA_logs_dir)
#     cat(paste("creating QA_logs directory in `QA_that` folder"))
#   }



  ## default option - no logs dir definition
  ##
  if(is.null(QA_logs_dir)){

    root = getwd()
    log_dir = file.path(getwd(),"QA_that","QA_logs")

    # check if dir exists and if not create it
    if(!dir.exists(log_dir)){
      dir.create(logs_dir, recursive = TRUE, showWarnings = TRUE)
      cat(paste("QA logs dir does not exist - creating it in `QA_that` folder"))
    }


  }else{ ## custom location for the logs directory

    log_dir = QA_logs_dir

    # check if dir exists and if not create it
    if(!dir.exists(log_dir)){
      dir.create(log_dir, recursive = TRUE, showWarnings = TRUE)
      cat(paste("QA logs dir does not exist - creating it in `QA_that` folder"))
    }

  }

  # root = getwd() # get root
  # log_dir = paste0(root,"/QA_that/QA_logs") # log files directory

  # get the file name of the current script
  # log_fname_w_path = paste0(log_dir,"/", log_file_name)

  log_fname_w_path = file.path(log_dir, log_file_name)   #paste0(log_dir,"/", log_file_name)

  if(is.null(log_file_name)){
    stop("log file name is NULL. Consider naming the log file to be created.")
  }else if(del.flag){
    unlink(log_fname_w_path)
    cat("Deleting file from previous run --> del.flag==T")
  }



  log_file = file(log_fname_w_path, open = "a") #open a  file and have it ready to append text to it


  ### to get the file name just right here -  it has .r at the end
  # browser()



  # group the log data frame
  log_df_grouped = log_df %>% group_by(check_description) %>% nest()


  # now use a map to paste to the log file for each category

  map2( log_df_grouped$check_description, log_df_grouped$data, map_write_to_log, log_file = log_file)



  ## close the file after logging everything
  close(log_file)



  return(log_df)

  }

