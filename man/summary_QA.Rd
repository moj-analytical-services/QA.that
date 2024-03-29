% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{summary_QA}
\alias{summary_QA}
\title{Summary function for data collection}
\usage{
summary_QA(..., QA_logs_dir = NULL, log_file_name = NULL, del.flag = T)
}
\arguments{
\item{...}{A collection of all the different dataframes coming from the corrsponding `checks`}

\item{log_file_name}{A name for the log file to be used to store the summary. This should be a character string with the suffix. Ex. "a_log_file.txt"}

\item{del.flag}{A binary flag to indicate whethe to delete the log file after consequetive runs. Set to `TRUE` by default will remove the existing file of the function is run
a second time in a row.}
}
\value{
This function will return a dataframe containing information from all the `checks`. In additon it will create a log file indicating the results of each `test`.
}
\description{
An important function to collect all information from relative `checks` and then display it in a log file for dissemination purposes.'
}
\examples{

## putting it all together, assuming you already have a `QA_that` folder in the root of you project, otherwise please run `useQA` first

## useQA("random_QA")


output1 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
 test_equal(descr = "test description text", 12,12)
 test_identical(descr = "test description text", 12,12.1)})


output2 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})



output3 = check_that(check_desc = "test round 3", {test_equal(descr = "test rounding equal 11 with 11.3 ", 11,11.3, rounding_digits = 0)
 test_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
 test_identical(descr = "test description text", 12,12.1)
 test_identical(descr = "check identical  that 10 i s equal to 10.2 ", 10,10.1)
 test_match(descr = "checking that two test strings match", "DFDF","dfdf")

})




bb = summary_QA(output1,output2, output3, log_file_name = "log that other file.txt")




}
