
<!-- README.md is generated from README.Rmd. Please edit that file -->

# QA.that

This package was created in an attempt to automate the QA for
publications in the Ministry of Justice (MoJ). The obvious inspiration
comes from the `test_that` package in R that in turn was inspirted from
test practices applied in C\#, Java, Python etc. Many thanks to the
developers of those packages as well.

## Introduction

The concept here is to include `tests` that are combined into a `checks`
that are in turn collected into sections/chapoters for each area/code we
want to QA.

More specifically, the collection here wil aim at including the
following:

  - The basic element, the atom of QA will be the `test`:
      - `test_is_equal`
      - `test_identical`
      - `test_match`
      - `test_numeric` \#\# to be implemented
      - `test_character`\#\# to be implemented
      - `test_true`\#\# to be implemented
      - `test_false`\#\# to be implemented
      - `test_NA`\#\# to be implemented
      - any other that may arise in the future
  - The `test_...()`(s) will be combined into `check_that()`(s) as
    follows:
      - `check_that("text description", {test_is_equal(...),
        test_identical(...). ...} )`
      - output will be a summary of the whole process
      - see if warnings can be caught there as well
      - Other items to be added in the future
  - Tests will be combined into `QA_that_` sections/chapters regarding
    spcecific areas you want to QA
      - `QA_that_`(s) are files containing all checks that need to be
        conducted in the project. The corresponding filenames will need
        to start with `QA_that_` so as to distinguish them from anything
        else and also they need to be included in a `QA_that` folder
      - `QA_that_`(s) can be for example tests that need to be conducted
        in A or B tables in PRSQ
      - For better structure all files should start with `QA_that_`
        prefix so as to be able to collect results better

## Installation

You can install the released version of QA.that from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("QA.that")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("moj-analytical-services/QA.that")
```

``` r
library(QA.that)
```

## Explaining the process step by step

A number of other excelent packages were used to construct `QA.that` as
follows (with many thanks to authors):

``` r

library(dplyr)
library(emojifont)
library(purrr)
library(magrittr)
library(crayon)
library(QA.that)
# emoji(search_emoji('smile'))
```

### Covering the basics

  - QA files should start with `QA_that_` so that they make sense and
    also to enbable the system to pick them up esier. For examnple
    `QA_that_A_tables_are_in_line_with_B.R`
  - all QA should be in one folder and the starting point is considered
    the root directory of your project by default. There is an option to
    allow for custom paths in creating these folders
  - IMPORTANT: The function `use_QA()` can ran from your console in the
    root directory (if used with default arguments) or an R script. If
    the `QA_dir` is specified then the QA folder structure will be
    created in that location

### QA logs and other elements

The process will create a corresponding QA log to contain and summarise
all the tests conducted in your code and tables. The output of the
functions are neatly contained in a list dataframes so that further
analysis can be added as needed.

### Creating a folder structure

It is essential to create a folder structure to house all the files and
logs associated with the QA currently undertaken. This can be cumbersome
given that the process will need to QA a number of tables for example.
The function `use_QA()` alleviates the problem somewhat by creating the
folders and a sample QA file to include your code.

The file can be changed but it would be prudent to keep the file name
prefix as in `QA_that_` so as to make it easier to pick up the files at
a later stage of the process where all the individual tests are gathered
if this is needed.

The function `use_QA()` will can be executed from the root directory or
from the directory where you want the folders to be created or
alternatively the `QA_dir` argument can be used in shich case folders
will be created in that file path.

If you are in a hurry, from your console (while using default arguments
and at the root of the directory where you want to create the folder)
type `use_QA()` and then press enter. This should create the folder in
the main project directory.

R does not like too many spaces when naming things so use`_` where
necessary.

``` r
# if the function runs here it will create a folder where this markdown file is located. 
# Run this from the root directory for better results!

use_QA("QA_that_A_match_B_tables") 

## or alternatively


use_QA(QA_dir = file.path("directory_path_placed_here"), QA_file_name = "QA_that_A_match_B_tables.R") 
```

### Conducting tests

Having done that the next steps are to start coding your QA tests. As
mentioned in the introduction section above, this will require including
a number of `test_...` *within* a `check_that()`. These are specialised
functions that were made to help with the hierarchical structure of the
code.

For example take the following code

``` r

output44 = check_that(check_desc = "numeric matching test 1", {test_is_equal(descr = "test description text", 11,11)
  test_is_equal(descr = "testing vectorization",c(1,2,2,3),c(1,2,3,5))
  test_identical(descr = "test description text", 12,12.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})
```

Notice that in the global environment the variables `output44` as well
as `global_output` were created. We know about the former since we
created it previously but the latter is a hidden container created to
house temporary data a an intermediate stage before adding all the
results together. In addition to that, it reduces the computational
burden on the system as the same container is loaded with information
instead of creating different variables each time a test is conducted.

As it will be explained later on, this action witht he `global_output`
has only local scope within each check and this is by design to avoid
crosstalk.

### Test functions

There are a number of functions available for conducting tests and many
more to be created according to what the team needs to QA. The most
important ones are the `test_is_equal()`, `test_identical()` and
`test_match()` functions.

As the name would suggest the function `test_is_equal()` will check if
two numeric values are equal but also taking into account rounding up
that may be in place. The function `test_identical()` will do the same
thing but exact equality is required while the `test_match()` is
primarily for character vectors and strings. Many more function will be
created and all of them will have a help file to accompany them for more
information.

The way to use them is specified in the corresponding help files but
almost all of them will have the following basic definition:

`test_...(descr, source_value, test_value, ...)`

meaning they will require a description (`descr`) for the test, the
`source_value` and `test_value`. The rest of the arguments will depend
on the function type.

**IMPORTANT NOTE: ** Running the functions on their own is possible but
not recommended (this will be possible removed a functionality in future
versions). This will result in augmenting the global container as all
tests are linked if not run as part of a `check_that()`.

Notice that in the example below running the test on its own will result
in augmenting the `global_container` variable regardless of whether a
copy is assigned or not. This is by design as the tests should be housed
and relate to one specific check that is conducted.

``` r

global_copy = test_is_equal(descr = "testing vectorization",c(1,2,2,3),c(1,2,3,5))

## the variables below are/should be the same !!

global_output

global_copy
```

### Including `test_..()` within a `check_that()`

The whole idea is to enable batch testing within a certain coherent
setting. With this in mind the `check_that()` function will be used to
include all the tests relative to that portion of the QA. Think of this
as a section of a greater chapter of the QA process.

Tests will be collected in check that will be collected in a
section/chapter of the QA. Imagine that you want to conduct a number of
tests for table A1 from PRSQ; this will be later added to similar tests
for A2 etc. All of them will be combined in a chapter, this being one of
the files created within the `QA_that` folder mentioned at the beginning
of this guide.

In this way different developers will be able to communicate and code
the QA without affecting each others work. Each will work on separate
standalone chapters.

**IMPORTANT NOTE:** The `global_output` has a local scope within a check
and this is by design. This means that if you are looking for a concrete
variable to take forward you are looking for an output from a
`check_that()` function.

So in other words, the `check_that()` will provide the variable
necessary to later add to the summary functions before populating the
logs etc.

``` r


output33 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})
```

## QA results and summary view - logs

After completing a series of tests for a given section it is necessary
to construct a log as well as provide fast draft outputs so as to check
the status of the QA. The concept here is that if it fails I want to
know it as fast as possible so as to move on with a suitable solution.

Therefore, with regards to the fast output all functions are designed to
be *chatty* in the sense that there will be a suitable console output
that will give an indication whether something is wrong or not.

The function `summary_QA()` is designed to provide a log file for a more
methodical analysis as well as include an overall output to the console
that will indicate at a glance if something is wrong.

**IMPORTANT NOTE: ** As mentioned in the guidance, the function will
require a `QA_that` folder with the associated log sub folders to be
created. If the folder exists then operation will continue as normal
otherwise an error will pop up so please keep this in mind. In addition,
the function has support for either creating the folder structure on the
root directory or the user can supply a custom root path where folder
structure will be created.

If you run the function from the markdown here you will end up with an
error as there is no QA\_that folder in the directory where the markdown
vignette file is saved (unless the `use_QA()` function was executed from
the same markdown in which case the QA folder structure will reside
where the Rmd file is) \!\!

Currently, using R version 3.5.1, there is no easy way to detect the
root directory from an Rmd document. For some reason the `getwd()`
function will return the current path of the Rmd file instead. The
solution to that will require more coding that it is currently required
to just add an argument for a custom folder path.

``` r

# run this after the QA_that folder is created and that the functions run from that folder
# 
qa_overview = summary_QA(output44,output33, log_file_name = "log_that_other_file.txt")
```

## Examples - Putting it all together

The following code is an overview of trhe whole process. This will need
to run, ideally, from an R script located in the `QA_that` folder. If
you have to run it as it is (from the markdown here), please also
uncomment and run the `use_QA()` function as well otherwise the system
will not be able to find a QA folder and you will get an error \!

Furthermore, there is an option include in both use\_QA() and
summary\_QA() functions that allow for custom fodler paths to be
included. More information is available in the help files. The example
below illustrate how to use the arguments and manually specify the
paths.

``` r

## Assuming that the folder structure is created etc. 
## 
## 


use_QA(QAname = "QA_that_A_B_match.R")

output44 = check_that(check_desc = "numeric matching test 1", {test_is_equal(descr = "test description text", 11,11)
  test_is_equal(descr = "testing vectorization",c(1,2,2,3),c(1,2,3,5))
  test_identical(descr = "test description text", 12,12.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})


output33 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})



output45 = check_that(check_desc = "test round 3", {test_is_equal(descr = "test rounding equal 11 with 11.3 ", 11,11.3, rounding_digits = 0)
  test_is_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
  test_identical(descr = "test description text", 12,12.1)
  test_identical(descr = "check identical  that 10 i s equal to 10.2 ", 10,10.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")



})

qa_overview = summary_QA(output44,output33, output45, log_file_name = "log_that_other_file.txt")





## With a customized folder structure. 
## 
## 


file_info_list = use_QA( QA_logs_dir = file.path( "place_custom_root_directory_here","test_folder"), QA_file_name = "QA_that_A_B_match.R")

output44 = check_that(check_desc = "numeric matching test 1", {test_is_equal(descr = "test description text", 11,11)
  test_is_equal(descr = "testing vectorization",c(1,2,2,3),c(1,2,3,5))
  test_identical(descr = "test description text", 12,12.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})


output33 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})



output45 = check_that(check_desc = "test round 3", {test_is_equal(descr = "test rounding equal 11 with 11.3 ", 11,11.3, rounding_digits = 0)
  test_is_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
  test_identical(descr = "test description text", 12,12.1)
  test_identical(descr = "check identical  that 10 i s equal to 10.2 ", 10,10.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")



})

qa_overview = summary_QA(output44,output33, output45, log_file_name = "log_that_other_file.txt", QA_logs_dir = file_info_list$QA_logs_dir)
```
