



output44 = check_that(check_desc = "numeric matching test 1", {test_equal(descr = "test description text", 11,11)
  test_equal(descr = "testing vectorization",c(1,2,2,3),c(1,2,3,5))
  test_identical(descr = "test description text", 12,12.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")})


output33 = check_that(check_desc = "text round 2", {test_match(descr = "test description match text", "DFD","DFD")})



output45 = check_that(check_desc = "test round 3", {test_equal(descr = "test rounding equal 11 with 11.3 ", 11,11.3, rounding_digits = 0)
  test_equal(descr = "check that 12 matches with 12.2 using rounding up", 12,12.4, rounding_digits = 0)
  test_identical(descr = "test description text", 12,12.1)
  test_identical(descr = "check identical  that 10 i s equal to 10.2 ", 10,10.1)
  test_match(descr = "checking that two test strings match", "DFDF","dfdf")



})






bb = summary_QA(output44,output33, output45, log_file_name = "log_that_other_file.txt")

