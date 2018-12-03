context("Testing the validity of the Rolls Constructor")

#test check_times with valid
expect_equal(check_times(100), TRUE)
expect_equal(check_times(2), TRUE)

#error testing check_times
expect_error(check_times(0))
expect_error(check_times(1.4))
