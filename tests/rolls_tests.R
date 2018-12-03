context("Testing the validity of the Rolls Constructor")

#test check_times with valid
expect_equal(check_times(100), TRUE)
expect_equal(check_times(2), TRUE)

#error testing check_times
expect_error(check_times(0))
expect_error(check_times(1.4))


regular_die <- device()
set_seed(125)
die_25 <- roll(fair_die, times = 25)

#testing the object roll

expect_equal(die_25$sides, c(1,2))
expect_equal(die_25$prob, c(.5, .5))
expect_equal(length(die_25$rolls, 25))

diff_die <- device(sides = 1:6, prob = rep(1/6, 6))
set.seed(123)
die_25rolls <- roll(diff_die, times = 25)

expect_equal(names(die_25rolls), c("rolls","sides","prob","total"))
expect_equal(length(die_25rolls$rolls), 25)
expect_equal(die_25rolls$total, 25)

#testing extract method
devic <- device(sides = c("blue", "green"), prob = c(0.5, 0.5))
roll_devic <- roll(devic, times = 20)
expect_equal(roll_devic[20], "blue")

#testing replace method
devic[20] <- "yellow"
expect_equal(length(roll_devic$rolls), 20)

#testing add function
devic_add <- roll_devic + 25
expect_equal(length(devic_add$rolls), 45)

