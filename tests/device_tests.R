##File to test the functionality of Device Object
context("checking functionality of Device")



#testing check_sides with valid
expect_true(check_sides(c("red", "blue", "green")))

#error testing for check_sides
expect_error(check_sides(c()))
expect_error(check_sides(c("blue", "green", "blue")))

#testing check_length with valid
expect_true(check_length(c("blue", "green"), prob= c(1, .8)))

#error testing for check_length
expect_error(check_length(c("blue", "green"), c(1)))

tester <- device()
#tests for fair dice
expect_equal(check_sides(tester$sides), TRUE)
expect_equal(check_prob(tester$prob), TRUE)
expect_equal(check_lengths(tester$prob, tester$sides), TRUE)

#testing for check_prob with valid

expect_true(check_prob(c(.2, .1)))

#error test for check_prob
expect_error(check_prob(c(.2, 11)))
expect_error(check_prob(c("a", "b")))

#testing the object constructer

expect_error(device(sides = c("blue", "green"), prob = c(1,1)))
expect_error(device(sides = c("blue", "blue"), prob = c(0.5,-1)))
expect_error(device(sides = c("blue", "green"), prob = c(1)))

