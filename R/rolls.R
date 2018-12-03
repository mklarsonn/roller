#' @title Roll
#' @description Create an object of class "roll"
#' @param device an object of class "device"
#' @param times number of rolls
#' @return an object of class "roll"
#' @return \item{rolls}{vector of rolls}
#' @return \item{sides}{vector of device \code{"sides"}}
#' @return \item{prob}{vector of device \code{"prob"}}
#' @return \item{total}{total number of rolls}
#' @export


roll <- function(device, times = 1) {
  if (!is(device, 'device')) {
    stop("device must be of class 'device'")
  }
  check_times(times)
  prob <- device$prob
  sides <- device$sides
  sample_rolls <- sample(sides, times, prob, replace = TRUE)
  total <- times
  rv <- list(rolls = sample_rolls, sides = sides, prob = prob, total = total)
  class(rv) <- "rolls"
  return(rv)
}


#check_times is a function to check whether times is valid
#'@export
check_times <- function(times){
  if (times < 1 | times %%1 != 0){
    stop("error:times must be an integer of at least 1")
  }
  else{
    TRUE
  }
}

#a way to better display the class object and their rolls
#'@export

print.rolls <- function(rolly, ...){
  cat('object "rolls"\n\n')
  cat('$rolls\n')
  print(rolly$rolls)
  invisible(rolly)
}

#' @title summary.rolls
#' @description returns the content of rolls in a dataframe
#' @param rolled an object of rolls
#' @return a dataframe with the summary of rolls containing
#' four elements
#' @export
summary.rolls <- function(x, ...) {
  freqs <- data.frame(
    side = x$sides,
    count = as.numeric(table(x$rolls)),
    prop = as.numeric(table(x$rolls))/sum(table(x$rolls))
  )
  obj <- list(freqs = freqs)
  class(obj) <- "summary.roll"
  obj
}

#we now have a fuctin to show the class of summary.rolls
#'@export

print.summary.rolls <- function(roll_s, ...){
  cat('summary "rolls"\n\n')
  print(roll_s$freq)
  invisible(roll_s)
}


#' @title Add rolls
#' @description addition of more rolls
#' @param roll2 an object of type rolls
#' @param n how many more rolls to add
#' @return the new rolls object with updates
#' @export
"+.rolls" <- function(roll2, n) {
  roll2$rolls <- c(roll2$rolls, sample(roll2$sides, n,
                                       roll2$prob, replace = TRUE))
  roll2total <- roll2$total + n
  return(roll2)
}
#' @title Extract Method
#' @description extracts specified values from a roll
#' @param roll2 an object of rolls
#' @param n an index
#' @return the value at the ith index
#' @export
"[.rolls" <- function(roll2, n) {
  return(roll2$rolls[n])
}

#' @title Replace Method
#' @description replaces specified values from a roll
#' @param roll2 an object of rolls
#' @param index the index of the value to replace
#' @param value what will take its place
#' @return the new rolled object
#' @export
"[<-.rolls" <- function(roll2, index, value) {
  roll2$rolls[index] <- value
  return(roll2)
}


#' @title Plot Rolls
#' @description plots the relative frequencies of rolls object
#' @param roll_d an object of rolls
#' @return a plot of sides and their frequencies
#' @export

plot.rolls <- function(roll_d) {
  get_summary <- summary(roll_d)$freqs
  barplot(get_summary$prop, names.arg = get_summary$side,
          xlab = "sides", ylab = "Relative Frequencies",
          main = paste("Plot of Relative Frequencies",
                       roll_d$total, "rolls"))
}

