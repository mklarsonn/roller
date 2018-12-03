#' @title device.R
#' @description create an object "device"
#' @param sides vector naming the sides of object
#' @param prob numeric vector of probabilities for each side
#' @return an object of class \code{"device"}
#' @export




#The function check_sides determines whether the sides vector is valid
#'@export
check_sides <- function(sides){
  if (length(sides) <1){
    stop("error:sides must be a vector with at least 1 element")
  }
  if (any(duplicated(sides))){
    stop("error:all sides must be unique")

  }
  else{
    TRUE
  }}

#The function check_prob determines whether the prob vector is valid
#' @export
check_prob <- function(prob){
  if (!length(prob) <1){
    stop("prob vector must have at least one element")
  }
  if (!is.numeric(prob)){
    stop("prob vector must be of type numeric")
  }
  if (any(is.na(prob))){
    stop("prob vector cannot contain any NA values")
  }
  if (any(prob<0) | any(prob>1)){
    stop("prob vector must have values between 0 and 1")
  }
  if (sum(prob) != 1){
    stop("the elements of prob must sum to 1")
  }
  else{
    TRUE}
}

#check_length checks that both the sides and prob vector have the same length
#' @export
check_length <- function(sides, prob){
  if (length(sides) != length(prob)){
    stop("error:sides and probability must have the same length")
  }
  else{
    TRUE
  }
}

#the main function of this object, which returns the new object of class device
#' @export
device <- function(sides = c(1, 2), prob = c(.5, .5)){
  check_prob(prob)
  check_sides(sides)
  check_lengths(sides,prob)
  return_dev <- list(sides = sides, prob = prob)
  class(return_dev) <- "device"
  return(return_dev)
}


#print.device prints out the contents of device in a table
#' @export
print.device <- function(devy, ...){
  cat('object "device"\n\n')
  for (i in 1:length(devy$sides)){
    cat(sprintf('"%s", p = %s', devy$sides[i], devy$prob[i], "\n"))
  }
  invisible(devy)
}


#is.device tests whether or not an input is valid
#' @export
is.device <- function(x){
  is(devy, 'device')
}

