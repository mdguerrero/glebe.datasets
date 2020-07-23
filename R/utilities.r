#' Randomly create a correlation coefficient.
#'
#' This function randomly creates a correlation coefficient.
#'
#' @export
#'
#' @param size A character element (options: "w" for weak, "s" for small, "m" for moderate or "l" for large).
#' @param direction A character element (options: "p" for positive or "n" for negative) This paramater is set to "p" by default.
#' @param seed_number A numeric (integer) element that allows a randomly generated effect size to be reproduced. This parameter is set to 1 by default.
#'
#'
#' @return Returns a numeric element between -0.70 and 0.70.
get_correlation_coefficent <- function(size, direction = "p", seed_number = 1) {
  set.seed(seed_number)
  if(size == "w") {
    output <- sample(0:10, 1) * .01
  } else if(size == "s") {
    output <- sample(11:29, 1) * .01
  } else if(size == "m") {
    output <- sample(30:49, 1) * .01
  }  else if(size == "l") {
    output <- sample(50:70, 1) * .01
  } else {
    output <- NA
  }
  if(is.na(output)) {
    return(NA)
  } else if(direction == "n") {
    return(output * -1)
  } else {
    return(output)
  }
}