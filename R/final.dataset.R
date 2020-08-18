# Clear global environment
rm(list = ls())

# Load packages
library(haven); library(scales); library(simstudy); library(write_xlsx)

# User-defined functions
get_correlation_coefficient <- function(size, direction = "p", seed_number = 1) {
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

export_dataset <- function(x, path = paste0(getwd(), "/", deparse(substitute(x))), type = "xlsx") {
  if(type == "spss") {
    if(! grepl(".sav", path)) {
      path <- paste0(path, ".sav")
    } 
    write_sav(x, path)
  } else {
    if(! grepl(".xlsx", path)) {
      path <- paste0(path, ".xlsx")
    } 
    write_xlsx(x, path)
  }
}

# Build covariance matrix
cov_mat <- data.frame(
  age = c(1, get_correlation_coefficient("w", "p", 1), get_correlation_coefficient("w", "p", 2), get_correlation_coefficient("m", "n", 4), get_correlation_coefficient("m", "n", 7), get_correlation_coefficient("s", "p", 22), get_correlation_coefficient("w", "p", 27), get_correlation_coefficient("w", "n", 33), get_correlation_coefficient("w", "p", 40), get_correlation_coefficient("s", "p", 48)),
  gender = c(get_correlation_coefficient("w", "p", 1), 1, get_correlation_coefficient("w", "p", 3), get_correlation_coefficient("s", "p", 5), get_correlation_coefficient("m", "p", 8), get_correlation_coefficient("s", "p", 23), get_correlation_coefficient("s", "p", 28), get_correlation_coefficient("w", "p", 34), get_correlation_coefficient("s", "p", 41), get_correlation_coefficient("s", "p", 49)),
  ethnicity = c(get_correlation_coefficient("w", "p", 2), get_correlation_coefficient("w", "p", 3), 1, get_correlation_coefficient("w", "p", 6), get_correlation_coefficient("w", "p", 9), get_correlation_coefficient("w", "p", 24), get_correlation_coefficient("w", "p", 29), get_correlation_coefficient("w", "p", 35), get_correlation_coefficient("s", "p", 42), get_correlation_coefficient("w", "p", 50)),
  vo2max = c(get_correlation_coefficient("m", "n", 4), get_correlation_coefficient("s", "p", 5), get_correlation_coefficient("w", "p", 6), 1, get_correlation_coefficient("s", "p", 10), get_correlation_coefficient("s", "n", 25), get_correlation_coefficient("s", "p", 30), get_correlation_coefficient("w", "p", 36), get_correlation_coefficient("l", "p", 43), get_correlation_coefficient("s", "n", 51)),
  mvpa = c(get_correlation_coefficient("m", "n", 7), get_correlation_coefficient("m", "p", 8), get_correlation_coefficient("w", "p", 9), get_correlation_coefficient("s", "p", 10), 1, get_correlation_coefficient("s", "n", 26), get_correlation_coefficient("s", "p", 31), get_correlation_coefficient("s", "p", 37), get_correlation_coefficient("s", "p", 44), get_correlation_coefficient("s", "n", 52)),
  sprint = c(get_correlation_coefficient("s", "p", 22), get_correlation_coefficient("s", "p", 23), get_correlation_coefficient("w", "p", 24), get_correlation_coefficient("s", "n", 25), get_correlation_coefficient("s", "n", 26), 1, get_correlation_coefficient("s", "p", 32), get_correlation_coefficient("w", "p", 38), get_correlation_coefficient("m", "n", 45), get_correlation_coefficient("m", "p", 53)),
  confidence = c(get_correlation_coefficient("w", "p", 27), get_correlation_coefficient("s", "p", 28), get_correlation_coefficient("w", "p", 29), get_correlation_coefficient("s", "p", 30), get_correlation_coefficient("s", "p", 31), get_correlation_coefficient("s", "p", 32), 1, get_correlation_coefficient("m", "p", 39), get_correlation_coefficient("s", "p", 46), get_correlation_coefficient("m", "n", 54)),
  wellbeing = c(get_correlation_coefficient("w", "n", 33), get_correlation_coefficient("w", "p", 34), get_correlation_coefficient("w", "p", 35), get_correlation_coefficient("w", "p", 36), get_correlation_coefficient("s", "p", 37), get_correlation_coefficient("w", "p", 38), get_correlation_coefficient("m", "p", 39), 1, get_correlation_coefficient("w", "p", 47), get_correlation_coefficient("s", "n", 55)),
  status = c(get_correlation_coefficient("w", "p", 40), get_correlation_coefficient("s", "p", 41), get_correlation_coefficient("s", "p", 42), get_correlation_coefficient("l", "p", 43), get_correlation_coefficient("s", "p", 44), get_correlation_coefficient("m", "n", 45), get_correlation_coefficient("s", "p", 46), get_correlation_coefficient("w", "p", 47), 1, get_correlation_coefficient("s", "n", 56)),
  bmi = c(get_correlation_coefficient("s", "p", 48), get_correlation_coefficient("s", "p", 49), get_correlation_coefficient("w", "p", 50), get_correlation_coefficient("s", "n", 51), get_correlation_coefficient("s", "n", 52), get_correlation_coefficient("m", "p", 53), get_correlation_coefficient("m", "n", 54), get_correlation_coefficient("s", "n", 55), get_correlation_coefficient("s", "n", 56), 1)
)

# Add row names that are identical to column names (nice to have when viewing the covariance matrix)
row.names(cov_mat) <- colnames(cov_mat)

# Generate correlated dataset
d <- genCorData(n, mu = c(25, 0.5, 1, 42, 4, 13, 4.5, 3.8, 0.1, 22), sigma = c(2, 0.1, 3, 3, 1, 2, 1.1, 1.1, 1, 1.5), corMatrix = unname(as.matrix(cov_mat)), cnames = "age,gender,ethnicity,vo2max,mvpa,sprint,confidence,wellbeing,status,bmi")

# Data wrangling
d$age <- round(rescale(d$age, to = c(17, 30)))
d$gender <- round(d$gender)
d$ethnicity <- round(rescale(d$ethnicity, to = c(1, 5)))
d$mvpa <- round(rescale(d$mvpa, to = c(0, 7)))
d$sprint <- rescale(d$sprint, to = c(10, 20))
d$confidence <- rescale(d$confidence, to = c(1, 7))
d$wellbeing <- rescale(d$wellbeing, to = c(1, 5))
d$status <- rescale(d$status, to = c(0, 1))
d$status <- ifelse(d$status < 0.6, 0, 1)

# More data wrangling
d$gender <- factor(d$gender, levels = 0:1, labels = c("Female", "Male"))
d$ethnicity <- factor(d$ethnicity, levels = 1:5, labels = c("Asian", "Hispanic", "Caucasian", "African-American", "Multiracial"))
d$status <- factor(d$status, levels = 0:1, labels = c("Recreational athlete", "Elite Athlete"))
d <- as.data.frame(d)
d <- d[order(colnames(d))]
d <- d[, ! colnames(d) %in% "id"]

# Export data
export_dataset(d, type = "spss")