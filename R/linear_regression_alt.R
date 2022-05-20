#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
slr_gd <- function(dat, response, explanatory){

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  y <- dat %>%
    pull({{response}}) %>%
    scale() %>%
    data.matrix()

  x <- dat %>%
    pull({{explanatory}}) %>%
    scale()

  x <- cbind(1, x) %>%
    data.matrix()

  betas <- matrix(0, nrow = 2, ncol = 1)
  error <- 1
  learning <- 0.015
  iterations <-  0
  cutoff <- .01
  max_iterations <- 50

  while (error >= cutoff & iterations < max_iterations) {
    deriv <- ((2 * t(x)) %*% (y - x %*% betas))
    betas <- betas + deriv *learning
    error = sum(deriv^2)
    iterations <- iterations + 1
    print(iterations)
    print(betas)

    if(iterations > max_iterations){
      print("Too many iterations!")
    }
  }

  results <- data.table(t(betas))
  colnames(results) <- c("Intercept", explan_name)

  return(results)
}


#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#'@export
mlr_gd <- function(dat, response) {

  y <- dat %>%
    pull({{response}}) %>%
    scale() %>%
    data.matrix()
  x <- dat %>%
    select(-{{response}}) %>%
    scale()
  x <- cbind(1, x) %>%
    data.matrix()

  betas <- matrix(0, nrow = ncol(x), ncol = 1)
  error <- 99
  learning <- 0.015
  cutoff <-0.01
  iterations <-  0
  max_iterations <- 100

  while (error >= cutoff & iterations < max_iterations) {
    deriv <- ((2 * t(x)) %*% (y - x %*% betas))
    error <- sum(deriv^2)
    betas <- betas + learning * deriv
    iterations <- iterations + 1
    print(iterations)
    print(betas)

    if(iterations > max_iterations){
      print("Too many iterations!")}
  }

  results <- t(betas) %>%
    data.table()

  names(results)[1] <- "Intercept"
  return(results)
}

#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#'@export
mlr_qr <- function(dat, response) {

  y <- dat %>%
    pull({{response}}) %>%
    data.matrix()
  x <- dat %>%
    select(-{{response}})
  x <- cbind(1, x) %>%
    data.matrix()

  QR <- qr(x)
  results <- t(solve.qr(QR, y)) %>%
    data.table()

  names(results)[1] <- "Intercept"

  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}
