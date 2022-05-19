#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory){

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  y <- dat %>%
    pull({{response}}) %>%
    scale() %>%
    as.matrix()

  x <- dat %>%
    pull({{explanatory}}) %>%
    scale()

  x <- cbind(1, x) %>%
    as.matrix()

  betas <- matrix(0, nrow = 2, ncol = 1)
  error <- 99
  learning <- 0.00001

  while (error >= 0.001) {
    deriv <- ((2 * t(x)) %*% (y - x %*% betas))
    error <- sum(deriv^2)
    change <- learning * deriv
    betas <- betas + change
  }

  beta_0 <- betas[1]
  beta_1 <- betas[2]

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

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
#'
#'@export
mlr_gd <- function(dat, response) {

  y <- dat %>%
    pull({{response}}) %>%
    scale() %>%
    as.matrix()
  x <- dat %>%
    select(-{{response}}) %>%
    scale()
  x <- cbind(1, x) %>%
    as.matrix()

  betas <- matrix(0, nrow = ncol(x), ncol = 1)
  error <- 99
  learning <- 0.00001

  while (error >= 0.00001) {
    deriv <- ((2 * t(x)) %*% (y - x %*% betas))
    error <- sum(deriv^2)
    change <- learning * deriv
    betas <- betas + change
  }

  results <- t(betas) %>%
    as.data.frame()

  names(results)[1] <- "Intercept"

  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `multiple_linear_regression`

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
#'
#'@export
mlr_qr <- function(dat, response) {

  y <- dat %>%
    pull({{response}}) %>%
    as.matrix()
  x <- dat %>%
    select(-{{response}})
  x <- cbind(1, x) %>%
    as.matrix()

  QR <- qr(x)
  results <- t(solve.qr(QR, y)) %>%
    as.data.frame()

  names(results)[1] <- "Intercept"

  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}
