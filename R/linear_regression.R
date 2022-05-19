#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>%
    pull({{explanatory}})
  y <- dat %>%
    pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  ### Edit code after here

  x_mat <- cbind(1, x) %>%
    data.matrix()

  betas <- solve(crossprod(x_mat)) %*% (t(x_mat) %*% data.matrix(y))

  beta_0 <- betas[1]
  beta_1 <- betas[2]

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}

#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#'@export
#'
multiple_linear_regression <- function(dat, response, method = NULL) {
  y <- data.matrix(dat %>%
                     pull({{response}}))
  x <- dat %>%
    select(-{{response}})

  x <- cbind(1, x) %>%
    as.matrix()

  coefs <- solve(crossprod(x)) %*% (t(x) %*% y)

  df_coefs <- data.table(t(coefs))

  colnames(df_coefs)[1] <- "Intercept"

  return(df_coefs)

}
