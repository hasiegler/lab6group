#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#' @import data.table
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  x <- dat %>%
    select(-{{response}}) %>%
    scale()

  x <- cbind(1, x) %>%
    data.matrix()

  y <- dat %>%
    pull({{response}}) %>%
    data.matrix()

  get_betas <- function(x, y, lambda){
    my_results <- t(solve(t(x) %*% x + lambda * diag(ncol(x))) %*% (t(x) %*% y)) %>%
      data.table()
    return(my_results)
  }

  results <- furrr::future_map_dfr(lambda, ~get_betas(x, y, .x))

  names(results)[1] <- "Intercept"

  results <- cbind(results,lambda)

  return(results)
}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#' @import data.table
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambda) {

  x <- train_dat %>%
    select(-{{response}}) %>%
    scale()

  x <- cbind(1, x) %>%
    data.matrix()

  y <- train_dat %>%
    pull({{response}}) %>%
    data.matrix()
  test <- test_dat %>%
    pull({{response}}) %>%
    data.matrix()

  get_sse <- function(x, y, y_test, lambda) {

    betas <- t(solve(t(x) %*% x + lambda * diag(ncol(x))) %*% (t(x) %*% y))
    preds <- x %*% t(betas)
    results <- cbind(test, preds) %>%
      data.table()

    sse <- results %>%
      mutate(sqr_error = ((test - preds)^2)) %>%
      summarize(error = sum(sqr_error))
    df_sse <- cbind(lambda, sse) %>%
      data.table()

    return(df_sse)
  }

  lambda_errors <- furrr::future_map_dfr(lambda, ~get_sse(x, y, test, .x))

  return(lambda_errors)

}
