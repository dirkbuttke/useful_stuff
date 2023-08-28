#' Calculates geometric adstock
#'
#' @param x A numeric vector.
#' @param lambda The carryover parameter to consider when calculating ad stock.
#' @param log If TRUE result will be returned as ln(. + 1).
#' @param fischer If TRUE initital ad stock will be estimated according to Fischer (XXX).
#' @param log_first If TRUE log will be calculated based on the flow variables.
#' @return The ad stock transformation of x
#' @examples
#' x <- runif(100, 1, 100)
#' adstock_x <- geometric_adstock(x, lambda = 0.8, log = TRUE, fischer = FALSE, log_first = FALSE)
#' @export
geometric_adstock <- function(x, lambda, log = FALSE, fischer = FALSE, log_first = FALSE) {

  n <- length(x)
  adstocked <- numeric(n)

  if (log_first) {
    x <- log(x + 1)
    x_avg_week <- log(mean(x[1:52]) + 1)
  } else {
    x_avg_week <- mean(x[1:52])
  }

  # Fischer Initialization
  if (fischer) {
    adstocked[1] <- lambda * x_avg_week/(1-lambda) + (1-lambda) * x[1]
  } else {
    adstocked[1] <- (1-lambda) * x[1]
  }

  # Adstock Calculation
  for (i in 2:n) {
    adstocked[i] <- (1-lambda) * x[i] + lambda * adstocked[i - 1]
  }

  # Logging based on the log parameter
  if (log && !log_first) {
    return(log(adstocked + 1))
  } else {
    return(adstocked)
  }
}
