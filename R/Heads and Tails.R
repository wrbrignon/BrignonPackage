#' Heads and Tails
#'
#' displays first and last 5 lines of dater
#' @param x dater
#' @return first and last 5 lines of dater
#' @export

ht <- function (x) rbind(head(x, 5), tail(x, 5))

