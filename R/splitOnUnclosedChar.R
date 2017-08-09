#' Splits a string on given character neither enclosed in brackets nor
#' double quotes
#'
#' @param string character. Vector of length 1.
#' @param peep character. Single character to split on. Cannot be any of
#' \code{[]{}\"}.
#' @param keep logical. Keep \code{peep}?
#' @return character. Vector of splits.
#'
#' @keywords internal
splitOnUnclosedChar <- function(string, peep, keep=FALSE) {
  stopifnot(isTruthyChr(string), isTruthyChr(peep), nchar(peep) == 1L,
            is.logical(keep))
  # split to single characters
  chars <- strsplit(string, '', fixed=TRUE)[[1L]]
  # setup
  accu <- vector('character')
  BRACKETS <- list(OPEN=c('[', '{'), CLOSE=c(']', '}'))
  opbr <- 0L      # if opbr is zero we r not in a struct
  qtct <- 0L      # if even we r not in a string
  last.cut <- 0L  # tracks last slice index
  i <- 1L
  prev <- chars[1L]
  # peep through
  for (char in chars) {
    if (char %in% BRACKETS$OPEN) opbr <- opbr + 1L
    if (char %in% BRACKETS$CLOSE) opbr <- opbr - 1L
    if (char == '"' && prev != '\\') qtct <- qtct + 1L
    if (char == peep && (opbr == 0L && qtct %% 2L == 0L)) {
      if (!keep) {
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
      } else {  # keep split character
        # get pre
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
        last.cut <- i - 1L
        # get split character
        accu <- append(accu, substr(string, last.cut + 1L, last.cut + 1L))
      }
      last.cut <- i
    }
    i <- i + 1L
    prev <- char
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
}
