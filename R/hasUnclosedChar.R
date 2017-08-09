#' Does a string contain a character neither enclosed in brackets nor
#' double quotes?
#'
#' @param string character. Vector of length 1.
#' @param peep character. Single character to search for. Cannot be any of
#' \code{[]{}"}.
#' @return Logical.
#'
#' @keywords internal
hasUnclosedChar <- function(string, peep) {
  stopifnot(isTruthyChr(string), isTruthyChr(peep), nchar(peep) == 1L)
  # split to single characters
  chars <- strsplit(string, '')[[1L]]
  # setup
  BRACKETS <- list(OPEN=c('[', '{'), CLOSE=c(']', '}'))
  opbr <- 0L     # if opbr is zero we r not in a struct
  qtct <- 0L     # if even we r not in a string
  prev <- chars[1L]
  # peep through --- THIS VERSION IS COOL
  for (char in chars) {
    if (char %in% BRACKETS$OPEN) opbr <- opbr + 1L
    if (char %in% BRACKETS$CLOSE) opbr <- opbr - 1L
    if (char == '"' && prev != '\\') qtct <- qtct + 1L
    if (char == peep && (opbr == 0L && qtct %% 2L == 0L)) {
      return(TRUE)
    }
    prev <- char
  }
  return(FALSE)
}
