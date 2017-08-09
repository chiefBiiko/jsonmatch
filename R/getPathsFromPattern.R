#' Splits pattern to paths while handling the wildcard
#'
#' @param json JSON string.
#' @param pattern Subset pattern.
#' @return Chr vector.
#'
#' @keywords internal
getPathsFromPattern <- function(json, pattern) {
  stopifnot(isTruthyChrVec(json), isTruthyChrVec(pattern))
  # selectors split
  selectors <- Filter(function(p) p != '',
                      strsplit(pattern, ',', fixed=TRUE)[[1L]])
  # make a flat vector
  rtn <- unlist(lapply(selectors, function(s) {
    if (grepl('\\*', s, perl=TRUE)) {  # GET ALL *matches in a vector
      handleWildcard(json, s)
    } else if (grepl('\\[\\d+\\:\\]', s, perl=TRUE)) {  # NEW
      handleTrailingColon(json, s)                      # NEW
    } else {
      s
    }
  }))
  # split 4 multiple array indices                      # NEW NEW
  if (any(grepl('\\[\\d+\\:\\d+\\][^\\s]+', rtn, perl=TRUE))) {
    rtn <- as.vector(sapply(as.list(rtn), function(trail) {
      if (grepl('\\[\\d+\\:\\d+\\]', trail, perl=TRUE)) {
        parsed.seq <- eval(parse(text=sub('^[^\\[]*\\[(\\d+\\:\\d+)\\].*$',
                                          '\\1', trail, perl=TRUE)))
        sapply(as.list(parsed.seq), function(digit) {
          sub('\\[\\d+\\:\\d+\\]', paste0('[', digit, ']'), trail, perl=TRUE)
        })
      } else {
        trail
      }
    }), mode='character')
  }
  # serve
  return(rtn)
}
