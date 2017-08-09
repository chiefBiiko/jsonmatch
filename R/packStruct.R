#' Packs atomic data in arrays, optionally adding keys
#'
#' @param accu Character vector of target values (JSON extracts).
#' @param json Input JSON string.
#' @param paths Subset pattern paths.
#' @return Character vector.
#'
#' @keywords internal
packStruct <- function(accu, json, paths) {
  stopifnot(is.character(accu), isTruthyChrVec(json), isTruthyChrVec(paths))
  rtn <- keys <- vector('character')
  i <- vector('integer')
  if (length(accu) > 1L) {
    if (isArray(json)) {          # base array
      rtn <- paste0('[',
                    paste0(sapply(accu, boxjson::boxAtoms, USE.NAMES=FALSE),
                           collapse=','),
                    ']')
    } else if (isObject(json)) {  # base object
      i <- 0L
      keys <- sub('^\\.', '', paths, perl=TRUE)
      rtn <- paste0('{',
                    paste0(sapply(accu, function(a) {
                      i <<- i + 1L
                      paste0(paste0('"', keys[i], '"', ':'),
                             boxjson::boxAtoms(a))
                    }, USE.NAMES=FALSE),
                    collapse=','),
                    '}')
    }
  } else if (length(accu) == 1L) {
    rtn <- boxjson::boxAtoms(accu, strict=FALSE)
  }
  # serve
  return(rtn)
}
