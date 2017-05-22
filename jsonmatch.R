# jsonmatch

#' Simple matching on JSON
#' 
#' @details Parameter \code{pattern} allows matching keys of a JSON 
#' object \code{.key}, and keys/indices of a JSON array \code{[0,3] or [0:5]}.
#' 
#' 
#' @export
jsonmatch <- function(json, pattern) {
  stopifnot(isTruthyChar(json), isTruthyChar(pattern))
  # split and transform arg pattern
  spl <- Filter(function(p) p != '', strsplit(pattern, ',', fixed=TRUE)[[1]])
  tsp <- transformSubsetPattern(spl)
  # iterate and reduce to target value(s)
  i <- 1L
  accu <- vector('character', length(tsp))
  repeat {
    curr <- json  # reduction base
    # reduce curr to target value
    for (ii in 1L:length(tsp[[i]])) {
      if (is.character(tsp[[i]][[ii]])) {
        curr <- extractValueFromKey(curr, tsp[[i]][[ii]])
      } else {
        curr <- paste0(sapply(tsp[[i]][[ii]], function(int) {
          extractValueFromIndex(curr, int)
        }), collapse='')
      }
    }
    accu[i] <- curr             # store target value
    i <- i + 1L                 # increment
    if (i > length(tsp)) break  # trapdoor
  }
  # package and return
  return(structure(if (length(accu) > 1L) {
    if (grepl('^\\[.*\\]$', json, perl=TRUE)) {
      paste0('[', paste0(packAtoms(accu), collapse=','), ']')
    } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {
      paste0('{', paste0(packAtoms(accu, gsub('.', '', spl, fixed=TRUE)), 
                         collapse=','), '}')
    }
  } else {
    packAtoms(accu)
  }, class='json'))
}
