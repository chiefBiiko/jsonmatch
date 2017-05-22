# jsonmatch

# TODO: -erase all \\s in input json and let em know
#       -checkstop that pattern is valid

#' Simple matching on JSON
#' 
#' @details Parameter \code{pattern} allows matching keys of a JSON 
#' object \code{.key}, and keys/indices of a JSON array \code{[0,3,5] or [0:5]}.
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
        curr <- extractValueFromObjKey(curr, tsp[[i]][[ii]])
      } else {
        xtrc <- sapply(tsp[[i]][[ii]], function(int) {
          extractValueFromArrIndex(curr, int)
        })
        curr <- gsub('([^[:alpha:]])","([^[:alpha:]])', '\\1,\\2', 
                     paste0(xtrc, collapse='","'), perl=TRUE)
      }
    }
    accu[i] <- curr             # store target value
    i <- i + 1L                 # increment
    if (i > length(tsp)) break  # trapdoor
  }
  # package and return
  rtn <- if (length(accu) > 1L) {
    if (grepl('^\\[.*\\]$', json, perl=TRUE)) {
      paste0('[', paste0(packAtoms(accu), collapse=','), ']')
    } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {
      paste0('{', paste0(packAtoms(accu, gsub('.', '', spl, fixed=TRUE)), 
                         collapse=','), '}')
    }
  } else {
    packAtoms(accu)
  }
  return(structure(rtn, class='json'))
}
