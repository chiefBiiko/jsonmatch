# jsonmatch

# TODO: -erase all \\s in input json and pattern
#       -use non-capturing regex groups where possible
#       -allow wildcard matching for obj.props
#       -write a command line version of jsonmatch
#       -work on matching multi-D arrays - DONE
#       -checkstop that pattern is valid - DONE

#' Simple matching on JSON
#' 
#' Subset JSON without parsing it to its equivalent R representation.
#' 
#' @details Parameter \code{pattern} allows matching keys of a JSON 
#' object \code{.key}, and keys/indices of a JSON array \code{[0,3,5] or [0:5]}.
#' ...
#' 
#' @export
jsonmatch <- function(json, pattern) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern), 
            verifyPatternSyntax(json, pattern))
  # split and transform arg pattern
  spl <- Filter(function(p) p != '', strsplit(pattern, ',', fixed=TRUE)[[1]])
  tsp <- transformSubsetPattern(spl)
  # iterate and reduce to target value(s)
  i <- 1L
  accu <- vector('character', length(tsp))
  repeat {
    curr <- json  # reduction base
    # reduce curr to target value
    for (key in tsp[[i]]) {
      if (is.character(key)) {  # either chr keys or numeric indices
        curr <- extractValueFromObjKey(curr, key)
      } else {                  # numeric array indices
        xtrc <- sapply(key, function(int) {
          extractValueFromArrIndex(curr, int)
        })
        # only quote string literals within json
        curr <- gsub('([^[:alpha:]])","([^[:alpha:]])', '\\1,\\2', 
                     paste0(xtrc, collapse='","'), perl=TRUE)
      }
    }
    accu[i] <- curr             # store target value
    i <- i + 1L                 # increment
    if (i > length(tsp)) break  # trapdoor
  }
  # package and return
  rtn <- if (length(accu) > 1L) {  # case multiple target values
    if (grepl('^\\[.*\\]$', json, perl=TRUE)) {         # case array
      paste0('[', paste0(packAtoms(accu), collapse=','), ']')
    } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {  # case object
      paste0('{', 
             paste0(packAtoms(accu, gsub('.', '', spl, fixed=TRUE)), 
                    collapse=','), 
             '}')
    }
  } else {                         # case single target value
    packAtoms(accu)
  }
  return(structure(rtn, class='json'))
}
