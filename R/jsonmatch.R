# jsonmatch

# TODO: -erase all \\s in input json - DONE
#       -use non-capturing regex groups where possible - DONE
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
  # split and transform subset pattern
  spl <- Filter(function(p) p != '', strsplit(pattern, ',', fixed=TRUE)[[1]])
  keys <- getKeysFromPattern(spl)
##print(keys)
  # iterate and reduce to target value(s)
  i <- 1L
  accu <- vector('character', length(keys))
  repeat {
    curr <- gsub('\\s+', '', json, perl=TRUE)  # reduction base
    # reduce curr to target value
    for (key in keys[[i]]) {
      if (is.character(key)) {                 # chr object keys
        curr <- extractValueFromObjKey(curr, key)
      } else {                                 # numeric array indices
        xtrc <- sapply(key, function(int) {
          extractValueFromArrIndex(curr, int)
        })
        # correctly quote string literals within json
        curr <- gsub('([^[:alpha:]])","([^[:alpha:]])', '\\1,\\2', 
                     paste0(xtrc, collapse='","'), perl=TRUE)
      }
    }
    accu[i] <- curr             # store target value
    i <- i + 1L                 # increment
    if (i > length(keys)) break  # trapdoor
  }
  # package and return
  rtn <- if (length(accu) > 1L) {  # case multiple target values
    if (grepl('^\\[.*\\]$', json, perl=TRUE)) {         # case array
      paste0('[', paste0(packAtoms(accu), collapse=','), ']')
    } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {  # case object
      paste0('{', 
             paste0(packAtoms(accu, gsub('^\\.', '', spl, perl=TRUE)), 
                    collapse=','), 
             '}')
    }
  } else {                         # case single target value
    packAtoms(accu)
  }
  return(structure(rtn, class='json'))
}
