# jsonmatch

# TODO: -erase all \\s in input json - DONE
#       -use non-capturing regex groups where possible - DONE
#       -allow wildcard matching for obj.props - DONE
#       -adjust verifyPatternSyntax 4 wildcards - DONE
#       -allow multiple wildcards in one property reference - DONE
#       -setup jsonbox to handle boxing of atomic data
#       -allow file references
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
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # mutate json
  json <- gsub('\\s+', '', json, perl=TRUE)
  # do a syntax check
  if (!verifyPatternSyntax(json, pattern)) stop('invalid pattern syntax')
  # split pattern to paths
  paths <- getPathsFromPattern(json, pattern)
  # get keys from paths
  keys <- getKeysFromPaths(paths)
  # iterate and reduce to target value(s)
  i <- 1L
  accu <- vector('character', length(keys))
  repeat {
    curr <- json                               # reduction base
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
             paste0(packAtoms(accu, sub('^\\.', '', paths, perl=TRUE)), 
                    collapse=','), 
             '}')
    }
  } else {                         # case single target value
    packAtoms(accu)
  }
  return(structure(rtn, class='json'))
}
