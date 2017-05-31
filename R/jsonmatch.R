# jsonmatch

# TODO: -adjust verifyPatternSyntax for below GOTCHA - DONE
#       -jsonlite::validate input JSON - PENDING
#       -rearrange packaging - PENDING
#       -fix boxjson not boxing unclosed atoms - DONE
#       -fix matching arrays - DONE
#       -erase all \\s in input json - DONE
#       -use non-capturing regex groups where possible - DONE
#       -allow wildcard matching for obj.props - DONE
#       -adjust verifyPatternSyntax 4 wildcards - DONE
#       -allow multiple wildcards in one property reference - DONE
#       -setup jsonbox to handle boxing of atomic data and auto_unbox - DONE
#       -allow file references - DONE
#       -work on matching multi-D arrays - DONE
#       -checkstop that pattern is valid - DONE
#       -SUGAR: allow open ended array indexing with a trailing colon [0:]
#       -write a cli 4 jsonmatch

# GOTCHA: Wildcard matching can only be used if all object keys in the input 
#         JSON string contain alphanumeric characters [a-zA-Z0-9] only.

#' Simple matching on JSON
#' 
#' Subset JSON without parsing it to its equivalent R representation.
#' 
#' @details Parameter \code{pattern} allows matching keys of a JSON 
#' object \code{.key}, and keys/indices of a JSON array \code{[0,3,5] or [0:5]}.
#' ...
#' 
#' @export
jsonmatch <- function(json, pattern, auto_unbox=FALSE) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # mutate input
  json <- mutateInputJSON(json)
  # do a syntax check
  if (!verifyPatternSyntax(json, pattern)) stop('invalid pattern syntax')
  # split pattern to paths
  paths <- getPathsFromPattern(json, pattern)
  # get keys from paths
  keys <- getKeysFromPaths(paths)
  # extraduce target value(s)
  accu <- vector('character', length(keys))
  for (i in seq(length(keys))) {
    curr <- json                               # reduction base
    # reduce curr to target value
    for (key in keys[[i]]) {
      if (is.character(key)) {                 # chr object keys
        curr <- extractValueFromObjKey(curr, key)
      } else {                                 # numeric array indices
        curr <- extractValueFromArrIndex(curr, key)
      }
    }
    accu[i] <- curr                            # store target value
  }
  # package
  rtn <- 
  if (length(accu) > 1L) {         # case multiple strings in accu
    if (grepl('^\\[.*\\]$', json, perl=TRUE)) {         # base array
      paste0('[', paste0(packStruct(accu), collapse=','), ']')
    } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {  # base object
      paste0('{', 
             paste0(packStruct(accu, sub('^\\.', '', paths, perl=TRUE)), 
                    collapse=','), 
             '}')
    }
  } else {
    boxjson::boxAtoms(accu)
  }
  # boxing
  if (auto_unbox) rtn <- boxjson::unboxAtoms(rtn)
  return(structure(rtn, class='json'))
}
