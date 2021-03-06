# jsonmatch

# TODO: -vectorize things for colon and wildcard matching - DONE
#       -fix boxing of array siblings - DONE
#       -update code with new utils - DONE
#       -jsonlite::validate input JSON - DONE
#       -adjust verifyPatternSyntax for messages - DONE
#       -rearrange packaging - DONE
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
#       -SUGAR: allow open ended array indexing with a trailing colon [0:] - DONE
#       -write a cli 4 jsonmatch

#' Simple matching on JSON
#'
#' Subset JSON without parsing it to its equivalent R representation.
#'
#' @param json Character. In-memory \code{JSON} string or filename.
#' @param pattern Character. Subset pattern.
#' @param auto_unbox Logical. Unbox \code{JSON}? Default: \code{FALSE}.
#' @param strict Logical. Only allow valid \code{JSON}? Default: \code{TRUE}.
#' @return JSON string.
#'
#' @details Parameter \code{pattern} allows matching keys of a JSON
#' object \code{.key,.k*y}, and keys/indices of a JSON array
#' \code{[0,3,5], [0:5], [0:]}.
#' ...
#'
#' @export
jsonmatch <- function(json, pattern, auto_unbox=FALSE, strict=TRUE) {
  stopifnot(isTruthyChrVec(json), isTruthyChrVec(pattern),
            is.logical(auto_unbox), is.logical(strict))
  # mutate json for safe processing
  json <- mutateInputJSON(json)
  # use strict
  if (strict && !jsonlite::validate(json)) stop('invalid json')
  # do a syntax check
  if (!(vps <- verifyPatternSyntax(json, pattern))) stop(attr(vps, 'msg'))
  # split pattern to paths
  paths <- getPathsFromPattern(json, pattern)
  # get keys from paths
  keys <- getKeysFromPaths(paths)
  # extraduce target value(s)
  accu <- vector('character', length(keys))
  for (i in seq_along(keys)) {
    curr <- json                                     # reduction base
    for (key in keys[[i]]) {                         # reduce curr to target
      if (is.character(key)) {                       # object keys
        curr <- extractValueFromObjKey(curr, key)
      } else if (is.numeric(key)) {                  # array indices
        curr <- extractValueFromArrIndex(curr, key)
      }
    }
    accu[i] <- curr                                  # store target value
  }
  # packing
  rtn <- packStruct(accu, json, paths)
  # boxing
  if (auto_unbox) rtn <- boxjson::unboxAtoms(rtn)
  # serving
  return(structure(rtn, class='json'))
}
