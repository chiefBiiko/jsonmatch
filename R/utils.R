# jsonmatch utils

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Performs a syntax check on pattern
#' 
#' @param json JSON string.
#' @param pattern Chr vector of length 1 specifying the subset pattern.
#' @return Logical indicating whether the syntax is correct.
#' 
#' @internal
verifyPatternSyntax <- function(json, pattern) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # root structure
  struct <- if (grepl('^\\[.*\\]$', json, perl=TRUE)) {  # case arr
    strsplit(pattern, '', fixed=TRUE)[[1]][1] == '['
  } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {     # case obj
    strsplit(pattern, '', fixed=TRUE)[[1]][1] == '.'
  }
  # check 4 invalid characters
  valid <- !grepl('[^\\.,:\\[\\]\\d[A-Za-z]]*', pattern, perl=TRUE)
  # early exit
  if (!struct || !valid) return(FALSE)
  # setup syntax check                                       # master regex
  rex <- '^(?:\\.[[:alnum:]]+)|^(?:\\[\\d+(?:,\\d+)*(?:\\:\\d+)*\\])'
  comps <- strsplit(pattern, ',', fixed=TRUE)[[1]]           # pattern components
  for (comp in comps) {                                      # do em all
    red <- comp                                              # reduction base
    repeat {                                                 # do predicate reduction
      if (!grepl(rex, red, perl=TRUE)) return(FALSE)    # check head
      red <- sub(rex, '', red, perl=TRUE)               # cut head
      if (nchar(red) == 0L) break                       # trapdoor
    }  
  }
  # exit
  return(TRUE)
}

#' Takes a split vector of dirty object keys and array indices and returns
#'  a 2d list providing symbols as required by \code{extractValueFrom*}.
#' 
#' @param split.pattern Character vector of jsonmatch-type key references.
#' @return 2d list providing symbols as required by \code{extractValueFrom*}.
#' 
#' @internal
getKeysFromPattern <- function(split.pattern) {
  stopifnot(is.character(split.pattern))
##print(split.pattern)
  # split to path components
  comps <- strsplit(split.pattern, 
                    '(?<=\\])(?=\\[)|(?<=[[:alnum:]])(?=\\[)|(?<=\\])(?=\\.)', 
                    perl=TRUE)
##print(comps)
  nodots <- lapply(comps, function(comp) {
    comp <- gsub('.', '', comp, fixed=TRUE)
    comp[comp != '']
  })
  subseq <- lapply(nodots, function(nd) {
    lapply(nd, function(d) {
      if (grepl('\\[[[:digit:],:]+\\]', d, perl=TRUE)) {
        eval(parse(text=sub('\\[([[:digit:],:]*)\\]', 'c(\\1)', d)))
      } else {
        d
      }
    })
  })
  return(subseq)
}

#' Extracts the item at given index of an JSON array
#'
#' @param arr JSON array.
#' @param index Array index/key for which to retrieve value, zero-indexed.
#' @return Character vector.
#'
#' @internal
extractValueFromArrIndex <- function(arr, index) {  # zero-indexed !!!
  stopifnot(isTruthyChr(arr), is.numeric(index), index %% 1L == 0L)
  # split arr contents on comma not enclosed in [] or {}
  # \\d and [:alpha:] on hold: '(?![^\\d]*+\\d)(?![^[A-Za-z]]*+[A-Za-z])'
  cospl <- strsplit(gsub('^\\[|\\]$', '', arr, perl=TRUE), 
                    ',(?![^\\[\\]]*+\\])(?![^\\{\\}]*+\\})',
                    perl=TRUE)[[1]]
  # strip qoutes at head and tail
  dqt <- gsub('^"|"$', '', cospl[index + 1L], perl=TRUE)
  # serve
  if (is.na(dqt)) stop('index out of bounds') else return(dqt)
}

#' Extracts the item at given key of an JSON object
#'
#' @param obj JSON object.
#' @param key Key for which to retrieve value.
#' @return Character vector.
#' 
#' @internal
extractValueFromObjKey <- function(obj, key) {
  stopifnot(isTruthyChr(obj), isTruthyChr(key),
            grepl(paste0('"', key,'":'), obj, perl=TRUE))
  chars <- strsplit(obj, '')[[1]]
  beg <- pos <- regexpr(paste0('"', key,'":'), obj)[1] + nchar(key) + 3L
  opbr <- 0L
  repeat {
    if (chars[pos] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    if (opbr == 0L) {
      return(gsub('^"|"$', '', substr(obj, beg, pos), perl=TRUE))
    }
    pos <- pos + 1L
  }
  stop('invalid JSON')
}

#' Packs atomic data in arrays, optionally adding keys
#'
#' @param accumulator Character vector of target values (JSON extracts).
#' @param keys Character vector of keys to prefix target values with,
#' if supplied must have same length as \code{accumulator}.
#' @return Character vector.
#'
#' @internal
packAtoms <- function(accumulator, keys) {
  stopifnot(is.character(accumulator),
            missing(keys) || is.character(keys))
  return(if (missing(keys)) {
    sapply(accumulator, function(a) {
      if (!grepl('^\\[|\\{.*\\]|\\}$', a, perl=TRUE)) {
        if (grepl('^[[:alpha:]]', a, perl=TRUE) &&
            !grepl('^null$|^false$', a, perl=TRUE)) {
          paste0('["', a, '"]') 
        } else {
          paste0('[', a, ']')
        }
      } else {
        a
      }
    }, USE.NAMES=FALSE) 
  } else {
    i <- 0L
    sapply(accumulator, function(a) {
      i <<- i + 1L
      if (!grepl('^\\[|\\{.*\\]|\\}$', a, perl=TRUE)) {
        if (grepl('^[[:alpha:]]', a, perl=TRUE) &&
            !grepl('^null$|^false$', a, perl=TRUE)) {
          paste0(paste0('"', keys[i], '"', ':'), '["', a, '"]')
        } else {
          paste0(paste0('"', keys[i], '"', ':'), '[', a, ']')
        }
      } else {
        paste0(paste0('"', keys[i], '"', ':'), a) 
      }
    }, USE.NAMES=FALSE)
  })
}