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
#' @param json JSON string
#' @param pattern Chr vector of length 1 specifying the subset pattern
#' @return Logical indicating whether the syntax is correct
#' 
#' @internal
verifyPatternSyntax <- function(json, pattern) {
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
  # setup syntax check
  rex <- '^(\\.[A-Za-z]+)|^(\\[\\d+(,\\d+)*(:\\d+)*\\])'  # master regex
  accu <- vector('logical')                               # accumulator
  comps <- strsplit(pattern, ',', fixed=TRUE)[[1]]        # pattern components
  for (i in 1L:length(comps)) {                           # do em all
    red <- comps[i]                                       # reduction base
    repeat {                                              # do predicate reduction
      append(accu, grepl(rex, red, perl=TRUE))        # check head
      red <- sub(rex, '', red, perl=TRUE)             # cut head
      if (nchar(red) == 0L) break                     # trapdoor
      if (!grepl(rex, red, perl=TRUE)) return(FALSE)  # invalid
    }  
  }
  # exit
  return(all(accu))
}

#' Takes a split vector of dirty object keys and array indices and returns
#'  a 2d list providing symbols as required by \code{extractValueFrom*}.
#' 
#' @param split.pattern Character vector of jsonmatch-type key references.
#' @return 2d list providing symbols as required by \code{extractValueFrom*}
#' 
#' @internal
transformSubsetPattern <- function(split.pattern) {
  comps <- strsplit(split.pattern, '(?=[[:alpha:]])', perl=TRUE)
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
#' @param json JSON array
#' @param index Array index/key for which to retrieve value, zero-indexed.
#' @return Character vector
#'
#' @internal
extractValueFromArrIndex <- function(json, index) {  # zero-indexed !!!
  stopifnot(isTruthyChr(json), is.numeric(index), index %% 1L == 0L)
  qdjson <- gsub('(\\d+|null|false)(?!")', '"\\1"', json, perl=TRUE)
  chars <- strsplit(qdjson, '')[[1]]
  CONT <- c('"', '[', '{')
  cnt <- 0L
  pos <- 2L  # first item is arrays opening bracket
  repeat {
    if (chars[pos] %in% CONT) {
      cnt <- cnt + 1L
      if (cnt %% 2L == 1L && (cnt - 1L) %/% 2L == index) {
        return(gsub('"(\\d+)"', '\\1',
                    gsub('^"|"$', '', 
                         sub('^(["\\]\\}][^"\\]\\}]*["\\]\\}]).*$', '\\1',
                             substr(qdjson, pos, nchar(qdjson)), 
                             perl=TRUE),
                         perl=TRUE), 
                    perl=TRUE))
      }
    }
    pos <- pos + 1L
  }
  stop('invalid JSON')
}

#' Extracts the item at given key of an JSON object
#'
#' @param json JSON object
#' @param key Key for which to retrieve value.
#' @return Character vector
#' 
#' @internal
extractValueFromObjKey <- function(json, key) {
  stopifnot(isTruthyChr(json), isTruthyChr(key),
            grepl(paste0('"', key,'":'), json, perl=TRUE))
  chars <- strsplit(json, '')[[1]]
  beg <- pos <- regexpr(paste0('"', key,'":'), json)[1] + nchar(key) + 3L
  opbr <- 0L
  repeat {
    if (chars[pos] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    if (opbr == 0L) {
      return(gsub('^"|"$', '', substr(json, beg, pos), perl=TRUE))
    }
    pos <- pos + 1L
  }
  stop('invalid JSON')
}

#' Packs atomic data in arrays, optionally adding keys
#'
#' @param accumulator Character vector of target values (JSON extracts)
#' @param keys Character vector of keys to prefix target values with,
#' if supplied must have same length as \code{accumulator}
#' @return Character vector
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