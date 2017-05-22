# jsonmatch utils

#' @internal
isTruthyChar <- function(string) {
  if (is.character(string) && nchar(string) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
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
  nodots <- lapply(comps, function(comp) comp[comp != '.'])
  #print(nodots)
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
#' @return Character vector of class 'json'
#'
#' @internal
extractValueFromArrIndex <- function(json, index) {  # zero-indexed !!!
  stopifnot(isTruthyChar(json), is.numeric(index), index %% 1L == 0L)
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
#' @return Character vector of class 'json'
#' 
#' @internal
extractValueFromObjKey <- function(json, key) {
  stopifnot(isTruthyChar(json), isTruthyChar(key),
            grepl(paste0('"', key,'":'), json, perl=TRUE))
  chars <- strsplit(json, '')[[1]]
  beg <- pos <- regexpr(paste0('"', key,'":'), json)[1] + nchar(key) + 3L
  opbr <- 0L
  repeat {
    if (chars[pos] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    pos <- pos + 1L
    if (opbr == 0L) return(gsub('^"|"$', '',
                                substr(json, beg, pos - 1L),
                                perl=TRUE))
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