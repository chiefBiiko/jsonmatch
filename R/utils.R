# jsonmatch utils

#' Is character vector with number of characters >= 1?
#'
#' @param x R object.
#' @return Logical.
#'
#' @keywords internal
isTruthyChr <- function(x) {
  if (is.character(x) && nchar(x) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Is JSON an array?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isArray <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\[.+\\]$', json, perl=TRUE))
}

#' Is JSON an object?
#'
#' @param json JSON string.
#' @return Logical.
#'
#' @keywords internal
isObject <- function(json) {
  stopifnot(isTruthyChr(json))
  return(grepl('^\\{.+\\}$', json, perl=TRUE))
}

#' Strips an array's outer brackets
#'
#' @param json JSON array.
#' @return Stripped JSON array.
#'
#' @keywords internal
stripArray <- function(json) {
  stopifnot(isTruthyChr(json))
  if (isArray(json)) {
    return(gsub('^\\[|\\]$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}

#' Strips an object's outer brackets
#'
#' @param json JSON object.
#' @return Stripped JSON object.
#'
#' @keywords internal
stripObject <- function(json) {
  stopifnot(isTruthyChr(json))
  if (isObject(json)) {
    return(gsub('^\\{|\\}$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}

#' Mutates input JSON for safe processing
#'
#' @param json Input JSON.
#' @return JSON string.
#'
#' @keywords internal
mutateInputJSON <- function(json) {
  stopifnot(isTruthyChr(json))
  # allow file references
  if (file.exists(json)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  # boxing
  if (boxjson::hasUnboxedAtom(json)) json <- boxjson::boxAtoms(json)
  return(json)  # serve
}

#' Splits a string on given character neither enclosed in brackets nor
#' double quotes
#'
#' @param string Character vector of length 1L.
#' @param character Single character to split on.
#' @return Chr vector.
#'
#' @keywords internal
splitOnUnclosedChar <- function(string, char, keep=FALSE) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L,
            is.logical(keep))
  # split to single characters
  chars <- strsplit(string, '', fixed=TRUE)[[1L]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  last.cut <- 0L    # tracks last slice index
  accu <- vector('character')
  # peep through
  for (i in seq(length(chars))) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char &&
        (opbr == 0L && opqt %% 2L == 0L  &&
         all(unlist(nsqt) %% 2L == 0L))) {
      if (!keep) {
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
      } else {  # keep split character
        # get pre
        accu <- append(accu, substr(string, last.cut + 1L, i - 1L))
        last.cut <- i - 1L
        # get split character
        accu <- append(accu, substr(string, last.cut + 1L, last.cut + 1L))
      }
      last.cut <- i
    }
  }
  # consume remainder
  if (last.cut < nchar(string))  {
    accu <- append(accu, substr(string, last.cut + 1L, nchar(string)))
  }
  # serve
  return(accu)
}
#' Performs a syntax check on pattern
#'
#' @param json JSON string.
#' @param pattern Chr vector of length 1 specifying the subset pattern.
#' @return Logical indicating whether the syntax is correct.
#'
#' @keywords internal
verifyPatternSyntax <- function(json, pattern) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # root structure
  struct <- if (isArray(json)) {  # case arr
    identical(strsplit(pattern, '', fixed=TRUE)[[1L]][1L], '[')
  } else if (isObject(json)) {     # case obj
    identical(strsplit(pattern, '', fixed=TRUE)[[1L]][1L], '.')
  }
  # check 4 invalid characters ...
  valid <- !grepl('[^[[:print:]]]*', pattern, perl=TRUE)  # chr class locked
  # ... when using the magic wildcard
  wild <- !(grepl('\\*', pattern, perl=TRUE) &&
            any(grepl('[^[:alnum:]]',                     # chr class locked
                      gsub('^"|"\\:$', '',
                           regmatches(json,
                                      gregexpr('"[^"]*"\\:',
                                               json,
                                               perl=TRUE))[[1]],
                           perl=TRUE),
                      perl=TRUE)))
  # early exit
  if (!struct) {
    return(structure(FALSE, msg='Incorrect entry reference in pattern'))
  }
  if (!valid) {
    return(structure(FALSE,
                     msg='input JSON contains non-printable characters'))
  }
  if (!wild) {
    return(structure(FALSE,
                     msg=paste('wildcards can only be used if all object',
                               'keys in json contain alphanumeric',
                               'characters [a-zA-Z0-9] only')))
  }
  # setup syntax check                                    # master regex
  rex <- paste0('^(?:\\.\\*?[[:print:]]*\\*?[[:print:]]*)+|',
                '^(?:\\[\\d+(?:,\\d+)*(?:\\:(?!\\d*\\:)(?:\\d+)?)*\\])')
  comps <- strsplit(pattern, ',', fixed=TRUE)[[1]]        # pattern components
  for (comp in comps) {                                   # do em all
    red <- comp                                           # reduction base
    while (nchar(red) > 0L) {                           # do predicate reduction
      if (!grepl(rex, red, perl=TRUE)) {                # check head
        return(structure(FALSE, msg='syntax error'))    # spotted an error
      }
      red <- sub(rex, '', red, perl=TRUE)               # cut head
    }
  }
  # exit
  return(TRUE)
}

#' Splits pattern to paths while handling the wildcard
#'
#' @param json JSON string.
#' @param pattern Subset pattern.
#' @return Chr vector.
#'
#' @keywords internal
getPathsFromPattern <- function(json, pattern) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # selectors split
  selectors <- Filter(function(p) p != '',
                      strsplit(pattern, ',', fixed=TRUE)[[1L]])
  # make a flat vector
  rtn <- unlist(lapply(selectors, function(s) {
    if (grepl('\\*', s, perl=TRUE)) {  # GET ALL *matches in a vector
      handleWildCard(json, s)
    } else if (grepl('\\[\\d+\\:\\]', s, perl=TRUE)) {  # NEW
      handleTrailingColon(json, s)                      # NEW
    } else {
      s
    }
  }))
  # split 4 multiple array indices                      # NEW NEW
  if (any(grepl('\\[\\d+\\:\\d+\\][^\\s]+', rtn, perl=TRUE))) {
    rtn <- as.vector(sapply(as.list(rtn), function(trail) {
      if (grepl('\\[\\d+\\:\\d+\\]', trail, perl=TRUE)) {
        parsed.seq <- eval(parse(text=sub('^[^\\[]*\\[(\\d+\\:\\d+)\\].*$',
                                          '\\1', trail, perl=TRUE)))
        sapply(as.list(parsed.seq), function(digit) {
          sub('\\[\\d+\\:\\d+\\]', paste0('[', digit, ']'), trail, perl=TRUE)
        })
      } else {
        trail
      }
    }), mode='character')
  }
  # serve
  return(rtn)
}

#' Fixes a trailing colon in array selectors
#'
#' @param json JSON string.
#' @param selector Object selector/path containing a trailing colon.
#' @return Chr vector.
#'
#' @keywords internal
handleTrailingColon <- function(json, selector) {
  stopifnot(isTruthyChr(json), isTruthyChr(selector))
  # regex 2 match the trailing colon part of the selector
  rex.colon.key <- '\\[\\d+\\:\\]'
  # extract the trailing colon part of the selector
  colon.key <- regmatches(selector, regexpr(rex.colon.key, selector, perl=TRUE))
  # get prefix of colon indexer
  pre <- if ((pos <- regexpr(colon.key, selector, fixed=TRUE)[1L]) > 1L) {
    substr(selector, 1L, pos - 1L)
  } else {
    ''
  }
  # array atoms to get length from
  arr.atoms <- stripArray(if (pre != '') jsonmatch(json, pre) else json)
  # get the array length - with base zero
  arr.len <- length(splitOnUnclosedChar(arr.atoms, ',')) - 1L
  # construct a complete array indexer
  xolon.key <- paste0(substr(colon.key, 1L, nchar(colon.key) - 1L),
                      as.character(arr.len),
                      ']')
  # swap original key 4 xolon.key
  glued <- sub(colon.key, xolon.key, selector, fixed=TRUE)
  # check 4 remainders
  clued <- sapply(glued, function(gk) {
    if (grepl('\\[\\d+\\:\\]', gk, perl=TRUE)) {
      handleTrailingColon(json, gk)
    } else {
      gk
    }
  }, USE.NAMES=FALSE)
  # serve
  clued
}

#' Returns a chr vector of wildcard matched object keys
#'
#' @param json JSON string.
#' @param selector Object selector/path containing the wildcard character.
#' @return Chr vector.
#'
#' @keywords internal
handleWildCard <- function(json, selector) {
  stopifnot(isTruthyChr(json), isTruthyChr(selector))
  # regex 2 match the wildcard part of the selector
  rex.wdcd.key <- paste0('\\.[[:alnum:]]+\\*[[:alnum:]]+|',  # <- this case 1st
                         '\\.[[:alnum:]]+\\*|',
                         '(?:\\.\\*[[:alnum:]]+\\*?)+|',
                         '^\\.\\*$')
  # extract the wildcard part of the selector
  wdcd.key <- regmatches(selector, regexpr(rex.wdcd.key, selector, perl=TRUE))
  # get prefix of wdcd.key
  pre <- if ((pos <- regexpr(wdcd.key, selector, fixed=TRUE)[1]) > 1L) {
    substr(selector, 1L, pos - 1L)
  } else {
    ''
  }
  # object context where to look for wildcard matches
  obj.ctx <- gsub('(?:^\\{|^\\[)|(?:\\}$|\\]$)', '',
                  if (!pre %in% c('', '.')) jsonmatch(json, pre) else json,
                  perl=TRUE)
  # make regex from wdcd.key expression
  rex.mtch.keys <- paste0('(?:"', sub('\\*', '[[:alnum:]]+',
                                      sub('^\\.', '', wdcd.key, perl=TRUE),
                                      perl=TRUE),
                          '"\\:)(?![^\\{]*\\})(?![^\\[]*\\])')  # keys on top level only
  # get all matching keys
  mtch.keys <- paste0('.',
                      gsub('["\\:]', '',
                           regmatches(obj.ctx,
                                      gregexpr(rex.mtch.keys,
                                               obj.ctx,
                                               perl=TRUE))[[1L]],
                           perl=TRUE))
  # glue things back together
  glued <- sapply(mtch.keys, function(mk) sub(wdcd.key, mk, selector, fixed=TRUE),
                  USE.NAMES=FALSE)
  # check if any wildcards remained - conditionally repeat
  clued <- sapply(glued, function(gk) {
    if (grepl('\\*', gk, perl=TRUE)) {
      handleWildCard(json, gk)
    } else {
      gk
    }
  }, USE.NAMES=FALSE)
  # serve
  return(clued)
}

#' Takes a split vector of dirty object keys and array indices and returns
#'  a 2d list providing references as required by \code{extractValueFrom*}.
#'
#' @param paths Character vector of jsonmatch-type key references.
#' @return 2d list providing symbols as required by \code{extractValueFrom*}.
#'
#' @keywords internal
getKeysFromPaths <- function(paths) {
  stopifnot(is.character(paths))
  # split paths to path components
  comps <- strsplit(paths,
                    paste0('(?<=\\])(?=\\[)|(?<=[[:print:]])(?=\\[)|',
                           '(?<=\\])(?=\\.)|(?<=[[:print:]])(?=\\.)'),
                    perl=TRUE)
  # strip dots and \\s
  nodots <- lapply(comps, function(comp) {
    comp <- gsub('.', '', comp, fixed=TRUE)
    comp[comp != '']
  })
  # make a 2D list of chr obj keys and numeric indices
  subseq <- lapply(nodots, function(nd) {
    lapply(nd, function(d) {
      if (grepl('^\\[\\d+(?:,\\d+)*(?:\\:(?!\\:)(?:\\d+)?)*\\]$',
                d,
                perl=TRUE)) {
        # parse in integers
        eval(parse(text=sub('^\\[(\\d+(?:,\\d+)*(?:\\:(?!\\:)(?:\\d+)?)*)\\]$',
                            'c(\\1)',
                            d,
                            perl=TRUE)))
      } else {
        d
      }
    })
  })
  # serve 4 extractValueFrom*
  return(subseq)
}

#' Extracts the item at given index of an JSON array
#'
#' @param arr JSON array.
#' @param index Array index/key for which to retrieve value, zero-indexed.
#' @return Character vector.
#'
#' @keywords internal
extractValueFromArrIndex <- function(arr, index) {  # zero-indexed !!!
  stopifnot(isTruthyChr(arr), is.numeric(index), index %% 1L == 0L)
  # split arr contents on comma not enclosed in [], {} or ""
  cospl <- splitOnUnclosedChar(stripArray(arr), ',')
  # get atoms out of array
  arr.sub <- cospl[index + 1L]
  # error out
  if (anyNA(arr.sub)) stop('index out of bounds')
  # collapse
  rtn <- paste0(arr.sub, collapse=',')
  # serve
  return(rtn)
}

#' Extracts the item at given key of an JSON object
#'
#' @param obj JSON object.
#' @param key Key for which to retrieve value.
#' @return Character vector.
#'
#' @keywords internal
extractValueFromObjKey <- function(obj, key) {
  stopifnot(isTruthyChr(obj), isTruthyChr(key),
            grepl(paste0('"', key,'"\\:'), obj, perl=TRUE))
  chars <- strsplit(obj, '', fixed=TRUE)[[1L]]
  beg <- pos <- regexpr(paste0('"', key,'":'), obj)[1L] + nchar(key) + 3L
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

#' Does a string contain a character neither enclosed in brackets nor
#' double quotes?
#'
#' @param string Character vector of length 1L.
#' @param character Single character to search for.
#' @return Logical.
#'
#' @keywords internal
hasUnclosedChar <- function(string, char) {
  stopifnot(is.character(string), is.character(char), nchar(char) == 1L)
  # split to single characters
  chars <- strsplit(string, '')[[1L]]
  # setup
  opbr <- 0L        # if opbr is zero we r not in a struct
  opqt <- 2L        # counts double quotes
  nsqt <- list(2L)  # counts nested double quotes
  # peep through
  for (i in seq_along(chars)) {
    if (chars[i] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[i] %in% c(']', '}')) opbr <- opbr - 1L
    if (chars[i] == '"') opqt <- opqt + 1L
    if (grepl('\\\\+', chars[i], perl=TRUE) && chars[i + 1L] == '"') {
      if (!chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- 2L + 1L
      } else if (chars[i] %in% names(nsqt)) {
        nsqt[[chars[i]]] <- nsqt[[chars[i]]] + 1L
      }
    }
    if (chars[i] == char &&
        (opbr == 0L && opqt %% 2L == 0L  &&
         all(unlist(nsqt) %% 2L == 0L))) {
      return(TRUE)
    }
  }
  return(FALSE)
}

#' Packs atomic data in arrays, optionally adding keys
#'
#' @param accu Character vector of target values (JSON extracts).
#' @param json Input JSON string.
#' @param paths Subset pattern paths.
#' @return Character vector.
#'
#' @keywords internal
packStruct <- function(accu, json, paths) {
  stopifnot(is.character(accu), isTruthyChr(json), isTruthyChr(paths))
  rtn <- keys <- vector('character')
  i <- vector('integer')
  if (length(accu) > 1L) {
    if (isArray(json)) {          # base array
      rtn <- paste0('[',
                    paste0(sapply(accu, boxjson::boxAtoms, USE.NAMES=FALSE),
                           collapse=','),
                    ']')
    } else if (isObject(json)) {  # base object
      i <- 0L
      keys <- sub('^\\.', '', paths, perl=TRUE)
      rtn <- paste0('{',
                    paste0(sapply(accu, function(a) {
                      i <<- i + 1L
                      paste0(paste0('"', keys[i], '"', ':'),
                             boxjson::boxAtoms(a))
                    }, USE.NAMES=FALSE),
                    collapse=','),
                    '}')
    }
  } else if (length(accu) == 1L) {
    rtn <- boxjson::boxAtoms(accu, strict=FALSE)
  }
  # serve
  return(rtn)
}
