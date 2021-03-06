jsonmatch
================

[![Travis-CI Build Status](https://travis-ci.org/chiefBiiko/jsonmatch.svg?branch=master)](https://travis-ci.org/chiefBiiko/jsonmatch) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/chiefBiiko/jsonmatch?branch=master&svg=true)](https://ci.appveyor.com/project/chiefBiiko/jsonmatch) [![Coverage Status](https://codecov.io/gh/chiefBiiko/jsonmatch/branch/master/graph/badge.svg)](https://codecov.io/gh/chiefBiiko/jsonmatch)

------------------------------------------------------------------------

Simple `JSON` matching.

------------------------------------------------------------------------

Get it
------

``` r
devtools::install_github('chiefBiiko/jsonmatch')
```

------------------------------------------------------------------------

Usage
-----

`jsonmatch::jsonmatch(json, pattern, auto_unbox=FALSE, strict=TRUE)`

-   `json` character. In-memory `JSON` string or filename. **required**
-   `pattern` character. Subset pattern. **required**
-   `auto_unbox` logical. Unbox `JSON`? Default: `FALSE`. **optional**
-   `strict` logical. Only allow valid `JSON`? Default: `TRUE`. **optional**

**Return** character, json. `JSON` subset.

`pattern` allows matching keys of `JSON` arrays and objects. Its syntax rules are simple:

-   Array values are referenced via their index (zero-based): `[0]` or `[1,3]` or `[2:]` or `[2:3]`
-   Object values are referenced via their key: `.key` or `.k*y` or `.*y` or `.k*` or `.*`
-   Multiple *subset paths* are separated by a comma: `.keyA[0],.keyB[0]`

Note that a colon within an array subset pattern is only valid if it is trailing, indicating to select all array elements with an index in the range of `[low:]` to `length(array) - 1` aka the end of the array. The wildcard character in an object subset pattern matches any alphanumeric character sequence. Wildcard matching cannot be used if object keys contain non-alphanumeric characters `[^[:alnum:]]`.

For the rationale of `auto_unbox` check out [`boxjson`](https://github.com/chiefBiiko/boxjson). If `strict` is `TRUE` and `jsonlite::validate(json)` evaluates to `FALSE` an error is thrown.

``` r
# some JSON
some.json <- jsonlite::toJSON(list(list(name='herman', gang='almans'), 
                                   list(name='habibo', gang=c('haji', '419'))))

# peek at it
cat('some JSON:\n', some.json, sep='')
```

    some JSON:
    [{"name":["herman"],"gang":["almans"]},{"name":["habibo"],"gang":["haji","419"]}]

``` r
# who u with
gangs <- jsonmatch::jsonmatch(some.json, '[0:].gang')
cat('gangs:\n', gangs, sep='')
```

    gangs:
    [["almans"],["haji","419"]]

------------------------------------------------------------------------

TODO
----

-   elaborate tests

------------------------------------------------------------------------

License
-------

MIT
