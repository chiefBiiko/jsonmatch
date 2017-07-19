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

``` r
# some JSON
some.json <- jsonlite::toJSON(list(list(name='herman', 
                                        gang='almans'), 
                                   list(name='fraudster', 
                                        gang=c('haji', '419'))))

# peek at it
cat('some JSON:\n', some.json, sep='')
```

    some JSON:
    [{"name":["herman"],"gang":["almans"]},{"name":["fraudster"],"gang":["haji","419"]}]

``` r
# who u with
gangs <- jsonmatch::jsonmatch(some.json, '[0:].gang')
cat('gangs:\n', gangs, sep='')
```

    gangs:
    [["almans"],["haji","419"]]

------------------------------------------------------------------------

License
-------

MIT
