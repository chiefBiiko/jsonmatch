# test jsonmatch

testthat::context('match consistency')

testthat::test_that('return matches explicit pattern', {
  
  # setup
  saka <- jsonlite::toJSON(list(a=FALSE, 
                                b=c('zu', 'lu'), 
                                c=list(x=4L, y=1L, z=9L)))
  waka <- jsonlite::toJSON(list(a=c(1,2),b=list(c(77,44), 'doo')))
  kafa <- jsonlite::toJSON(list(list(list(36)), list(44)))
  kaka <- jsonlite::toJSON(list(abc=1L, dbe=2L))
  lala <- jsonlite::toJSON(list(acab=c('A', 'B', 'C', 'D', 'E', 'F')))
  tata <- jsonlite::toJSON(list(acab=list(1, 2, 3, list(44, 55, 66))))
  
  # single item
  testthat::expect_identical(jsonmatch(saka, '.b[0]'), 
                             structure('["zu"]', class='json'))
  
  # multiple items pt1
  testthat::expect_identical(jsonmatch(saka, '.b[0:1]'), 
                             structure('["zu","lu"]', class='json'))
  
  # multiple items pt2
  testthat::expect_identical(jsonmatch(saka, '.b[0],.c'),
                             structure(
                               '{"b[0]":["zu"],"c":{"x":[4],"y":[1],"z":[9]}}',
                               class='json'
                             ))
  
  # JSON constant
  testthat::expect_identical(jsonmatch(saka, '.a'),
                             structure('[false]', class='json'))
  
  # throws on incorrect horizontal array indexing
  testthat::expect_error(jsonmatch(kafa, '[0][0][1]'))
  testthat::expect_error(jsonmatch(kafa, '[0][1][0]'))
  
  # gracefully handles incorrect vertical array indexing at a path's base
  testthat::expect_identical(jsonmatch(kafa, '[1][0][0]'),  # correct: '[1][0]'
                             structure('[44]', class='json'))
  
  # unboxing
  testthat::expect_identical(jsonmatch(kaka, '.abc,.dbe', auto_unbox=TRUE),
                             structure('{"abc":1,"dbe":2}', class='json'))
  
  # 2D array pt 1
  testthat::expect_identical(jsonmatch(waka, '.b[0]'),
                             structure('[77,44]', class='json'))
  
  # 2D array pt 2
  testthat::expect_identical(jsonmatch(waka, '.b[0:1]'),
                             structure('[[77,44],["doo"]]', class='json'))
  
  # 3D array
  testthat::expect_identical(jsonmatch(kafa, '[0][0][0]'),
                             structure('[36]', class='json'))
  
  # arrays
  testthat::expect_identical(jsonmatch(tata, '.acab'), 
                             structure('[[1],[2],[3],[[44],[55],[66]]]', 
                                       class='json'))
  
  # array edges
  testthat::expect_error(jsonmatch(lala, '.acab[1:]'))
  # should be: structure('[["B"],["C"],["D"],["E"],["F"]]', class='json')
  
})