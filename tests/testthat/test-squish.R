context('List squishing')

test_that('Proper output from squishList', {
    vecList <- list(a=1:4,
                    b=data.frame(x=1:2),
                    a=5:10,
                    b=data.frame(x=3:6))
    vecSquish <- squishList(vecList)
    expect_identical(vecSquish$a, 1:10)
    expect_identical(vecSquish$b, data.frame(x=1:6))

    listList <- list(a=list(x=1:3, y=1:5),
                     a=list(x=4:5),
                     x=1:3)
    listSquish <- squishList(listList)
    expect_identical(listSquish$a$x, 1:5)
    expect_identical(listSquish$x, 1:3)

    nullList <- list(a=1:2,
                     a=NULL,
                     a=3:4)
    nullSquish <- squishList(nullList)
    expect_identical(nullSquish$a, 1:4)

    matList <- list(a=matrix(1, nrow=1, ncol=2),
                    a=matrix(1, nrow=2, ncol=2))
    matSquish <- squishList(matList)
    expect_identical(matSquish$a, matrix(1, nrow=3, ncol=2))
    expect_identical(squishList(list()), list())
    vecList <- list(a=1:4,
                    b=data.frame(x=1:3),
                    c=c('a', 'b'),
                    a=2:5,
                    b=data.frame(x=3:6),
                    c=c('b', 'c'))
    unqSquish <- squishList(vecList, unique=TRUE)
    expect_identical(unqSquish$a, 1:5)
    expect_identical(unqSquish$b, data.frame(x=1:6))
    expect_identical(unqSquish$c, c('a', 'b', 'c'))
    unqList <- list(a=1:10, b=data.frame(x=1:10), c=letters[1:5])
    expect_identical(squishList(c(unqList, unqList), unique=TRUE), unqList)
})
