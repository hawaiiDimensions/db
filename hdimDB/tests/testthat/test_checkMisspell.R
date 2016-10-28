context('test functionality of check empty')

## load test data
data('testData')

## use the checker function
check <- checkEmpty(testData)

test_that('checkEmpty finds missing entries', {
    expect_equal(check$column$Collector, 4520)
})

test_that('checkEmpty finds missing contingencies', {
    expect_equal(check$contingency$beating, 4445)
})
