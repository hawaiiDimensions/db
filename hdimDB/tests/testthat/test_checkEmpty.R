context('test functionality of check empty')

## load test data
data('testData')

## use the checker function
check <- checkEmpty(testData)

test_that('checkEmpty finds missing entries', {
    expect_equal(check$errHDIM[check$errMessage == 'empty.column.Collector'], 
                 testData$HDIM[testData$ERROR == 'empty value'])
})

test_that('checkEmpty finds missing contingencies', {
    expect_equal(check$errHDIM[check$errMessage == 'empty.contingency.beating'], 
                 testData$HDIM[testData$ERROR == 'empty contingency'])
})

test_that('checkEmpty finds misplaced entries', {
    expect_equal(check$errHDIM[check$errMessage == 'empty.misplaced.beating'], 
                 testData$HDIM[testData$ERROR == 'misplaced value'])
})
