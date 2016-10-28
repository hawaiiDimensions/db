context('test functionality of duplicate HDIM number checker')

## load test data
data('testData')

## use the checker function
check <- dupHDIM(testData)

test_that('dupHDIM finds duplicate HDIM numbers', {
    expect_equal(check, 9024)
})
