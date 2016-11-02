context('test functionality of misspelling checker')

## load test data
data('testData')

## use the checker function
check <- checkMisspell(testData)

test_that('checkMisspell finds misspelled entries', {
    expect_equal(check$Method, 9024)
})

