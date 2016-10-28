context('test functionality of time checking function')

## load test data
data('testData')

## use the checker function
check <- checkTime(testData)

test_that('checkTime finds problem time entries', {
    expect_equal(check, 8081)
})
