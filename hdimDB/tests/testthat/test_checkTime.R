context('test functionality of time checking function')

## load test data
data('testData')

## use the checker function
check <- checkTime(testData)

test_that('checkTime finds incorrectly formatted time entries', {
    expect_equal(check$errHDIM[check$errMessage == 'time.TimeBegin'], 
                 testData$HDIM[testData$ERROR == 'invalid time format'])
})

test_that('checkTime finds incorrectly formatted dates and dates outside the correct range', {
    expect_equal(check$errHDIM[check$errMessage == 'time.Date'], 
                 testData$HDIM[testData$ERROR == 'invalid date'])
})