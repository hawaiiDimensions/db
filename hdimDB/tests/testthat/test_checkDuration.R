context('test functionality of beating duration checking function')

## load test data
data('testData')

## use the checker function
check <- checkDuration(testData)

test_that('checkTime finds invalid beating time durations', {
    expect_equal(check$errHDIM[check$errMessage == 'BeatingDuration'], 
                 testData$HDIM[testData$ERROR == 'invalid beatingduration'])
})