context('test functionality of misspelling checker')

## load test data
data('testData')

## use the checker function
check <- checkMisspell(testData)

test_that('checkMisspell finds misspelled entries', {
    expect_equal(check$errHDIM[gsub("\\..*", '', as.character(check$errMessage)) == 'misspelled'], 
                 testData$HDIM[testData$ERROR == 'misspelled value'])
})
