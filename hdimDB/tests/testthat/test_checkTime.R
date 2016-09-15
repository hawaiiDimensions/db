context('time checker function')

## make a test database with wrong date
testdb <- data.frame(HDIM = 5000, Date = 'vi/3/2015')

test_that('checkTime finds problem time entries', {
    expect_equal(checkTime(testdb), 5000)
})
