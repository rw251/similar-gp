library('RUnit')

#source("R/functions.R")

test.suite <- defineTestSuite("example", dirs = file.path("tests"), testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)
