# TODO: Add comment
# 
# Author: shuywang
###############################################################################


dir = "E:/";
logFile = "UserSessionsSample.csv";

sample = read.csv(paste(dir, logFile, sep = ""));

sample$Timestamp = as.POSIXlt(as.character(sample$Timestamp), format = "%m/%d/%Y %H:%M:%S", tz="UTC");

excelSample = sample[sample$App == "Excel", ];

excelSample$App = factor(excelSample$App);
excelSample$AppMode = factor(excelSample$AppMode);
excelSample$AppModeExtended = factor(excelSample$AppModeExtended);