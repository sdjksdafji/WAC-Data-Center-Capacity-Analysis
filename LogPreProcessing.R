# TODO: Add comment
# 
# Author: shuywang
###############################################################################

dir = "E:/";
logFile = "logs_2014_08_27.ss.csv";

log = read.csv(paste(dir, logFile, sep = ""));

log$Quanta = as.POSIXlt(as.character(log$Quanta), format = "%m/%d/%Y %I:%M:%S %p", tz="UTC");

role = "ExcelServicesEcsRole"

excelLog = log[log$Role == "ExcelServicesEcsRole", ];
excelLog$Role = factor(excelLog$Role)
excelLog$TraceGroup = factor(excelLog$TraceGroup)

excelLogCpu = excelLog[excelLog$TraceGroup == "ECS_CPU", ]
excelLogMem = excelLog[excelLog$TraceGroup == "ECS_Memory", ]

excelLog = merge(excelLogMem, excelLogCpu, by = c("Quanta", "Machine", "Role", "Datacenter"));

head(excelLog)

