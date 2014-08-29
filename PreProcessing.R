# TODO: Add comment
# 
# Author: shuywang
###############################################################################

dir = "E:/";
logFile = "logs_2014_08_27.ss.csv";

log = read.csv(paste(dir, logFile, sep = ""));

group = "Conversion-PdfServer";



newLog = NULL
for(group in levels(log$TraceGroup)){
	print(group);
	
	subLog = log[log$TraceGroup == group, ];
	
	drops <- c("Role", "TraceGroup")
	subLog = subLog[,!(names(subLog) %in% drops)];
	
	names(subLog)[names(subLog)=="Percentage"] = paste(group, "Percentage", sep = ".");
	names(subLog)[names(subLog)=="CurrentUsage"] = paste(group, "CurrentUsage", sep = ".");
	names(subLog)[names(subLog)=="TotalAvailable"] = paste(group, "TotalAvailable", sep = ".");
	#print(head(subLog));
	
	if(is.null(newLog)){
		newLog = subLog;
	}else{
		newLog = merge(newLog, subLog,by=c("Quanta"));#,"Machine", "Role", "Datacenter"
	}
	print(dim(newLog));
}

