# TODO: Add comment
# 
# Author: shuywang
###############################################################################

readLog = function(filePath = "E:/logs_2014_08_27.ss.csv"){

	log = read.csv(filePath);
	
	log$Quanta = as.POSIXlt(as.character(log$Quanta), format = "%m/%d/%Y %I:%M:%S %p", tz="UTC");
	
	role = "ExcelServicesEcsRole"
	
	excelLog = log[log$Role == "ExcelServicesEcsRole", ];
	excelLog$Role = factor(excelLog$Role)
	excelLog$TraceGroup = factor(excelLog$TraceGroup)
	
	excelLogCpu = excelLog[excelLog$TraceGroup == "ECS_CPU", ]
	excelLogMem = excelLog[excelLog$TraceGroup == "ECS_Memory", ]
	
	excelLog = merge(excelLogMem, excelLogCpu, by = c("Quanta", "Machine", "Role", "Datacenter"));
	
	names(excelLog) <- sub(".x", ".ECS_Mem", names(excelLog));
	names(excelLog) <- sub(".y", ".ECS_CPU", names(excelLog));
	
	# drop the TraceGroup column
	excelLog = excelLog[,!(names(excelLog) %in% grep("^TraceGroup", names(excelLog), value = TRUE))];
	
	#return(excelLog);
	
	s = split(excelLog, list(as.character(excelLog$Quanta), excelLog$Datacenter), drop = TRUE);
	
	f = function(x){
		r = data.frame("Quanta" = x[1, "Quanta"], "Datacenter" = x[1, "Datacenter"], 
				"Percentage.ECS_Mem_AVG" = mean(x$Percentage.ECS_Mem),
				"CurrentUsage.ECS_Mem_AVG" = mean(x$CurrentUsage.ECS_Mem),
				"TotalAvailable.ECS_Mem_AVG" = mean(x$TotalAvailable.ECS_Mem),
				"Percentage.ECS_CPU_AVG" = mean(x$Percentage.ECS_CPU),
				"CurrentUsage.ECS_CPU_AVG" = mean(x$CurrentUsage.ECS_CPU),
				"TotalAvailable.ECS_CPU_AVG" = mean(x$TotalAvailable.ECS_CPU));
	}
	
	r = lapply(s, f);
	df = do.call(rbind, r);
	return(df);
}