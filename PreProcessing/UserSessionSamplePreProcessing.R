# TODO: Add comment
# 
# Author: shuywang
###############################################################################

readSessionData = function(filePath = "E:/UserSessionsSample.csv"){

	sample = read.csv(filePath);
	
	sample$Timestamp = as.POSIXlt(as.character(sample$Timestamp), format = "%m/%d/%Y %H:%M:%S", tz="UTC");
	
	xSample = sample[sample$App == "Excel", ];
	
	xSample$App = factor(xSample$App);
	xSample$AppMode = factor(xSample$AppMode);
	xSample$AppModeExtended = factor(xSample$AppModeExtended);
	
	xSample$Mode = factor(paste(xSample$AppMode, xSample$AppModeExtended, sep = "."));
	xSample = xSample[,!(names(xSample) %in% c("AppMode", "AppModeExtended"))];
	
	modes = levels(xSample$Mode);
	init = TRUE;
	
	for(mode in modes){
		if(init){
			result = xSample[xSample$Mode == mode, ];
			init = FALSE;
		}else{
			temp = xSample[xSample$Mode == mode, ];
			result = merge(result, temp, by = c("Timestamp", "Datacenter", "App"), all = TRUE);
			
		}
		# rename SessionCount to indicate which mode
		names(result)[names(result) == "SessionCount"] = paste("SessionCount", mode, sep = ".");
		
		# drop the Mode column
		result = result[,!(names(result) %in% c("Mode"))];
	}
	
	intColumns = grep("^SessionCount", names(result), value = TRUE);
	
	for(col in intColumns){
		result[is.na(result[, col]), col] <- 0
	}

	#rename timestamp to quanta
	names(result)[names(result) == "Timestamp"] = "Quanta";
	
	return(result);
}