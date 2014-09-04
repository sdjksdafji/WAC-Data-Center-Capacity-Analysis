# TODO: Add comment
# 
# Author: shuywang
###############################################################################


readData = function(){
	source("./PreProcessing/LogPreProcessing.R");
	source("./PreProcessing/UserSessionSamplePreProcessing.R");
	log = readLog();
	session = readSessionData();
	data = merge(session, log, by = c("Quanta", "Datacenter"));
	return(data);
}
