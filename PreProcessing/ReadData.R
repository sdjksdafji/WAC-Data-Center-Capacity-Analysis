# TODO: Add comment
# 
# Author: shuywang
###############################################################################


readData = function(){
	log = readLog();
	session = readSessionData();
	data = merge(session, log, by = c("Quanta", "Datacenter"));
	return(data);
}
