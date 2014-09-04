# TODO: Add comment
# 
# Author: shuywang
###############################################################################

predictionMem = "Percentage.ECS_Mem_AVG"
predictionCpu = "Percentage.ECS_CPU_AVG"

library(caret)

source("./PreProcessing/ReadData.R");
data = readData();

inTrain = createDataPartition(data$Percentage.ECS_CPU_AVG, p = 4/5)[[1]]
training = data[ inTrain,]
testing = data[-inTrain,]

predictors = grep("^SessionCount", names(data), value = T);
#predictors = c(predictors, "Datacenter")

# remove zero variance
for(predictor in predictors){
	v = var(training[,paste(predictor)]);
	if(v < 0.1){
		predictors = predictors[predictors!=predictor];
		print(paste("remove", predictor, "because its extra low variance:", v, sep = " "));
	}
}

addq <- function(x) paste0("`", x, "`");
trainingFormulaMem = as.formula(paste(addq(predictionMem) , " ~ ", paste(addq(predictors),collapse=" + " )));
trainingFormulaCpu = as.formula(paste(addq(predictionCpu) , " ~ ", paste(addq(predictors),collapse=" + " )));




# ridge regression

hypothesisMem1 = train(trainingFormulaMem, method = "ridge", data = training);
predMem = predict(hypothesisMem1, testing);
windows();
print(qplot(predMem, testing[,paste(predictionMem)]
		, main = "Error Plot For Avg Mem Prediction (Ridge Regression)"));
print("Error Summary For Avg Mem Prediction (Ridge Regression)");
summary(abs(predMem - testing[,paste(predictionMem)]));

hypothesisCpu1 = train(trainingFormulaCpu, method = "ridge", data = training);
predCpu = predict(hypothesisCpu1, testing);
windows();
print(qplot(predCpu, testing[,paste(predictionCpu)]
		, main = "Error Plot For Avg Cpu Prediction (Ridge Regression)"));
print("Error Summary For Avg Cpu Prediction (Ridge Regression)");
summary(abs(predCpu - testing[,paste(predictionCpu)]));



# random forest

hypothesisMem2 = train(trainingFormulaMem, method = "rf", data = training, verbose = TRUE);
predMem = predict(hypothesisMem2, testing);
windows();
qplot(predMem, testing[,paste(predictionMem)]
	, main = "Error Plot For Avg Mem Prediction (Random Forest)")
print("Error Summary For Avg Mem Prediction (Random Forest)");
summary(abs(predMem - testing[,paste(predictionMem)]));

hypothesisCpu2 = train(trainingFormulaCpu, method = "rf", data = training, verbose = TRUE);
predCpu = predict(hypothesisCpu2, testing);
windows();
qplot(predCpu, testing[,paste(predictionCpu)]
		, main = "Error Plot For Avg Cpu Prediction (Random Forest)")
print("Error Summary For Avg Cpu Prediction (Random Forest)");
summary(abs(predCpu - testing[,paste(predictionCpu)]));


# SVM linear


hypothesisMem3 = train(trainingFormulaMem, method = "svmLinear", data = training);
predMem = predict(hypothesisMem3, testing);
windows();
qplot(predMem, testing[,paste(predictionMem)]
		, main = "Error Plot For Avg Mem Prediction (SVM without Kernel)")
print("Error Summary For Avg Mem Prediction (SVM without Kernel)");
summary(abs(predMem - testing[,paste(predictionMem)]));

hypothesisCpu3 = train(trainingFormulaCpu, method = "svmLinear", data = training);
predCpu = predict(hypothesisCpu3, testing);
windows();
qplot(predCpu, testing[,paste(predictionCpu)]
		, main = "Error Plot For Avg Cpu Prediction (SVM without Kernel)")
print("Error Summary For Avg Cpu Prediction (SVM without Kernel)");
summary(abs(predCpu - testing[,paste(predictionCpu)]));


# SVM with polynomial kernels


hypothesisMem4 = train(trainingFormulaMem, method = "svmPoly", data = training);
predMem = predict(hypothesisMem4, testing);
windows();
qplot(predMem, testing[,paste(predictionMem)]
		, main = "Error Plot For Avg Mem Prediction (SVM with poly kernel)")
print("Error Summary For Avg Mem Prediction (SVM with poly kernel)");
summary(abs(predMem - testing[,paste(predictionMem)]));

hypothesisCpu4 = train(trainingFormulaCpu, method = "svmPoly", data = training, verbose = TRUE);
predCpu = predict(hypothesisCpu4, testing);
windows();
qplot(predCpu, testing[,paste(predictionCpu)]
		, main = "Error Plot For Avg Cpu Prediction (SVM with poly kernel)")
print("Error Summary For Avg Cpu Prediction (SVM with poly kernel)");
summary(abs(predCpu - testing[,paste(predictionCpu)]));




# LASSO regression


hypothesisMem5 = train(trainingFormulaMem, method = "lasso", data = training);
predMem = predict(hypothesisMem5, testing);
windows();
qplot(predMem, testing[,paste(predictionMem)]
		, main = "Error Plot For Avg Mem Prediction (LASSO regression)")
print("Error Summary For Avg Mem Prediction (LASSO regression)");
summary(abs(predMem - testing[,paste(predictionMem)]));

hypothesisCpu5 = train(trainingFormulaCpu, method = "lasso", data = training);
predCpu = predict(hypothesisCpu5, testing);
windows();
qplot(predCpu, testing[,paste(predictionCpu)]
		, main = "Error Plot For Avg Cpu Prediction (LASSO regression)")
print("Error Summary For Avg Cpu Prediction (LASSO regression)");
summary(abs(predCpu - testing[,paste(predictionCpu)]));
