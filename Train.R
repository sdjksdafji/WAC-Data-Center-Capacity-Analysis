# TODO: Add comment
# 
# Author: shuywang
###############################################################################

prediction = "Percentage.ECS_Mem_AVG"

library(caret)

data = readData();

inTrain = createDataPartition(data$Percentage.ECS_CPU_AVG, p = 4/5)[[1]]
training = data[ inTrain,]
testing = data[-inTrain,]

addq = function(x) paste0("`", x, "`")
trainingFormula = as.formula(paste(addq(prediction) , " ~ ", paste(addq(predictors),collapse=" + " )))




# ridge regression

hypothesis1 = train(trainingFormula, method = "ridge", data = training);
pred = predict(hypothesis1, testing);
qplot(pred, testing[,paste(prediction)])
print("Error Summary");
summary(abs(pred - testing[,paste(prediction)]));



# random forest

hypothesis2 = train(trainingFormula, method = "rf", data = training);
trainingPredict = predict(hypothesis2, training);
qplot(trainingPredict, training[,paste(prediction)])
print("Error Summary");
summary(abs(trainingPredict - training[,paste(prediction)]));