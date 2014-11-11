# The machine learning approach is neural networks with multilayers.
# The package used in this program is Stuttgart Neural Network Simulator (SNNS) for R (RSNNS).

# install.packages("RSNNS")
library(RSNNS)

# Suppose our "data" includes 8 colunms, the first 7 ones are features, the last colunm is the continus target data.
# In order to do the classification, it needs to obtain dicrete output values.
data[,8] <- round(data[,8])
# Randomize the order of the data.
data = data[sample(1:nrow(data),length(1:nrow(data))),1:ncol(data)]
# Specify input values and outputs with class labels.
dataInputs = data[,1:7]
dataTargets = decodeClassLabels(data[,8])

# Constructing coordinating data.
coordinate.data <- data.frame(data[,8],encodeClassLabels(dataTargets))
coordinate.data <- coordinate.data[!duplicated(coordinate.data),]

# Split data into training and testing with a fixed ratio.
data = splitForTrainingAndTest(dataInputs, dataTargets, ratio=0.15)
# Normalize input data.
data = normTrainingAndTestSet(data)

# Use mpl fuction to obtain the model.
model = mlp(data$inputsTrain, data$targetsTrain, size=5, learnFunc="Quickprop", learnFuncParams=c(0.1, 2.0, 0.0001, 0.1),maxit=100, inputsTest=data$inputsTest, targetsTest=data$targetsTest) 
# Make predictions.
predictions = predict(model, data$inputsTest)
predictions.explict <- rep(0, length = nrow(predictions))
test.explict <- rep(0, length = nrow(predictions))
for(i in 1:nrow(predictions)){
  predictions.explict[i] <- coordinate.data[coordinate.data[, 2] == encodeClassLabels(predictions)[i], ][1, 1]
  # Get back the true value of the test inputs.
  test.explict[i] <- coordinate.data[coordinate.data[, 2] == encodeClassLabels(data$targetsTest)[i], ][1, 1]
} 

# Calculate the accuracy of the model.
accuracy = sum(encodeClassLabels(predictions) == encodeClassLabels(data$targetsTest))/ncol(data$targetsTest)
print(accuracy)
print(cbind(data$inputsTest, test.explict, predictions.explict))