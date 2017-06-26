## Goal
## Predict activity using accelerometer and gyroscope data. This is the training data set. We must validade the model using the test dataset, also providade by UCI.

load("samsungData.rda")
names(samsungData)[1:12]


## see possible activities (factors) and check their observation count (rows)
table(samsungData$activity)

## transform activity into a factor
samsungData <- transform(samsungData, activity = factor(activity))

## reduce analysis to 1 of the 30 users subjected to the research
sub1<- subset(samsungData, subject == 1)

## scatter plot with the first and second variable for the first user subject. Body_acc_mean_x and body_acc_mean_y respectively.
par(mfrow = c(1,2), mar = c(5,4,1,1))
plot(sub1[, 1], col = sub1$activity, ylab = names(sub1[1]))
plot(sub1[, 2], col = sub1$activity, ylab = names(sub1[2]))
legend("bottomright", legend = unique(sub1$activity), col = unique(sub1$activity), pch = 1)

distanceMatrix <- dist(sub1[, 1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col = ubclass(sub1$activity))
