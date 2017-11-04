install.packages("neuralnet", dependencies = T)

library(neuralnet)

directory <- "C:\\Users\\nxk161830\\Google Drive\\_UTD_Grad_Semesters\\_UTD_SPRING_2017_\\MachineLearning\\Assignments\\Project\\dataset"

TrainDataFrame <- read.csv(paste0(directory, "\\Pre_Processed\\trainIdx1_nn.csv", collapse = NULL), col.names = c("UserID", "AlbumID", "Rating"), header=TRUE, sep = ",")
TestDataFrame <- read.csv(paste0(directory , "\\Pre_Processed\\testIdx1_nn.csv", collapse = NULL), col.names = c("UserID", "AlbumID", "Rating"), header=TRUE, sep = ",")

trainDF <- TrainDataFrame
testDF <- TestDataFrame

n <- names(trainDF)
n
f <- as.formula(paste("Rating ~", paste(n[!n %in% "Rating"], collapse = " + ")))
f

nn <- neuralnet(f, data=trainDF, hidden = c(3,2))
plot(nn)
nn$result.matrix

pred <- compute(nn,testDF[,1:2])
pred


plot(real.values, pred.scaled, col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')

pred.scaled[pred.scaled <= mean(pred.scaled)] <- 0
pred.scaled[pred.scaled > mean(pred.scaled)] <- 1
tab <- table(real.values, pred.scaled)
tab
sum_diag <- sum(diag(tab))
sum_all <- sum(tab)
accuracy <- sum_diag/sum_all
error <- 1 - accuracy

TP <- tab[1,1]
FN <- tab[1,2]
FP <- tab[2,1]
TN <- tab[2,2]

precision <- (TP/(TP+FP))
recall <- (TP/(TP+FN))
f_measure <- (2*TP/(2*TP+FP+FN))

accuracy
error
precision
recall
f_measure



