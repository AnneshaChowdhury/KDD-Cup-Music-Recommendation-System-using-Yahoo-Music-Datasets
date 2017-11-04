#SVM
library(e1071)


directory <- "C:\\Users\\nxk161830\\Google Drive\\_UTD_Grad_Semesters\\_UTD_SPRING_2017_\\MachineLearning\\Assignments\\Project\\dataset"

TrainDataFrame <- read.csv(paste0(directory, "\\Pre_Processed\\trainIdx1_svm.csv", collapse = NULL), col.names = c("UserID", "AlbumID", "Rating"), header=TRUE, sep = ",")
TestDataFrame <- read.csv(paste0(directory , "\\Pre_Processed\\testIdx1_svm.csv", collapse = NULL), col.names = c("UserID", "AlbumID", "Rating"), header=TRUE, sep = ",")


#SVM
svm_accuracy <- c()
svm_error <- c()
svm_precision <- c()
svm_recall <- c()
svm_f_measure <- c()

trainDF <- TrainDataFrame
testDF <- TestDataFrame


#SVM
svmmodel <- svm(as.matrix(Rating)~., data = trainDF, kernel = "polynomial", degree="4" type="C-classification", cost = .1, scale = FALSE)

p <- predict(svmmodel, testDF, type="class")

tab <- table(testDF[,3], p)

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


