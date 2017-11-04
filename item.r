#Common
directory <- "C:\\Users\\nxk161830\\Google Drive\\_UTD_Grad_Semesters\\_UTD_SPRING_2017_\\MachineLearning\\Assignments\\Project\\dataset"

TrainDataFrame <- read.csv(paste0(directory, "\\Pre_Processed\\trainIdx1_ItemBased.csv", collapse = NULL),header = TRUE, sep = ",")
TestDataFrame <- read.csv(paste0(directory , "\\Pre_Processed\\testIdx1_ItemBased.csv", collapse = NULL),header = TRUE, sep = ",")
ValidDataFrame <- read.csv(paste0(directory, "\\Pre_Processed\\validationIdx1_ItemBased.csv", collapse = NULL), header = TRUE, sep = ",")
TrainDataFrame

traindf <- (TrainDataFrame[,!(names(TrainDataFrame) %in% c("UserID"))])


matrics  <- matrix(NA, nrow=ncol(traindf),ncol=ncol(traindf),dimnames =list(colnames(traindf),colnames(traindf)))

distance <- function(point1, point2) 
{
  dist <- sum(point1*point2) / (sqrt(sum(point1*point1)) * sqrt(sum(point2*point2)))
  return(dist)
}

for(i in 1:ncol(traindf)) {
  for(j in 1:ncol(traindf)) {
    matrics[i,j] <- distance(as.matrix(traindf[i]),as.matrix(traindf[j]))
  }
}

matrics <- as.data.frame(matrics)

neighbours <- matrix(NA, nrow=ncol(matrics),ncol=11,dimnames=list(colnames(matrics)))

for(i in 1:ncol(traindf)) {
  neighbours[i,] <- (t(head(n=11,rownames(matrics[order(matrics[,i],decreasing=TRUE),][i]))))
}
neighbours


################User based##################################################################################################


ratings <- function(old, new)
{
  x <- sum(old*new)/sum(new)
  x
}

temp <- matrix(NA, nrow=nrow(TrainDataFrame),ncol=ncol(TrainDataFrame)-1,dimnames=list((TrainDataFrame$UserID),colnames(TrainDataFrame[-1])))

for(i in 1:nrow(temp)) {
  for(j in 1:ncol(temp)) 
  {
    UserID <- rownames(temp)[i]
    album <- colnames(temp)[j]
    
    if(as.integer(TrainDataFrame[TrainDataFrame$UserID==UserID,album]) == 1)
    { 
      temp[i,j]<-""
    } else {
      
      best<-((head(n=11,(matrics[order(matrics[,album],decreasing=TRUE),][album]))))
      bname <- as.character(rownames(best))
      bestnew <- as.numeric(best[,1])
      
      bestnew<-bestnew[-1]
      bname<-bname[-1]
      
      bestrated<- TrainDataFrame[,c("UserID",bname)]
      best_temp<-bestrated[bestrated$UserID==UserID,]
      best_temp <- as.numeric(best_temp[!(names(best_temp) %in% c("UserID"))])
      
      temp[i,j]<-ratings(new=bestnew,old=best_temp)
    }}}
finalscores <- temp


finalOP <- matrix(NA, nrow=nrow(finalscores),ncol=100,dimnames=list(rownames(finalscores)))
for(i in 1:nrow(finalscores)) {
  finalOP[i,] <- names(head(n=100,(finalscores[,order(finalscores[i,],decreasing=TRUE)])[i,]))
}
finalOP

