install.packages("rpart")
install.packages("rpart.plot")


install.packages("caTools")
install.packages("caret")
install.packages("dplyr")
install.packages("corrplot")
install.packages("tree")
install.packages("ROCR")

library(caTools)
library(caret)
library(dplyr)
library(corrplot)
library(tree)
library(ROCR)


library(rpart)
library(rpart.plot)

eBay.auction <- read.csv("E:/uOttawa/DTI -5126, IAI 5120/Assignments/Assignment 3/eBayAuctions1.csv")

df$Duration <- as.factor(eBay.auction$Duration)
eBay.auction$Competitive. <- as.factor(eBay.auction$Competitive.)

dimnames(eBay.auction)

set.seed(100)
train.index <- sample(c(1:dim(eBay.auction)[1]), dim(eBay.auction)[1]*0.6)
train.df <- eBay.auction[train.index, ]
valid.df <- eBay.auction[-train.index, ]

unique(eBay.auction$category)

deeper.ct <- rpart(Competitive. ~., data = train.df, method = "class", 
control = rpart.control(maxdepth =7, minsplit=1, cp=0, minbucket=50))

length(deeper.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

prp(deeper.ct, type = 1, extra =1, split.font= 1, varlen = -10, compress = FALSE)

printcp(deeper.ct)

pruned.ct <- prune(deeper.ct, cp = deeper.ct$cptable[which.min
(deeper.ct$cptable[, "xerror"]), "CP"])

length(pruned.ct.1$frame$var[pruned.ct.1$frame$var == "<leaf>"])

prp(pruned.ct, type = 1, extra =1, split.font= 1, varlen = -10)

prp(pruned.ct, type = 1, extra =100, split.font= 1, varlen = -10)


printcp(deeper.ct)

preds1.valid <- predict(pruned.ct, newdata = valid.df, type = "class") 
#use the predict() function and pass in the testing subset

#Print the confusion Matrix
confusionMatrix(valid.df$Competitive., preds1.valid)



preds1.train <- predict(pruned.ct, newdata = train.df, type = "class") 
#use the predict() function and pass in the testing subset

#Print the confusion Matrix
confusionMatrix(train.df$Competitive., preds1.train)

# estimate variable importance
importance <- varImp(pruned.ct)
# summarize importance
print(importance)
# plot importance
plot(importance)

#only taking the 4 ones because they are the only ones that show up 
in the tree, so obviously most important

deeper.ct.1.d.1 <- rpart(Competitive. ~ + ClosePrice + Category 
+ OpenPrice + sellerRating, data = train.df, method="class", 
control = rpart.control(minbucket = 50, maxdepth = 7, cp=0, minsplit= 1))

#length
length(deeper.ct.1.d.1$frame$var[deeper.ct.1.d.1$frame$var == "<leaf>"])

#plot
prp(deeper.ct.1.d.1, type = 1, extra =1, split.font= 1, varlen = -10, compress = FALSE)


printcp(deeper.ct.1.d.1)

bestcp <- deeper.ct.1.d.1$cptable[which.min(deeper.ct.1.d.1$cptable[,"xerror"]),"CP"]


# Prune the tree using the best cp.
pruned.ct.1.d.1 <- prune(deeper.ct.1.d.1, cp = bestcp)


# Plot pruned tree
length(pruned.ct.1.d.1$frame$var[pruned.ct.1.d.1$frame$var == "<leaf>"])



prp(pruned.ct.1.d.1, type = 1, extra =1, split.font= 1, varlen = -10)
#after creating the pruned tree we are supposed to get the confusion matrix



preds.valid <- predict(pruned.ct.1.d.1, newdata = valid.df, type = "class") 
#use the predict() function and pass in the testing subset

#Print the confusion Matrix
confusionMatrix(valid.df$Competitive., preds.valid)



#Print the confusion Matrix
confusionMatrix(train.df$Competitive., preds.train)

preds.train <- predict(pruned.ct.1.d.1, newdata = train.df, type = "class") 
#use the predict() function and pass in the testing subset


#deeper tree
preds.valid <- predict(pruned.ct, newdata = valid.df, type = "class") 
#use the predict() function and pass in the testing subset

#Print the confusion Matrix
confusionMatrix(valid.df$Competitive., preds.valid)


#Deeper tree
preds.train <- predict(pruned.ct, newdata = train.df, type = "class") 
#use the predict() function and pass in the testing subset

#Print the confusion Matrix
confusionMatrix(train.df$Competitive., preds.train)

#lift chart
# this is the code for lift chart

pred <- prediction(c(as.numeric(as.character(preds.valid))),
c(as.numeric(as.character(valid.df$Competitive.))))

lift <- performance(pred, measure="lift", x.measure="rpp")
plot(lift, col="orange", lwd=2, main="Lift curve")



#scatter plot
plot(train.df$OpenPrice, train.df$ClosePrice, col=c("red","blue")[train.df$Competitive.],
xlim=c(0,50), ylim=c(0,75))
#for adding coordinates to the each of the competetive nature

#for adding lines
text(x=train.df$OpenPrice, y=train.df$ClosePrice)
lines(x=c(1.8,1.8), y=c(0,80), type = "l", lty = 1, lwd = 2, col = "green")
lines(x=c(10,10), y=c(0,80), type = "l", lty = 1, lwd = 2, col = "green")
lines(x=c(4.9,4.9), y=c(0,80), type = "l", lty = 1, lwd = 2, col = "green")
lines(x=c(0,55), y=c(1.8,1.8), type = "l", lty = 1, lwd = 2, col = "purple")
lines(x=c(0,55), y=c(10,10), type = "l", lty = 1, lwd = 2, col = "purple")
lines(x=c(0,55), y=c(4,4), type = "l", lty = 1, lwd = 2, col = "purple")
legend(35, 30, legend=c("non competetive", "competetive"),
col=c("red", "blue"), lty=3, cex=0.8)


