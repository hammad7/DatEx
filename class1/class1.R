#segmentation class1
setwd('../class1')

library(caret)
library(neuralnet)
data("segmentationData")
#dat <- readRDS('dat.rds')#****saved with p=100
dat <- segmentationData
set.seed(5678)
t <- 'Class'
idi <- createDataPartition(dat[,t],p=0.8,list=F)

itest <- dat[-idi,3:61]
dat <- dat[idi,3:61]

svml <- readRDS('svmLinear.rds')
svmr <- readRDS('svmRadial.rds')
svmp <- readRDS('svmPoly.rds')
lda <- readRDS('lda.rds')
rf <- readRDS('rf.rds')
rp <- readRDS('rpart.rds')
boostgbm <- readRDS('boostgbm.rds')
boostxgb <- readRDS('boostxgb.rds')
boostada <- readRDS('boostada.rds')
reglog <- readRDS('reglog.rds')
knn <- readRDS('knn.rds')
nb <- readRDS('nb.rds')
nnrpropp <- readRDS('nnrprop+.rds')
nnrpropm <- readRDS('nnrprop-.rds')
nnsag <- readRDS('nnsag.rds')
nnslr <- readRDS('nnslr.rds')


tim <- data.frame()
train_df <- data.frame()
val_df <- data.frame()
test_df <- data.frame()

lt <-list(svml,svmr,svmp,lda,rf,rp,boostgbm,boostxgb,boostada,reglog,nb,knn)
# for(i in 1:12){
#   obj <- lt[[i]]
#   print(obj$method)
#   print(obj$results[which.min(obj$results$Kappa),])
# }

for(i in 1:12){
  
  obj <- lt[[i]]
  
  #training
  pred <- predict(obj,dat[!names(dat) %in% t])#nb warnings
  obj_tab <- confusionMatrix(table(dat[,t],pred))
  train_df <- rbind(train_df,c(obj_tab$overall[1:2],obj_tab$byClass))
  #sensitivity of major class only because 4 rows
  
  #test
  pred2 <- predict(obj,itest[!names(dat) %in% t])
  obj_tab2 <- confusionMatrix(table(itest[,t],pred2))
  test_df <- rbind(test_df,c(obj_tab2$overall[1:2],obj_tab2$byClass))
  
  tim <- rbind(tim,obj$times$everything[1:3])
}

for(i in 1:12){
  val_df <- rbind(val_df, lt[[i]]$results[which.max(lt[[i]]$results$Kappa),c("Accuracy",'Kappa')])
}

colnames(train_df) <- c('Accuracy','Kappa','Sensitivity','Specificity','Pos Pred Value','Neg Pred Value','Prevalence','Detection Rate','Detection Prevalence','Balanced Accuracy')
colnames(val_df) <- c('Accuracy','Kappa')
colnames(test_df) <- c('Accuracy','Kappa','Sensitivity','Specificity','Pos Pred Value','Neg Pred Value','Prevalence','Detection Rate','Detection Prevalence','Balanced Accuracy')
colnames(tim) <- c('user','self', 'elapsed')

ltn <- list(nnrpropp,nnrpropm,nnsag,nnslr)
for(i in 1:4){
  obj <- ltn[[i]]
  
  pred <- apply(obj$net.result[[which.min(obj$result.matrix[1,])]],1,which.max)#min error rep only
  #pred <- sort(unique(dat[,t]))[pred]
  pred <- gsub(t,'',colnames(obj$response))[pred]
  net_tab <- confusionMatrix(table(dat[,t],pred))
  train_df <- rbind(train_df,c(net_tab$overall[1:2],net_tab$byClass))
  
  mxs <- apply(dat[!names(dat) %in% t], 2, max) 
  mns <- apply(dat[!names(dat) %in% t], 2, min)
  dv <- mxs-mns
  dv[dv==0] <- 1 #avoid NaN
  
  test_msc <- scale(itest[,!names(itest) %in% t],scale=dv)
  res <- compute(obj,test_msc,rep=which.min(obj$result.matrix[1,]))$net.result
  pred2 <- apply(res,1,which.max)#min error rep only
  #pred2 <- sort(unique(itest[,t]))[pred2]
  pred2 <- gsub(t,'',colnames(obj$response))[pred2]
  net_tab2 <- confusionMatrix(table(itest[,t],pred2))
  test_df <- rbind(test_df,c(net_tab2$overall[1:2],net_tab2$byClass))
}

rownames(train_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(val_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
rownames(test_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(tim) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')

tr <- round(train_df[order(train_df$Kappa,decreasing = T),],3)
vl <-round(val_df[order(val_df$Kappa,decreasing = T),],3)
tt <- round(test_df[order(test_df$Kappa,decreasing = T),],3)
#tm <- tim[order(tim$elapsed),]

#tm_withnn <- c(tim[,3],c(42.75,37.00,37.67,39.83))
View(tt)
View(tr)
View(vl)

up <- data.frame(elapsed=c(42.75,37.00,37.67,39.83))
rownames(up) <- c('NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- rbind(tim['elapsed'],up)
tm_withnn <- tm_withnn[order(tm_withnn$elapsed),,drop=F]

#names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
#tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

max(table(dat[,t])/length(dat[,t]))#baseline
