#airfoil reg3

#z <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat')
#write.csv(z,'reg3.csv')

########################start
setwd('../reg3')
dat <- read.csv('reg3.csv')
dat <- dat[,2:7]

library(caret)
library(neuralnet)


set.seed(5678)
t <- 'V6'
idi <- createDataPartition(dat[,t],p=0.8,list=F)

itest <- dat[-idi,]
dat <- dat[idi,]

svml <- readRDS('svmLinear.rds')
svmr <- readRDS('svmRadial.rds')
svmp <- readRDS('svmPoly.rds')
rf <- readRDS('rf.rds')
rp <- readRDS('rpart.rds')
boostgbm <- readRDS('boostgbm.rds')
boostxgb <- readRDS('boostxgb.rds')
reglin <- readRDS('reglin.rds')
regquad <- readRDS('regquad.rds')
knn <- readRDS('knn.rds')
nnrpropp <- readRDS('nnrprop+.rds')
nnrpropm <- readRDS('nnrprop-.rds')
nnsag <- readRDS('nnsag.rds')
nnslr <- readRDS('nnslr.rds')


tim <- data.frame()
train_df <- data.frame()
val_df <- data.frame()
test_df <- data.frame()

lt <-list(svml,svmr,svmp,rf,rp,boostgbm,boostxgb,reglin,regquad,knn)
# for(i in 1:10){
#   obj <- lt[[i]]
#   print(obj$method)
#   print(obj$results[which.min(obj$results$RMSE),])
# }

for(i in 1:10){
  
  obj <- lt[[i]]
  if(i!=9){
    pred <- predict(obj,dat[!names(dat) %in% t])
    rmse <- RMSE(pred,dat[,t])
    rsqua <- R2(pred,dat[,t])
    train_df <- rbind(train_df,data.frame(RMSE=rmse,Rsqua=rsqua))
    
    pred2 <- predict(obj,itest[!names(itest) %in% t])
    rmse2 <- RMSE(pred2,itest[,t])
    rsqua2 <- R2(pred2,itest[,t])
    test_df <- rbind(test_df,data.frame(RMSE=rmse2,Rsqua=rsqua2))
  }
  else{
    deg <- 2
    m <- dat[,!names(dat) %in% t]
    idx <- sapply(m, class)%in%c('numeric','integer')
    n <- as.data.frame(lapply(1:deg,function(i){m[,idx]^i}))
    n <- cbind(m[!colnames(m)%in%colnames(n)],n)
    pred <- predict(obj,n)
    rmse <- RMSE(pred,dat[,t])
    rsqua <- R2(pred,dat[,t])
    train_df <- rbind(train_df,data.frame(RMSE=rmse,Rsqua=rsqua))
    
    m <- itest[,!names(itest) %in% t]
    idx <- sapply(m, class)%in%c('numeric','integer')
    n <- as.data.frame(lapply(1:deg,function(i){m[,idx]^i}))
    n <- cbind(m[!colnames(m)%in%colnames(n)],n)
    pred2 <- predict(obj,n)
    rmse2 <- RMSE(pred2,itest[,t])
    rsqua2 <- R2(pred2,itest[,t])
    test_df <- rbind(test_df,data.frame(RMSE=rmse2,Rsqua=rsqua2))
  }
  tim <- rbind(tim,obj$times$everything[1:3])
}

for(i in 1:10){
  val_df <- rbind(val_df, lt[[i]]$results[which.min(lt[[i]]$results$RMSE),c("RMSE",'Rsquared')])
}

colnames(train_df) <- c('RMSE','Rsquared')
colnames(val_df) <- c('RMSE','Rsquared')
colnames(test_df) <- c('RMSE','Rsquared')
colnames(tim) <- c('user','self', 'elapsed')

ltn <- list(nnrpropp,nnrpropm,nnsag,nnslr)
for(i in 1:4){
  obj <- ltn[[i]]
  
  mxs <- apply(dat, 2, max) 
  mns <- apply(dat, 2, min)
  div <- mxs-mns
  div[div==0] <- 1
  msc <- scale(dat,scale=div)
  pred <- obj$net.result[[which.min(obj$result.matrix[1,])]]*(mxs[t]-mns[t]) + mean(dat[,t])#min error rep only
  rmse <- RMSE(dat[,t],pred)
  rsqua <- R2(dat[,t],pred)
  train_df <- rbind(train_df,data.frame(RMSE=rmse,Rsquared=rsqua))
  
  #normalise by train data info
  test_msc <- scale(itest[,!names(itest) %in% t],
                    scale=div[!names(itest) %in% t])
  res <- compute(obj,test_msc,rep=which.min(obj$result.matrix[1,]))$net.result
  res <- res*(mxs[t]-mns[t]) + mean(dat[,t])
  rmse2 <- RMSE(itest[,t],res)
  rsqua2 <- R2(itest[,t],res)#corr not traditional R2
  test_df <- rbind(test_df,data.frame(RMSE=rmse2,Rsquared=rsqua2))
}

rownames(train_df) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(val_df) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
rownames(test_df) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(tim) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')

tr <- round(train_df[order(train_df$RMSE,decreasing = F),],3)
vl <-round(val_df[order(val_df$RMSE,decreasing = F),],3)
tt <- round(test_df[order(test_df$RMSE,decreasing = F),],3)
#tm <- tim[order(tim$elapsed),]

#tm_withnn <- c(tim[,3],c(62.11,84.90,7.03,20.25))
View(tt)
View(tr)
View(vl)

up <- data.frame(elapsed=c(62.11,84.90,7.03,20.25))
rownames(up) <- c('NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- rbind(tim['elapsed'],up)
tm_withnn <- tm_withnn[order(tm_withnn$elapsed),,drop=F]

#names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
#tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

RMSE(rep(mean(dat[,t]),length(dat[,t])),dat[,t])#baseline