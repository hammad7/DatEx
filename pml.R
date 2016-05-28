#bookmarks,http://machinelearningmastery.com/author/jasonb/
#pml video links,  ?ada and paper referene at last

#add more getMOdelInfo, caret firefox
#metric-roc
#train$times

#svm-caret kernlab-tab
#rf,rpart
#gam
#nb
#boost 4+xg (gbm)
#lda-vedeo  ,qda --23 pml video,categorical,cv,table,step,fda,mda
#knn-video
#nn,softmax,algo
#glm-spline,interaction
  #factors
#knnimput
#topic modelling
#LDA,lsa

#test on which set

#csvupload,text

#predict
#missclassification, gini,info

#parallel

#weather.csv rattle,iris,luv,faithful,segmrntationdata,BloodBrain,Smarket,crabs,
#MASS::boston(medv),library(ISLR),
#data(BostonHousing),data(BreastCancer),data(Glass),data(Ionosphere),
#data(PimaIndiansDiabetes),data(Sonar)data(Soybean)-linkbook, UCI
#library(AppliedPredictiveModeling),data(abalone)
#data(GermanCredit)caret-logistic
#infert

#library(cluster.datasets),https://cran.r-project.org/web/packages/dendextend/vignettes/Cluster_Analysis.html,
#spherical
#ROC
#summary-rattle like tab NA,unique
#plot 2 most pca,cmdsclaes

#gamboost iris >2 class not working


#data split
#sherical/anescomb data.rd*******
#UCI-adult,letter-wine,yeast

#metric#evaluation- Rsquared,rmse cp,kappa,Accuracy ,,,sensitivity(),
#specificity(),f-score,?confusion.Matrix()****for formulae,last 3-4 bookmarks-logloss
#time-compute cost
#summary(resamples()#################)

#dynamic plots

#use render ui if alternate text
#Repo
#https://journal.r-project.org/archive/2010-1/RJournal_2010-1_Guenther+Fritsch.pdf


library(ElemStatLearn)#data
#different bag
#diff regulari
#rf on sin,spher data,anescombe
names(getModelInfo())

#rf,rpart not work
df <- data.frame(V1=rep(c(10,10,0,0),25)+rnorm(100))
df[,2] <- c(rep(c(10,10,10,0,0,0),16),10,10,10,0)+rnorm(100)
df[,3] <- c(rep(c('0','10'),50))#+rnorm(100))
inTrain <- createDataPartition(y=df$V3, p=0.7, list=FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]
ggplot2::qplot(V1,V2,data=df,col=V3)
modFit <- train(V3 ~ .,method="rpart",data=training)
modFit
sum(ifelse(predict(modFit$finalModel,testing)==testing$V3,1,0))/100
rattle::fancyRpartPlot(modFit$finalModel)
modFit$finalModel


#circular,spherical****
#rf,rpart not work
df <- data.frame(V1=10*cos(c(seq(0,10,0.1)))+rnorm(101))
df[,2] <- 10*sin(c(seq(0,10,0.1)))+rnorm(101)
df0 <- data.frame(V1=30*cos(c(seq(0,10,0.1)))+rnorm(101))
df0[,2] <- 30*sin(c(seq(0,10,0.1)))+rnorm(101)
df <- rbind(df,df0)
df[,3] <- c(rep('0',101),rep('10',101))#+rnorm(202)

inTrain <- createDataPartition(y=df$V3, p=0.7, list=FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]
ggplot2::qplot(V1,V2,data=training,col=V3)
modFit <- train(V3 ~ .,method="rpart",data=training)
modFit
sum(abs(predict(modFit$finalModel,testing)-testing$V3))/60
sum(abs(predict(modFit$finalModel)-training$V3))/142
#plot(1:142,predict(modFit$finalModel),type = 'l')
rattle::fancyRpartPlot(modFit$finalModel)
modFit$finalModel
modFit$modelType

##sphere
df<-data.frame(V1=0,V2=0,V3=0)
i<-1;a1 <- 10; b1 <- 15; c1 <-0
for(tht in seq(0,2*pi,0.1)){
  for(phi in seq(0,2*pi,0.1)){
    df[i,] <- c(a1*cos(tht)*sin(phi)+c1*rnorm(1),a1*sin(tht)*sin(phi)+c1*rnorm(1),a1*cos(phi)+c1*rnorm(1))
    i=i+1
  }
}
for(tht in seq(0,2*pi,0.1)){
  for(phi in seq(0,2*pi,0.1)){
    df[i,] <- c(b1*cos(tht)*sin(phi)+c1*rnorm(1),b1*sin(tht)*sin(phi)+c1*rnorm(1),b1*cos(phi)+c1*rnorm(1))
    i=i+1
  }
}
df[,4] <- c(rep('0',7938/2),rep('10',7938/2))
library(rgl)
plot3d(df$V1,df$V2,df$V3,col=as.numeric(df$V4)+1)
write.csv(df,'spherical.csv')

#########imp
df <- data.frame(V1=1:100+rnorm(100))
df[,2] <- (1:100)/2+rnorm(100)
df[,3] <- c(rep('0',33),rep('5',33),rep('10',33),10)#+rnorm(100)
inTrain <- createDataPartition(y=df$V3, p=0.7, list=FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]
ggplot2::qplot(V1,V2,data=training,col=V3)
modFit <- train(V3 ~ .,method="rpart",data=training)
modFit
rattle::fancyRpartPlot(modFit$finalModel)
modFit$finalModel
modFit$modelType



inTrain <- createDataPartition(y=iris$Species, p=0.7, list=FALSE)
training <- iris[inTrain,]
testing <- iris[-inTrain,]
#table(iris$Species)
#ggplot2::qplot(Petal.Width,Petal.Length,colour=Species,data=training)
#table(training$Species)

library(caret)
#train_control <- trainControl(method="boot", number=100)
#modFit <- train(Species ~ .,method="rpart",data=training,trControl=train_control)
modFit <- train(Species ~ .,method="rpart",data=training)
#recursive partitioning, regresion & classification tree
modFit
print(modFit$finalModel)
#modFit$results
plot(modFit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(modFit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

library(rattle)
fancyRpartPlot(modFit$finalModel)
#install.packages('rpart.plot')

sum(ifelse(predict(modFit,newdata=testing)==testing$Species,1,0))/45


library(rattle)
rattle()#######

?loess
#bagging  => same bias, low variance
?bag
getTree()
?train
#xgboost

args(train.default)
args(trainControl)#
#data analyses
library(kernlab)
data(spam)
hist(spam$capitalAve)#############skewed

mean(spam$capitalAve)
sd(spam$capitalAve)############highly variable

#1.so standardise (but use same tranformation{preObj} parameters on test set)
preObj <- preProcess(,method=c('center','scale'))#also knnImpute method
transformed_test <- predict(preObj,testset)
#or
preProcess arg in train()
#2.BoxCox

##########knnImpute

#for factors#################
dummyVars()

#######no varaibility
nearZeroVar()

#########
library(splines)#same on test by predict() not bs()
bs(training$age, df=3)#with lm

############transformations log,pca,boxcox
a <- prcomp(log(x+1))
a$rotation
#or preProcess

expand.grid()

library(caret)
library(klaR)
# load the iris dataset
data(iris)
# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
# train a naive bayes model
model <- NaiveBayes(Species~., data=data_train)
# make predictions
x_test <- data_test[,1:4]
y_test <- data_test[,5]
predictions <- predict(model, x_test)
# summarize results
confusionMatrix(predictions$class, y_test)

#binary.data() in neuralnet

library(kernlab)###better detail
x <- rbind(matrix(rnorm(120),,2),matrix(rnorm(120,mean=3),,2))
y <- matrix(c(rep(1,60),rep(-1,60)))
svp <- ksvm(x,y,type="C-svc")
plot(svp,data=x)
#points(x,pch = c("o","+")[1:120 %in% svp@SVindex + 1])
svp@SVindex

library(e1071)#caret's lsvmLinear2
#linear-kernlab
#svm
data(iris)
model <- svm(Species ~ ., data = iris)

# alternatively the traditional interface:
x <- subset(iris, select = -Species)
y <- iris[,5]
model <- svm(x, y) 
?svm
print(m)
summary(m)

# test with train data
pred <- predict(model, x)
# (same as:)
pred <- fitted(model)

# Check accuracy:
table(pred, y)

# compute decision values and probabilities:
pred <- predict(model, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4,]

plot(model,iris)

# visualize (classes by color, SV by crosses):
plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model$index + 1])

## try regression mode on two dimensions

# create data
x <- seq(0.1, 5, by = 0.05)
y <- log(x) + rnorm(x, sd = 0.2)

# estimate model and predict input values
m   <- svm(x, y)
new <- predict(m, x)


# visualize
plot(x, y)
points(x, log(x), col = 2)
points(x, new, col = 4)

#values for the C, the "cost" of the radial kernel. This parameter controls the complexity of the boundary between support vectors. The radial kernel also requires setting a smoothing parameter, sigma.
#For multiclass-classification with k levels, k>2, libsvm uses the 'one-against-one'-approach, in which k(k-1)/2 binary classifiers are trained; the appropriate class is found by a voting scheme.

inTraining <- createDataPartition(mydata[,input$t_f], p=trainprop, list=FALSE)
training.set <- mydata[inTraining,]

Totalvalidation.set <- mydata[-inTraining,]
inTesting <- createDataPartition(Totalvalidation.set[,input$t_f], p=testprop, list=FALSE)

testing.set <- Totalvalidation.set[inTesting,]
validation.set <- Totalvalidation.set[-inTesting,]



# Training SVM Models
library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	       # plot the ROC curves

### Get the Data
# Load the data and construct indices to divide it into training and test data sets.
data(segmentationData)  	# Load the segmentation data set
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
testData  <- segmentationData[-trainIndex,]
trainX <-trainData[,4:61]        # Pull out the variables for training
sapply(trainX,summary)           # Look at a summary of the training data

## SUPPORT VECTOR MACHINE MODEL
# First pass
set.seed(1492)
# Setup for cross validation
ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
                     repeats=5,		    # do 5 repititions of cv
                     summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                     classProbs=TRUE)


#Train and Tune the SVM
svm.tune <- train(x=trainX,
                  y= trainData$Class,
                  method = "svmRadial",   # Radial kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

svm.tune



#     Transform data to the format of an SVM package
#   Conduct simple scaling on the data
#   Consider the RBF kernel K(x, y) = exp (?????||x???y||2)
#   Use cross-validation to find the best parameter C and ??
#   Use the best parameter C and ?? to train the whole training set5
#   Test
#   
#   In this post, we present a variation of the methodology using R and the caret package. First, we set up for an analysis, loading the segmentation data set from the caret package and using the  caret's createDataPartition() function to produce training and test data sets.
  
  # Training SVM Models
  library(caret)
  library(dplyr)         # Used by caret
  library(kernlab)       # support vector machine 
  library(pROC)	       # plot the ROC curves
  
  ### Get the Data
  # Load the data and construct indices to divide it into training and test data sets.
  data(segmentationData)  	# Load the segmentation data set

  trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
  trainData <- segmentationData[trainIndex,]
  testData  <- segmentationData[-trainIndex,]
  trainX <-trainData[,4:61]        # Pull out the variables for training
  sapply(trainX,summary)           # Look at a summary of the training data
  
  Next, we carry out a two pass training and tuning process. In the first pass, shown in the code block below, we arbitrarily pick some tuning parameters and use the default caret settings for others. In the trainControl() function we specify 5 repetitions of 10 fold cross validation. in the train() function which actually does the work, we specify the radial kernel using the method parameter and the ROC as the metric for assessing performance. The tuneLength parameter is set to pick 9 arbitrary values for the C, the "cost" of the radial kernel. This parameter controls the complexity of the boundary between support vectors. The radial kernel also requires setting a smoothing parameter, sigma. In this first, pass we let train() use its default method of calculating an analytically derived estimate for sigma. Also note that we instruct train() to center and scale the data before running the analysis with the preProc parameter.
  
  ## SUPPORT VECTOR MACHINE MODEL
  # First pass
  set.seed(1492)
  # Setup for cross validation
  ctrl <- trainControl(method="repeatedcv",   # 10fold cross validation
  repeats=5,		    # do 5 repititions of cv
  summaryFunction=twoClassSummary,	# Use AUC to pick the best model
  classProbs=TRUE)
  
  
  #Train and Tune the SVM
  svm.tune <- train(x=trainX,
  y= trainData$Class,
  method = "svmRadial",   # Radial kernel
  tuneLength = 9,					# 9 values of the cost function
  preProc = c("center","scale"),  # Center and scale data
  metric="ROC",
  trControl=ctrl)
  
svm.tune
  
  
  # Second pass
  # Look at the results of svm.tune and refine the parameter space
  
  set.seed(1492)
  # Use the expand.grid to specify the search space	
  grid <- expand.grid(sigma = c(.01, .015, 0.2),
  C = c(0.75, 0.9, 1, 1.1, 1.25)
  )
  
  #Train and Tune the SVM
  svm.tune <- train(x=trainX,
  y= trainData$Class,
  method = "svmRadial",
  preProc = c("center","scale"),
  metric="ROC",
  tuneGrid = grid,
  trControl=ctrl)
  
  svm.tune
#Linear Kernel
set.seed(1492)                     
  
  #Train and Tune the SVM
  svm.tune2 <- train(x=trainX,
  y= trainData$Class,
  method = "svmLinear",
  preProc = c("center","scale"),
  metric="ROC",
  trControl=ctrl)	
  
  
  svm.tune2

rValues <- resamples(list(svm=svm.tune,svm.tune2))
rValues$values

summary(rValues)

bwplot(rValues,metric="ROC",ylab =c("linear kernel", "radial kernel"))	

model.matrix()
nnet::class.ind()


data(BloodBrain)
a <- train(bbbDescr, logBBB,'rpart')
b <- train(bbbDescr, logBBB,'lm')
c <- train(bbbDescr, logBBB,'neuralnet')
d <- train(bbbDescr, logBBB,'knn')
e <- train(bbbDescr, logBBB,'gbm')
res <- resamples(list(a,b,c,d,e))
summary(res)




circlize_dendrogram(dend)

t <- summary(y)
dim(t$values)[1]

t$metrics
t$statistics[[1]]

resampleHist()

plot(nb_Mod)
plot(gam_Mod$finalModel)

#class
svm
neuralnet
rf
#reg
svm

#http://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-8-340
data("segmentationData")
trainIndex <- createDataPartition(segmentationData$Case,p=.5,list=FALSE)
trainData <- segmentationData[trainIndex,]
testData  <- segmentationData[-trainIndex,]
trainX <-trainData[,4:61]

ctrl <- trainControl(method =  'boot')
ctrl1 <- trainControl(method =  'repeatedcv')

svml <- train(trainX,trainData$Class,method = 'svmLinear',
               trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svml
svml$finalModel
svml$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svml,trainX)))
confusionMatrix(table(testData$Class,predict(svml,testData[,4:61])))

svmr <- train(trainX,trainData$Class,method = 'svmRadial',
              trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svmr
svmr$finalModel
svmr$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svmr,trainX)))
confusionMatrix(table(testData$Class,predict(svmr,testData[,4:61])))

svmp <- train(trainX,trainData$Class,method = 'svmPoly',
              trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svmp
svmp$finalModel
svmp$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svmp,trainX)))
confusionMatrix(table(testData$Class,predict(svmp,testData[,4:61])))

svmrc <- train(trainX,trainData$Class,method = 'svmRadialCost',
              trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svmrc
svmrc$finalModel
svmrc$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svmrc,trainX)))
confusionMatrix(table(testData$Class,predict(svmrc,testData[,4:61])))

svmrw <- train(trainX,trainData$Class,method = 'svmRadialWeights',
              trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svmrw
svmrw$finalModel
svmrw$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svmrw,trainX)))
confusionMatrix(table(testData$Class,predict(svmrw,testData[,4:61])))

svmrs <- train(trainX,trainData$Class,method = 'svmRadialSigma',
              trControl = ctrl)
max(table(trainData$Class)/length(trainData$Class))#baseline
svmrs
svmrs$finalModel
svmrs$times$everything[1]
confusionMatrix(table(trainData$Class,predict(svmrs,trainX)))
confusionMatrix(table(testData$Class,predict(svmrs,testData[,4:61])))

summary(resamples(list(svml,svmr,svmp,svmrc,svmrw,svmrs)))#on validation test?


#nnet,BFGS
cl <- makeCluster(3)
registerDoParallel(cl)

z <- train(mydata[,!names(mydata) %in% 'Class'],mydata[,'Class'],method='nnet',
           trControl = ctrl)
z
z$finalModel
confusionMatrix(table(mydata[,'Class'],predict(z,mydata[,!names(mydata) %in% 'Class'])))
#maxit,size
nnetTunegrid <- expand.grid(.size = c(3,4,5),
                            .decay = c(0.08,0.09,0.1,0.12))
z1 <- train(mydata[,!names(mydata) %in% 'Class'],mydata[,'Class'],method='nnet',
           trControl = ctrl,tuneGrid = nnetTunegrid)
z1
z1$finalModel
confusionMatrix(table(mydata[,'Class'],predict(z1,mydata[,!names(mydata) %in% 'Class'])))

nnetTunegrid2 <- expand.grid(.size = c(4,5,6),
                            .decay = c(0.085,0.09,0.095))
z2 <- train(mydata[,!names(mydata) %in% 'Class'],mydata[,'Class'],method='nnet',
           trControl = ctrl,tuneGrid = nnetTunegrid2)
z2
z2$finalModel
confusionMatrix(table(mydata[,'Class'],predict(z2,mydata[,!names(mydata) %in% 'Class'])))

nnetTunegrid <- expand.grid(.size = c(6,10),.decay = c(0.07,0.075,0.08))
z1 <- train(mydata[,!names(mydata) %in% 'Class'],mydata[,'Class'],method='nnet',
            tuneGrid = nnetTunegrid, maxit = 150)
z1$times$everything
z1
z1$finalModel
confusionMatrix(table(mydata[,'Class'],predict(z1,mydata[,!names(mydata) %in% 'Class'])))

stop_cluster(cl)

#center scale necar?
#verify by searching data-on net



#mydata


svml <- readRDS('svmLinear.rds')
svmr <- readRDS('svmRadial.rds')
lda <- readRDS('lda.rds')
rf <- readRDS('rf.rds')
boostgbm <- readRDS('boostgbm.rds')
boostada <- readRDS('boostada.rds')
knn <- readRDS('knn.rds')
nb <- readRDS('nb.rds')
nnrpropp <- readRDS('nnrprop+sse.rds')
nnrpropm <- readRDS('nnrprop-sse.rds')
nnsag <- readRDS('nnsagsse.rds')
nnslr <- readRDS('nnslrsse.rds')

tim <- data.frame()
train_df <- data.frame()
val_df <- data.frame()
test_df <- data.frame()

lt <-list(svml,svmr,lda,rf,boostgbm,boostada,nb,knn)

for(i in 1:8){
  
obj <- lt[[i]]

#training
pred <- predict(obj,dat[!names(dat) %in% t])#nb warnings
obj_tab <- confusionMatrix(table(dat[,t],pred))
train_df <- rbind(train_df,c(obj_tab$overall[1:2],obj_tab$byClass))
#obj_tab$table
#class(obj_tab$byClass)
#validation
#val_df <- rbind(val_df, obj$results[,c("Accuracy",'Kappa')])

#test
pred2 <- predict(obj,itest[!names(dat) %in% t])
obj_tab2 <- confusionMatrix(table(itest[,t],pred2))
test_df <- rbind(test_df,c(obj_tab2$overall[1:2],obj_tab2$byClass))

tim <- rbind(tim,obj$times$everything[1:3])
}

val_df <- rbind(val_df, lt[[1]]$results[,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[2]]$results[3,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[3]]$results[,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[4]]$results[2,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[5]]$results[6,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[6]]$results[9,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[7]]$results[2,c("Accuracy",'Kappa')])
val_df <- rbind(val_df, lt[[8]]$results[3,c("Accuracy",'Kappa')])

colnames(train_df) <- c('Accuracy','Kappa','Sensitivity','Specificity','Pos Pred Value','Neg Pred Value','Prevalence','Detection Rate','Detection Prevalence','Balanced Accuracy')
colnames(val_df) <- c('Accuracy','Kappa')
colnames(test_df) <- c('Accuracy','Kappa','Sensitivity','Specificity','Pos Pred Value','Neg Pred Value','Prevalence','Detection Rate','Detection Prevalence','Balanced Accuracy')
colnames(tim) <- c('user','self', 'elapsed')

ltn <- list(nnrpropp,nnrpropm,nnsag,nnslr)
for(i in 1:4){
  obj <- ltn[[i]]
  
  pred <- apply(obj$net.result[[which.min(obj$result.matrix[1,])]],1,which.max)#min error rep only
  pred <- sort(unique(dat[,t]))[pred]
  net_tab <- confusionMatrix(table(dat[,t],pred))
  train_df <- rbind(train_df,c(net_tab$overall[1:2],net_tab$byClass))
  
  res <- compute(obj,itest[,!names(dat) %in% t],rep=which.min(obj$result.matrix[1,]))$net.result
  pred2 <- apply(res,1,which.max)#min error rep only
  pred2 <- sort(unique(itest[,t]))[pred2]
  net_tab2 <- confusionMatrix(table(itest[,t],pred2))
  test_df <- rbind(test_df,c(net_tab2$overall[1:2],net_tab2$byClass))
}

rownames(train_df) <- c('svmLinear','svmRadial','LDA','Random Forest','Gradient Boosting','AdaBoost','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(val_df) <- c('svmLinear','svmRadial','LDA','Random Forest','Gradient Boosting','AdaBoost','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
rownames(test_df) <- c('svmLinear','svmRadial','LDA','Random Forest','Gradient Boosting','AdaBoost','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(tim) <- c('svmLinear','svmRadial','LDA','Random Forest','Gradient Boosting','AdaBoost','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
#sapply(lt,function(a){a$method})

tr <- round(train_df[order(train_df$Kappa,decreasing = T),],3)
vl <-round(val_df[order(val_df$Kappa,decreasing = T),],3)
tt <- round(test_df[order(test_df$Kappa,decreasing = T),],3)
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(78.43,25.85,18.78,41.77))
View(tt)

names(tm_withnn) <- c('svmLinear','svmRadial','LDA','Random Forest','Gradient Boosting','AdaBoost','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

max(table(dat$Class)/length(dat$Class))#baseline

#***
#seeds"boot632" in which case B is the number of resamples plus 1
isd <- vector(mode='list',ifelse(grepl("cv", input$_Res_met,11,26))
for(i in 1:(length(isd)-1)) isd[[i]] <- sample.int(1000, switch(,svml=1,svmr=3,lda=1,rf=3,boostgbm=9,boostada=9,knn=3,nb=2))# #tun para
isd[[length(isd)]] <- sample.int(1000, 1)

#switch(svml=1,svmr=3,lda=1,rf=3,boostgbm=9,boostada=9,knn=3,nb=2)
#switch(,'1'=1,'2'=3,'3'=1,'4'=3,'5'=9,'6'=9,'7'=3,'8'=2)
***#

#knowledeg data class3
library(gdata)
traindata <- read.xls('http://archive.ics.uci.edu/ml/machine-learning-databases/00257/Data_User_Modeling_Dataset_Hamdi%20Tolga%20KAHRAMAN.xls',sheet = 2)
traindata <- traindata[,1:6]
colnames(traindata)[6] <- 'UNS'

testdata <- read.xls('http://archive.ics.uci.edu/ml/machine-learning-databases/00257/Data_User_Modeling_Dataset_Hamdi%20Tolga%20KAHRAMAN.xls',sheet = 3)
testdata <- testdata[,1:6]
colnames(testdata)[6] <- 'UNS'
levels(testdata$UNS)[4] <- 'very_low'

dat <- rbind(traindata,testdata)

write.csv(dat,'class3.csv')

dat <- read.csv('class3.csv')
set.seed(5678)
idi <- createDataPartition(dat$UNS,p=0.8,list=F)

itest <- dat[-idi,2:7]
dat <- dat[idi,2:7]


svml <- readRDS('svmLinear.rds')
svmr <- readRDS('svmRadial.rds')
svmp <- readRDS('svmPoly.rds')
lda <- readRDS('lda.rds')
rf <- readRDS('rf.rds')
rp <- readRDS('rpart.rds')
boostgbm <- readRDS('boostgbm.rds')
boostxgb <- readRDS('boostxgb.rds')
knn <- readRDS('knn.rds')
nb <- readRDS('nb.rds')
nnrpropp <- readRDS('nnrprop+.rds')
nnrpropm <- readRDS('nnrprop-.rds')
nnsag <- readRDS('nnsag.rds')
nnslr <- readRDS('nnslr.rds')

t <- 'UNS'
tim <- data.frame()
train_df <- data.frame()
val_df <- data.frame()
test_df <- data.frame()

lt <-list(svml,svmr,svmp,lda,rf,rp,boostgbm,boostxgb,nb,knn)


for(i in 1:10){
  
  obj <- lt[[i]]
  
  #training
  pred <- predict(obj,dat[!names(dat) %in% t])#nb warnings
  obj_tab <- confusionMatrix(table(dat[,t],pred))
  train_df <- rbind(train_df,c(obj_tab$overall[1:2],obj_tab$byClass[which.max(table(dat[,t])),]))
  #sensitivity of major class only because 4 rows
  
  #test
  pred2 <- predict(obj,itest[!names(dat) %in% t])
  obj_tab2 <- confusionMatrix(table(itest[,t],pred2))
  test_df <- rbind(test_df,c(obj_tab2$overall[1:2],obj_tab2$byClass[which.max(table(dat[,t])),]))
  
  tim <- rbind(tim,obj$times$everything[1:3])
}

for(i in 1:10){
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
  train_df <- rbind(train_df,c(net_tab$overall[1:2],net_tab$byClass[which.max(table(dat[,t])),]))
  
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
  test_df <- rbind(test_df,c(net_tab2$overall[1:2],net_tab2$byClass[which.max(table(dat[,t])),]))
}

rownames(train_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(val_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
rownames(test_df) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(tim) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')

tr <- round(train_df[order(train_df$Kappa,decreasing = T),],3)
vl <-round(val_df[order(val_df$Kappa,decreasing = T),],3)
tt <- round(test_df[order(test_df$Kappa,decreasing = T),],3)
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(31.30,31.34,61.70,50.04))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

max(table(dat[,t])/length(dat[,t]))#baseline


#ionosphere class2
library(mlbench)
data("Ionosphere")
#write.csv(Ionosphere,'class2.csv')

dat <- Ionosphere#read.csv('class3.csv')
dat$V1 <- as.numeric(dat$V1)
dat$V2 <- as.numeric(dat$V2)
set.seed(5678)
t <- 'Class'
idi <- createDataPartition(dat[,t],p=0.8,list=F)

itest <- dat[-idi,]
dat <- dat[idi,]


svml <- readRDS('svmLinear.rds')
svmr <- readRDS('svmRadial.rds')
svmp <- readRDS('svmPoly.rds')
sqda <- readRDS('sqda.rds')
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

lt <-list(svml,svmr,svmp,sqda,rf,rp,boostgbm,boostxgb,boostada,reglog,nb,knn)


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

rownames(train_df) <- c('svmLinear','svmRadial','svmPoly','sQDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','AdaBoost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(val_df) <- c('svmLinear','svmRadial','svmPoly','sQDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','AdaBoost','Logistic','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')
rownames(test_df) <- c('svmLinear','svmRadial','svmPoly','sQDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','AdaBoost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
rownames(tim) <- c('svmLinear','svmRadial','svmPoly','sQDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','AdaBoost','Logistic','Naive Bayes','KNN')#,'NNrprop+','NNrprop-','NNsag','NNslr')

tr <- round(train_df[order(train_df$Kappa,decreasing = T),],3)
vl <-round(val_df[order(val_df$Kappa,decreasing = T),],3)
tt <- round(test_df[order(test_df$Kappa,decreasing = T),],3)
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(31.30,31.34,61.70,50.04))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','sQDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','AdaBoost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

max(table(dat[,t])/length(dat[,t]))#baseline


#segmentation class1
library(caret)
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
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(42.75,37.00,37.67,39.83))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','LDA','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Adaboost','Logistic','Naive Bayes','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

max(table(dat[,t])/length(dat[,t]))#baseline

#boston reg1
library(mlbench)
library(caret)
library(neuralnet)
data("BostonHousing")
dat <- BostonHousing

dat$chas <- as.numeric(dat$chas)

set.seed(5678)
t <- 'medv'
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
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(0,0,0,0))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

RMSE(rep(mean(dat[,t]),length(dat[,t])),dat[,t])#baseline


#Concrete reg2
library(AppliedPredictiveModeling)
data(concrete)
dat <- concrete

library(caret)
library(neuralnet)


set.seed(5678)
t <- 'CompressiveStrength'
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
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(0,0,0,0))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

RMSE(rep(mean(dat[,t]),length(dat[,t])),dat[,t])#baseline






#airfoil reg3
z <- read.table('http://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat')
write.csv(z,'reg3.csv')

########################start
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
tm <- tim[order(tim$elapsed),]

tm_withnn <- c(tim[,3],c(0,0,0,0))
View(tt)
View(tr)
View(vl)

names(tm_withnn) <- c('svmLinear','svmRadial','svmPoly','Random Forest','Rpart','Gradient Boosting','eXtreme Grad. B.','Linear Reg.','Quadratic Reg.','KNN','NNrprop+','NNrprop-','NNsag','NNslr')
tm_withnn <- as.data.frame(sort(tm_withnn))
colnames(tm_withnn) <- 'Time (s);'
View(tm_withnn)

RMSE(rep(mean(dat[,t]),length(dat[,t])),dat[,t])#baseline







saveRDS(svm_Mod,'svmLinear.rds')
saveRDS(svm_Mod,'svmRadial.rds')
saveRDS(svm_Mod,'svmPoly.rds')
#saveRDS(lda_Mod,'lda.rds')
saveRDS(rf_Mod,'rf.rds')
saveRDS(rf_Mod,'rpart.rds')
saveRDS(reg_Mod,'reglin.rds')
saveRDS(reg_Mod,'regquad.rds')
saveRDS(boost_Mod,'boostgbm.rds')
saveRDS(boost_Mod,'boostxgb.rds')
#saveRDS(boost_Mod,'boostada.rds')
#saveRDS(nb_Mod,'nb.rds')
saveRDS(knn_Mod,'knn.rds')
saveRDS(neuralnet_Mod,'nnrprop+.rds')
saveRDS(neuralnet_Mod,'nnrprop-.rds')
saveRDS(neuralnet_Mod,'nnsag.rds')
saveRDS(neuralnet_Mod,'nnslr.rds')


zt <- cbind(t1,t2,t3)
colnames(zt) <- c('BostonHousing (s)','Concrete (s)','Airfoil Self-Boise (s)')
View(zt)


tt1 <- tm_withnn[order(row.names(tm_withnn)),,drop=F]#reg1time
ttest1 <- tt[order(row.names(tt)),]#reg
df1 <- cbind(ttest1,tt1,data.frame(id=factor(1:dim(tt1)[1])))
colnames(df1)[3] <- 'time'

tt2 <- tt2[order(row.names(tt2)),,drop=F]#reg1time
ttest2 <- ttest2[order(row.names(ttest2)),]
df2 <- cbind(ttest2,tt2,data.frame(id=factor(1:dim(tt3)[1])))
colnames(df2)[3] <- 'time'

tt3 <- tt3[order(row.names(tt3)),,drop=F]#reg1time
ttest3 <- ttest3[order(row.names(ttest3)),]
df3 <- cbind(ttest3,tt3,data.frame(id=factor(1:dim(tt3)[1])))
colnames(df3)[3] <- 'time'

z <- rbind(df1,df2,df3)

z$id <- factor(rep(row.names(z)[1:14],3))

library(ggplot2)

regplot <-  ggplot(z, aes(y= RMSE, x= time))+ 
  geom_point(aes(shape=id,colour = id),size=3)+
  scale_shape_manual(values=1:nlevels(z$id))+
  labs( y="RMSE", x="Time (s)")

pdf('regplot.pdf')
regplot
dev.off()

a1 <- tt4
a2 <- tt5
a3 <- tt6

b1 <- ttest4
b2 <- ttest5
b3 <- ttest6

#class
tt4 <- tt4[order(row.names(tt4)),,drop=F]
ttest4 <- ttest4[order(row.names(ttest4)),]
df4 <- cbind(ttest4,tt4,data.frame(id=factor(1:dim(tt4)[1])))
colnames(df4)[11] <- 'time'

tt5 <- tt5[order(row.names(tt5)),,drop=F]#reg1time
ttest5 <- ttest5[order(row.names(ttest5)),]
df5 <- cbind(ttest5,tt5,data.frame(id=factor(1:dim(tt5)[1])))
colnames(df5)[11] <- 'time'

tt6 <- tt6[order(row.names(tt6)),,drop=F]#reg1time
ttest6 <- ttest6[order(row.names(ttest6)),]
df6 <- cbind(ttest6,tt6,data.frame(id=factor(1:dim(tt6)[1])))
colnames(df6)[11] <- 'time'



z2 <- rbind(df4,df5,df6)
z2$id <- factor(c(rownames(df4),rownames(df5),rownames(df6)))
#write.csv(z2,'classstat.csv')
library(ggplot2)

classplot <-  ggplot(z2, aes(y= Accuracy, x= time))+ 
  geom_point(aes(shape=id,colour = id),size=3)+
  scale_shape_manual(values=1:nlevels(z2$id))+
  labs( y="Acuuracy", x="Time (s)")

pdf('classplot.pdf')
classplot
dev.off()


# ggplot(df, aes(x= RMSE, y= time))+ 
#   geom_point(aes(shape=row.names(df),colour = row.names(df)),size=3)+
#   scale_shape_manual(values=1:nlevels(factor(row.names(df))))+
#   labs(title = "Summary of RMSE with time", x="RMSE", y="Time (s)")
# 
#+ geom_text(aes(label=row.names(df)),hjust=-0.2, vjust=-0.3)

#tab1
tab1 <- data.frame(Time=c(0.05,0.08,0.35,0.06,0.30,0.20,66.09),
                   Calinski_Harabasz=c(375.80,374.61,349.29,329.78,314.67,352.83,234.14),
                   Silhouette=c(0.47,0.47,0.53,0.45,0.35,0.45,0.27),
                   id=factor(1:7))
rownames(tab1) <- c('kmeans','pam','clara','hclust','dbscan','nbclust','mclust')

tab2 <- data.frame(Time=c(0.07,0.55,0.15,0.12,0.13,3.51),
                   Calinski_Harabasz=c(54.97,58.14,49.17,59.03,43.33,57.53),
                   Silhouette=c(0.26,0.25,0.19,0.20,0.10,0.29),
                   id=factor(1:6))
rownames(tab2) <- c('kmeans','pam','clara','hclust','dbscan','nbclust')

z3 <- rbind(tab1,tab2)
z3$id <- rep(row.names(z3)[1:7],2)[1:13]

clusplot <-  ggplot(z3[c(1:6,8:13),], aes(y= Calinski_Harabasz, x= Time))+ 
  geom_point(aes(shape=id,colour = Silhouette),size=3)+
  scale_shape_manual(values=1:length(z3$id))+
  labs( y="Calinski_Harabasz Criteria", x="Time (s)")
clusplot
write.csv(z3,'clusstat.csv')

pdf('clusplot.pdf')
clusplot
dev.off()
#tab2
tab1 <- data.frame(Time=c(0.05703712, 0.08698702, 0.3515151, 0.06108713, 0.30057, 0.20836, 66.096),
                   Calinski_Harabasz=c(375.80,374.61,349.29,329.78,314.67,352.83,234.14), 
                   CIndex=c(0.0558,0.0560,0.0826,0.0698,0.3214,0.06356,0.07962),
                   id=factor(1:7),id2=factor(rep(1,7)))
rownames(tab1) <- c('kmeans','pam','clara','hclust','dbscan','nbclust','mclust')

tab2 <- data.frame(Time= c(0.07640004, 0.553493, 0.156563, 0.1242771, 0.132267, 3.51663),
                   Calinski_Harabasz=c(54.97529, 58.14732, 49.17934, 49.03108, 43.33908, 57.53891), 
                   CIndex=c(0.07792885, 0.09497314, 0.1318833, 0.1342278, 0.3199434, 0.1121687),
                   id=factor(1:6),id2=factor(rep(2,6)))
rownames(tab2) <- c('kmeans','pam','clara','hclust','dbscan','nbclust')

z3 <- rbind(tab1,tab2)
z3$id <- rep(row.names(z3)[1:7],2)[1:13]

clusplot <-  ggplot(z3[c(1:6,8:13),], aes(y= Calinski_Harabasz, x= CIndex))+ 
  geom_point(aes(shape=id,colour = id2),size=3)+
  scale_shape_manual(values=1:length(z3$id))+
  labs( y="Calinski_Harabasz Criteria", x="CIndex)")
clusplot
#write.csv(z3,'clusstat3.csv')

pdf('clusplot3.pdf')
clusplot
dev.off()



#metric results
setwd('rdir/proRepo/')
reg <- read.csv('regstat.csv')
clas <- read.csv('classstat.csv')

r1 <- reg[1:14,]
r2 <- reg[15:28,]
r3 <- reg[29:42,]

r1 <- reg[1:14,]
r2 <- reg[15:28,]
r3 <- reg[29:42,]

od <- order(r1$RMSE+ r2$RMSE + r3$RMSE)
regfin <- cbind(r1[od,2:3], r2[od,2:3],r2[od,2:3])
rownames(regfin) <- r1[od,5]

c1 <- clas[1:16,]
c1[17,] <- c(NA,rep(0,11),NA)
c1[17,1] <- 'sQDA'
c1[17,13] <- 'sQDA'

c2 <- clas[17:32,]
c2[17,] <- c(NA,rep(0,11),NA)
c2[17,1] <- 'LDA'
c2[17,13] <- 'LDA'

c3 <- clas[33:46,]
c3[15,] <- c(NA,rep(0,11),NA)
c3[15,1] <- 'sQDA'
c3[15,13] <- 'sQDA'
c3[16,] <- c(NA,rep(0,11),NA)
c3[16,1] <- 'Logistic'
c3[16,13] <- 'Logistic'
c3[17,] <- c(NA,rep(0,11),NA)
c3[17,1] <- 'Adaboost'
c3[17,13] <- 'Adaboost'

c1 <-   c1[order(c1$X),]
c2 <-   c2[order(c2$X),]
c3 <-   c3[order(c3$X),]

sm <- c1$Kappa+ c2$Kappa + c3$Kappa
sm[1] <- sm[1]*3/2
sm[5] <- sm[5]*3/2
sm[6] <- sm[6]*3/2
sm[14] <- sm[14]*2.5
#sm[15:16] <- sm[15,16]
odc <- order(sm,decreasing = T)
classfin <- cbind(c1[odc,2:3], c2[odc,2:3],c3[odc,2:3])
rownames(classfin) <- c1[odc,13]
classfin[16,5:6] <- c('-','-')
classfin[14,5:6] <- c('-','-')
classfin[12,5:6] <- c('-','-')
classfin[6,3:4] <- c('-','-')
classfin[14,1:2] <- c('-','-')
#write.csv(classfin,'classresrepo.csv')

tr1 <- r1[,4:5]
tr2 <- r2[,4:5]
tr3 <- r3[,4:5]

trt <- cbind(tr1[,1,drop=F],tr2[,1,drop=F],tr3)
trt[15,] <- c(0,0,0,NA)
trt[16,] <- c(0,0,0,NA)
trt[17,] <- c(0,0,0,NA)
trt[18,] <- c(0,0,0,NA)
trt[19,] <- c(0,0,0,NA)

trt$id <- as.character(trt$id)
trt[15,4] <- 'Adaboost'
trt[16,4] <- 'Logistic'
trt[17,4] <- 'sQDA'
trt[18,4] <- 'Naive Bayes'
trt[19,4] <- 'LDA'

a <- trt[order(trt$id),]

cr1 <- c1[,12:13]
cr2 <- c2[,12:13]
cr3 <- c3[,12:13]


crt <- cbind(cr1[,1,drop=F],cr2[,1,drop=F],cr3)
crt[18,] <- c(0,0,0,NA)
crt[19,] <- c(0,0,0,NA)

crt$id <- as.character(crt$id)
crt[18,4] <- 'Linear Reg.'
crt[19,4] <- 'Quadratic Reg.'

b <- crt[order(crt$id),]

ba <- cbind(b[,1:3],a[,1:3])
rownames(ba) <- b$id

for(i in 1:dim(ba)[2]){
  ba[,i] <- round(ba[,i],2)
}

ba[1,3:6] <- c('-','-','-','-')
ba[5,2] <- '-'
ba[5,4:6] <- c('-','-','-')
ba[6,1:3] <- c('-','-','-')
ba[7,3:6] <- c('-','-','-','-')
ba[8,4:6] <- c('-','-','-')
ba[13,1:3] <- c('-','-','-')
ba[16,1] <- '-'
ba[16,3:6] <- c('-','-','-','-')
colnames(ba) <- c(' Segmentation','Ionosphere','Knowledge M','Boston Housing','Concrete CS','Airfoil SN')
write.csv(ba,'timsrepo.csv')
