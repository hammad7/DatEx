
# nnutil_prin <- function (x, ...) 
# {
#   matrix <- x$result.matrix
#   #cat("Call: ", deparse(x$call), "\n\n", sep = "")
#   if (!is.null(matrix)) {
#     if (ncol(matrix) > 1) {
#       cat(ncol(matrix), " repetitions were calculated.\n\n", 
#           sep = "")
#       sorted.matrix <- matrix[, order(matrix["error", ])]
#       if (any(rownames(sorted.matrix) == "aic")) {
#         print(t(rbind(Error = sorted.matrix["error", 
#                                             ], AIC = sorted.matrix["aic", ], BIC = sorted.matrix["bic", 
#                                                                                                  ], `Reached Threshold` = sorted.matrix["reached.threshold", 
#                                                                                                                                         ], Steps = sorted.matrix["steps", ])))
#       }
#       else {
#         print(t(rbind(Error = sorted.matrix["error", 
#                                             ], `Reached Threshold` = sorted.matrix["reached.threshold", 
#                                                                                    ], Steps = sorted.matrix["steps", ])))
#       }
#     }
#     else {
#       cat(ncol(matrix), " repetition was calculated.\n\n", 
#           sep = "")
#       if (any(rownames(matrix) == "aic")) {
#         print(t(matrix(c(matrix["error", ], matrix["aic", 
#                                                    ], matrix["bic", ], matrix["reached.threshold", 
#                                                                               ], matrix["steps", ]), dimnames = list(c("Error", 
#                                                                                                                        "AIC", "BIC", "Reached Threshold", "Steps"), 
#                                                                                                                      c(1)))))
#       }
#       else {
#         print(t(matrix(c(matrix["error", ], matrix["reached.threshold", 
#                                                    ], matrix["steps", ]), dimnames = list(c("Error", 
#                                                                                             "Reached Threshold", "Steps"), c(1)))))
#       }
#     }
#   }
#   cat("\n")
# }

#library(shiny)
# library(UsingR)#all
library(threejs)
library(ggplot2)
library(cluster)#pamk()
library(fpc)#pamk
library(vegan)#cascadeKM(),CAlinski
library(mclust)#mclust
library(NbClust)
library(neuralnet)
library(caret)
library(rattle)#rpart plot
# library(kernlab)
# library(rpart)
# library(randomForest)
# library(mboost)
# library(ada)
# library(gbm)
# library(GAMBoost)
# library(xgboost)
# library(klaR)#nb
options(shiny.maxRequestSize = 50*1024^2)
#     datalist <- data(package = .packages(all.available = TRUE))
#mydata <- get(data(list=datalist$results[533,3], package=datalist$results[533,1]))
# head(datalist$results)
# # number of packages
# length(unique(datalist$results[,1]))
# # no of datasets
# length(unique(datalist$results[,3]))
#as<-count(datalist$results[,1])
#as[order(as$freq,decreasing = T),]

shinyServer(
  function(input,output,session){
    
    reacVal <- reactiveValues()
    
    observeEvent(input$dataname,{
      if(input$dataname!='')
      {
        itemName <- strsplit(input$dataname,' *--')[[1]][1]
        itemIndex <- which(datalist$results[,3]==itemName)
        mydata <<- get(data(list=datalist$results[itemIndex,3], package=datalist$results[itemIndex,1]))
        reacVal$mydata0 <- mydata;print('data0')
      }
    })
    
    observeEvent(input$file,{
      if(input$file!='')
      {
#         itemName <- strsplit(input$dataname,' *--')[[1]][1]
#         itemIndex <- which(datalist$results[,3]==itemName)
#         mydata <<- get(data(list=datalist$results[itemIndex,3], package=datalist$results[itemIndex,1]))
        mydata <<- read.csv(input$file$datapath);print('data0') 
        reacVal$mydata0 <- mydata
      }
    })
    
    datachoice <- eventReactive(reacVal$mydata0, {
      if(is.null(reacVal$mydata0))
        return()
      checkboxGroupInput('uCGI','Features to include:',
                               choices = colnames(reacVal$mydata0),
                               selected =colnames(reacVal$mydata0) )
    }) 
    output$choiceg <- renderUI({
      datachoice()
    })
    observeEvent(input$uCGI,{
      if(is.null(reacVal$mydata0))
         return()
      mydata <<- as.data.frame(reacVal$mydata0[,input$uCGI])#mydata intermediate/final var
      cat('data1ch')
      print(dim(mydata))
      if(length(input$uCGI)==1)
        colnames(mydata) <- input$uCGI
      reacVal$mydata1 <- mydata
    })

    #pm target init
    data_t <- eventReactive(reacVal$mydata1, {
      if(is.null(reacVal$mydata1))
        return()
      selectInput('t_f','Target:',
                  choices = colnames(reacVal$mydata1))
    }) 
    output$t_g <- renderUI({
      data_t()
    })
    
    #     observeEvent(input$t_f,{
    #       if(is.null(input$t_f))
    #         return()
    #       target <<- input$t_f
    #       reacVal$target <- input$t_f
    #     })
    
    
    #factors
    datafac <- eventReactive(reacVal$mydata1, {
      if(is.null(reacVal$mydata1))
        return()
      checkboxGroupInput('facs','Factors(Categorical):',
                         choices = colnames(reacVal$mydata1),
                         selected = colnames(reacVal$mydata1)[sapply(reacVal$mydata1, class) %in% c('factor','character')] )
    }) 
    output$data_fac <- renderUI({
      datafac()
    })
    observeEvent(input$facs,{
      if(is.null(reacVal$mydata1))
        return()
#         conv <- function(obj,types){
#         out <- lapply(1:length(obj),
#                       FUN = function(i){FUN1 <- switch(types[i],
#                                                        character = as.character,
#                                                        numeric = as.numeric,
#                                                        factor = as.factor,
#                                                        complex = as.complex,
#                                                        logical=as.logical) 
#                       obj[,i] <- FUN1(obj[,i])})
#         names(out) <- colnames(obj)
#         as.data.frame(out,stringsAsFactors = FALSE)
#         }
#       #original classes  
#       reacVal$mydata2 <- conv(reacVal$mydata1,sapply(reacVal$mydata1,class))
      #other numeric
      md2 <- reacVal$mydata1
      md2[!colnames(reacVal$mydata1)%in%input$facs] <- lapply(reacVal$mydata1[!colnames(reacVal$mydata1)%in%input$facs],as.numeric)
      md2[colnames(reacVal$mydata1)%in%input$facs] <- lapply(reacVal$mydata1[colnames(reacVal$mydata1)%in%input$facs],as.factor)
      names(md2) <- colnames(md2)
      mydata <<- md2;print('data2fac')
      reacVal$mydata2 <- as.data.frame(md2)
    },ignoreNULL = F)
    
    
    #data part
    dprt <- eventReactive(input$t_f, {
      if(is.null(input$t_f))
        return()
      tagList(
        textInput('dpart','Training/Test/Validation',value = '80/20'),
        actionButton("button_part", "Partition")
      )
    })
    observeEvent(input$button_part,{
      if(is.null(reacVal$mydata2))
        return()
      
      #p <- (as.numeric(strsplit(input$dpart,'/')[[1]]))/100
      p <- (as.numeric(strsplit(input$dpart,'/')[[1]]))/100
      
      if(exists('SEED'))#5678
        set.seed(SEED)#identical
      inTraining <- createDataPartition(reacVal$mydata2[,input$t_f], p=p[1], list=FALSE)
      #nontraining <- reacVal$mydata1[-inTraining,]
      mydata <<- reacVal$mydata2[inTraining,]
      
      #inTestval <- createDataPartition(nontraining[,input$t_f], p=p[2]/(p[2]+p[3]), list=FALSE)
      
      #testing <<- nontraining[inTestval,]
      testing <<- reacVal$mydata2[-inTraining,]
      print('mydata')
      reacVal$mydata <- mydata
      reacVal$testing <- testing
      #validation <<- nontraining[-inTestval,]
      #reacVal$validation <- validation
      
      if(exists('neuralnet_Mod')){#environment
        rm(neuralnet_Mod)
      }
      if(exists('svm_Mod')){
        rm(svm_Mod)
      }
      if(exists('gam_Mod')){
        rm(gam_Mod)
      }
      if(exists('lda_Mod')){
        rm(lda_Mod)
      }
      if(exists('rf_Mod')){
        rm(rf_Mod)
      }
      if(exists('boost_Mod')){
        rm(boost_Mod)
      }
      if(exists('reg_Mod')){
        rm(reg_Mod)
      }
      if(exists('nb_Mod')){
        rm(nb_Mod)
      }
      if(exists('knn_Mod')){
        rm(knn_Mod)
      }
    })
    output$partbt <- renderUI({
      dprt()
    })
    
    
#     #Neural net
#     nnplt <- eventReactive(input$buttonpm, {
#       layer <- as.numeric(strsplit(input$nnlay,',')[[1]])
#       lr <- NULL
#       if(input$nnalgo=='backprop')
#         lr <-input$nnlr
#       
#       ff <- formula(paste('~',paste(paste(names(mydata),collapse='+'),'-1')))
#       m <<- model.matrix(ff,data=mydata)
#       
#       targs <- NULL
#       #if target is factor /char
#       if(!(input$t_f %in% colnames(m)))#'Species' %in% colnames(v)
#       {
#         targs <- paste(input$t_f,unique(mydata[,input$t_f]),sep='')
#         form <- paste(paste(targs,collapse = '+'),paste(colnames(m)[!colnames(m) %in% targs],collapse = ' + '),sep = '~')
#         #also remove '.0' from new colnames
#       }
#       else
#         form <- paste(c(input$t_f , paste(names(mydata)[!names(mydata) %in% input$t_f],collapse = ' + ')),collapse = ' ~ ')
#       
#       #cat(form)
#       ctrl <- trainControl(method =  input$nnet_Res_met)
#       net <- train(m[,!colnames(m) %in% targs], m[,targs],method = 'neuralnet',
#                        hidden=layer,threshold = input$nnthresh,
#                        stepmax = input$nniter,rep=input$nnrep,
#                        algorithm = input$nnalgo,learningrate = lr,
#                    trControl = ctrl)
#       neuralnet_Mod <<- net#for resample
#       #print(net)######to be done-different
#       reacVal$nn <- net
#       
#       #wil  it predict itself
# #*       if(class(mydata[,input$t_f])=='factor'){
# #*         pred <- apply(net$net.result[[1]],1,which.max)#first rep only
# #*         pred <- sort(unique(mydata[,input$t_f]))[pred]
# #*         #pred <- factor(net$response%*%(1:dim(net$response)[2]), labels = unique(mydata[,input$t_f])) 
# #*         #print(net$response)
# #*         #print(mydata[,input$t_f])
# #*         reacVal$net_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
# #*       }
#       plot.nn(net$finalModel)
#     })
#     output$nnplotpm <- renderPlot({
#       nnplt()
#     })
#     output$nnbuttonpm <- renderUI({
#       if(is.null(reacVal$mydata))#input$t_f
#         return()
#       tagList(
#         #numericInput('nnl','Layers/Neurons',1,1,7,step = 1),
#         textInput('nnlay','Neurons/Layer(\',\' separated)',value = '1'),
#         numericInput('nnthresh','Threshold',0.01,0),
#         numericInput('nniter','Maximum Iterations',1e+05,1,step = 1),
#         numericInput('nnrep','Repetitions',1,1,step = 1),
#         selectInput('nnalgo','Algo', c( 'backprop', 'rprop+', 'rprop-', 'sag', 'slr'),
#                     selected = 'rprop+'),
#         numericInput('nnlr','Learning Rate(backprop)',NULL,0),
#         selectInput('nnet_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" , "none"),
#                     selected = 'boot'),
#         actionButton("buttonpm", "Run Neural Network")
#       )
#     })
#     
#     setnn <- eventReactive(reacVal$nn,{
#       reacVal$nn
#     })
#     output$nnt <- renderPrint({
#       setnn()
#     })
#     
#     setnet_tab <- eventReactive(reacVal$net_tab,{
#       reacVal$net_tab
#     })
#     output$net_tab <- renderPrint({
#       setnet_tab()
#     })

#replace names() by colnames()    
    

    #Neural net 3+
    nnplt <- eventReactive(input$buttonpm, {
      layer <- as.numeric(strsplit(input$nnlay,',')[[1]])
      lr <- NULL
      if(input$nnalgo=='backprop')
        lr <-input$nnlr
      
      if(class(mydata[,input$t_f])=='character' |class(mydata[,input$t_f])=='factor')
      {
      maxs <<- apply(mydata[!names(mydata) %in% input$t_f], 2, max) 
      mins <<- apply(mydata[!names(mydata) %in% input$t_f], 2, min)
      div <- maxs-mins
      div[div==0] <- 1 #avoid NaN
      m_sc <<- scale(mydata[!names(mydata) %in% input$t_f],scale=div)
      m_sc <<- cbind(m_sc,mydata[input$t_f])
      }
      else
      {
        maxs <<- apply(mydata, 2, max) 
        mins <<- apply(mydata, 2, min)
        div <- maxs-mins
        div[div==0] <- 1
        m_sc <<- scale(mydata,scale=div)
      }
#       net <- neuralnet(form, m_sc,
#                        hidden=c(6,4),
#                        linear.output = T,
#                        lifesign = 'full')
#       pred <-  net$net.result[[which.min(net$result.matrix[1,])]] *(maxs['medv']-mins['medv']) + mean(mydata$medv)
#       sqrt(sum((mydata[,'medv']-pred)^2)/nrow(mydata))
#       sum((pred - mean(mydata[,'medv']))^2)/sum((mydata[,'medv'] - mean(mydata[,'medv']))^2)
      
      
    #  if(class(mydata[,input$t_f])=='character' |class(mydata[,input$t_f])=='factor')
      #{ 
        ff <- formula(paste('~',paste(paste(names(mydata),collapse='+'),'-1')))
        m <- model.matrix(ff,data=as.data.frame(m_sc))#mydata)#global m
      #}
    #  else
    #    m <- mydata
      
      targs <- NULL
      #if target is factor /char
      if(!(input$t_f %in% colnames(m)))#'Species' %in% colnames(v)
      {
        targs <- paste(input$t_f,unique(mydata[,input$t_f]),sep='')
        form <- paste(paste(targs,collapse = '+'),paste(colnames(m)[!colnames(m) %in% targs],collapse = ' + '),sep = '~')
        #also remove '.0' from new colnames
      }
      else
        form <- paste(c(input$t_f , paste(names(mydata)[!names(mydata) %in% input$t_f],collapse = ' + ')),collapse = ' ~ ')
      
      #cat(form)
      isd <<- sample.int(10000,1) #round(abs(rnorm(1,100,100)),0)
      set.seed(isd)
      print(paste('SEED:',isd))
      

      net <- neuralnet(form, m,
                        hidden=layer,threshold = input$nnthresh,
                        stepmax = input$nniter,rep=input$nnrep,
                        algorithm = input$nnalgo,err.fct = input$nnef,
                        act.fct = input$nnaf,learningrate = lr,
                       linear.output = input$nnlo,
                       lifesign = 'full')
      neuralnet_Mod <<- net#for resample
      #print(net)######to be done-different
      reacVal$nn <- net
      if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){#or factor
        pred <- apply(net$net.result[[which.min(net$result.matrix[1,])]]#*(maxs[input$t_f]-mins[input$t_f]) + mean(mydata[,input$t_f])
                      ,1,which.max)#min error rep only
        #pred <- sort(unique(mydata[,input$t_f]))[pred]
        pred <- gsub(input$t_f,'',colnames(net$response))[pred]
        reacVal$net_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        
#        neuralnet_Mod$weights[[which.min(neuralnet_Mod$result.matrix[1,])]]
#         md <- mydata
#         md[,1] <- rep(1,dim(mydata)[1])
#         id <- which.min(neuralnet_Mod$result.matrix[1,])
#         as.matrix(md[1:3,])%*%neuralnet_Mod$weights[[id]][[1]]
        
        #scale by train data scaling metrics
        test_m_sc <- scale(testing[,!names(mydata) %in% input$t_f],scale=div)
        
        res <- compute(net,test_m_sc,rep=which.min(net$result.matrix[1,]))$net.result
        #normalise by train data info
        #res <- res*(maxs[input$t_f]-mins[input$t_f]) + mean(mydata[,input$t_f])
        pred2 <- apply(res,1,which.max)#min error rep only
        #pred2 <- sort(unique(mydata[,input$t_f]))[pred2]
        pred2 <- gsub(input$t_f,'',colnames(net$response))[pred2]
        reacVal$net_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
      }
      else
      {
        pred <- net$net.result[[which.min(net$result.matrix[1,])]]*(maxs[input$t_f]-mins[input$t_f]) + mean(mydata[,input$t_f])#min error rep only
        #rmse <- (sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata)))
        rmse <- RMSE(mydata[,input$t_f],pred)
        #rsqua <- (sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2))
        rsqua <- R2(mydata[,input$t_f],pred)#corr not traditional R2
        dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
        rownames(dftr) <- 'Training set:    '
        reacVal$net_tab <- dftr
          #RMSE(pred,mydata[,input$t_f])
        
        #normalise by train data info
        test_m_sc <- scale(testing[,!names(mydata) %in% input$t_f],
                           scale=div[!names(mydata) %in% input$t_f])
        res <- compute(net,test_m_sc,rep=which.min(net$result.matrix[1,]))$net.result
        #denormalise by train data info
        res <- res*(maxs[input$t_f]-mins[input$t_f]) + mean(mydata[,input$t_f])
        #pred2 <- apply(res,1,which.max)#min error rep only
        #pred2 <- sort(unique(mydata[,input$t_f]))[pred2]
        
        #rmse2 <- (sqrt(sum((testing[,input$t_f]-res)^2)/nrow(testing)))
        rmse2 <- RMSE(testing[,input$t_f],res)
        #rsqua2 <- (sum((res - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2))
        rsqua2 <- R2(testing[,input$t_f],res)#corr not traditional R2
        dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
        rownames(dftr2) <- 'Test set:     '
        reacVal$net_tab2 <- dftr2
      }
      plot.nn(net)
    })
    output$nnplotpm <- renderPlot({
      nnplt()
    })
    output$nnbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))#input$t_f
        return()
      tagList(
        #numericInput('nnl','Layers/Neurons',1,1,7,step = 1),
        textInput('nnlay','Neurons/Layer(\',\' separated)',value = '1'),
        numericInput('nnthresh','Threshold',0.01,0),
        numericInput('nniter','Maximum Iterations',1e+05,1,step = 1),
        numericInput('nnrep','Repetitions',1,1,step = 1),
        selectInput('nnalgo','Algo', c( 'backprop', 'rprop+', 'rprop-', 'sag', 'slr'),
                    selected = 'rprop+'),
        selectInput('nnef','Error Function', c( 'sse', 'ce'),
                    selected = 'sse'),
        selectInput('nnaf','Activation Function', c( 'logistic', 'tanh'),
                    selected = 'logistic'),
        checkboxInput('nnlo','linear output',value = T),
        numericInput('nnlr','Learning Rate(backprop)',NULL,0),
        actionButton("buttonpm", "Run Neural Network")
      )
    })
    
    setnn <- eventReactive(reacVal$nn,{
      reacVal$nn
    })
    output$nnt <- renderPrint({
      setnn()
    })
    
    setnet_tab <- eventReactive(reacVal$net_tab,{
      reacVal$net_tab
    })
    output$net_tab <- renderPrint({
      setnet_tab()
    })
    
    setnet_tab2 <- eventReactive(reacVal$net_tab2,{
      reacVal$net_tab2
    })
    output$net_tab2 <- renderPrint({
      setnet_tab2()
    })
    
    
    #svm
    svmplt <- eventReactive(input$buttonsvm, {
      ctrl <- trainControl(method =  input$svm_Res_met)
      svm <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = input$svm_md,
                         trControl = ctrl)
      svm_Mod <<- svm
      #print(svm)######to be done-different
      reacVal$svm <- svm
   #   <- paste('Number of Support Vectors: ',svm$finalModel

      ##**todo      see  ?resamples eg trcontrol-index
#       rValues <- resamples(list(svm,reacVal$neuralnet))
#       cat(rValues$values)
#       cat(summary(rValues))
      if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){
        pred <- predict(svm,mydata[!names(mydata) %in% input$t_f])
        reacVal$svm_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        pred2 <- predict(svm,testing[!names(mydata) %in% input$t_f])
        reacVal$svm_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
      }
      else
      {
        pred <- predict(svm,mydata[!names(mydata) %in% input$t_f])
        #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
        #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
        rmse <- RMSE(pred,mydata[,input$t_f])
        rsqua <- R2(pred,mydata[,input$t_f])
        dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
        rownames(dftr) <- 'Training set:    '
        reacVal$svm_tab <- dftr
        
        pred2 <- predict(svm,testing[!names(mydata) %in% input$t_f])
        #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
        #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
        rmse2 <- RMSE(pred2,testing[,input$t_f])
        rsqua2 <- R2(pred2,testing[,input$t_f])
        
        dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
        rownames(dftr2) <- 'Test set:     '
        reacVal$svm_tab2 <- dftr2
      }
      
      #plot(svm) cv
      plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
           col = as.integer(mydata[,input$t_f]),#put for factors
           pch = c("o","+")[1:dim(mydata)[1] %in% svm$finalModel@SVindex + 1])#+1?
    })
    output$svmplotpm <- renderPlot({
      svmplt()
    })
    output$svmbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('svm_md','method', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
                    selected = 'svmLinear'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('svm_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" , "none"),
                    selected = 'boot'),
        
        actionButton("buttonsvm", "Run SVM")
      )
    })
    
    setsvm <- eventReactive(reacVal$svm,{
      reacVal$svm 
    })
    output$svmt <- renderPrint({
      setsvm()
    })
    
    setsvm_tab <- eventReactive(reacVal$svm_tab,{
      reacVal$svm_tab
    })
    output$svm_tab <- renderPrint({
      setsvm_tab()
    })
    
    setsvm_tab2 <- eventReactive(reacVal$svm_tab2,{
      reacVal$svm_tab2
    })
    output$svm_tab2 <- renderPrint({
      setsvm_tab2()
    })
    
    
    #GAM
    gamplt <- eventReactive(input$buttongam, {
      ctrl <- trainControl(method =  input$gam_Res_met)
      gam <- train(mydata[,!colnames(mydata) %in% input$t_f],mydata[,input$t_f],method = 'gam',
                   trControl = ctrl)
      gam_Mod <<- gam
      #print(gam)######to be done-different
      reacVal$gam <- gam
      #   <- paste('Number of Support Vectors: ',gam$finalModel
      
      ##**todo      see  ?resamples eg trcontrol-index
      #       rValues <- resamples(list(gam,reacVal$neuralnet))
      #       cat(rValues$values)
      #       cat(summary(rValues))
      if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){
        pred <- predict(gam,mydata[!names(mydata) %in% input$t_f])
        reacVal$gam_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        pred2 <- predict(gam,testing[!names(mydata) %in% input$t_f])
        reacVal$gam_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
      }
      else
      {
        pred <- predict(gam,mydata[!names(mydata) %in% input$t_f])
        #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
        #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
        rmse <- RMSE(pred,mydata[,input$t_f])
        rsqua <- R2(pred,mydata[,input$t_f])
        dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
        rownames(dftr) <- 'Training set:    '
        reacVal$gam_tab <- dftr
        
        pred2 <- predict(gam,testing[!names(mydata) %in% input$t_f])
        #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
        #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
        rmse2 <- RMSE(pred2,testing[,input$t_f])
        rsqua2 <- R2(pred2,testing[,input$t_f])
        dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
        rownames(dftr2) <- 'Test set:     '
        reacVal$gam_tab2 <- dftr2
      }
      
      plot(gam) #cv
#       plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
#            col = as.integer(mydata[,input$t_f]),#put for factors
#            pch = c("o","+")[1:dim(mydata)[1] %in% gam$finalModel@SVindex + 1])#+1?
    })
    output$gamplotpm <- renderPlot({
      gamplt()
    })
    output$gambuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
#         selectInput('gam_md','method', c( 'gamLinear',"gamRadial","gamPoly","gamRadialCost","gamRadialSigma" , "gamRadialWeights"),
#                     selected = 'gamLinear'),
        #selectInput('gam_met','metric', c( 'ROC',"gamRadial","gamPoly","gamRadialCost","gamRadialSigma" , "gamRadialWeights"),
        #            selected = 'gamLinear'),
        #selectInput('gam_pre','Preprocess', c( 'gamLinear',"gamRadial","gamPoly","gamRadialCost","gamRadialSigma" , "gamRadialWeights"),
        #            selected = 'gamLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('gam_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" , "none"),
                    selected = 'boot'),
        
        actionButton("buttongam", "Run GAM")
      )
    })
    
    setgam <- eventReactive(reacVal$gam,{
      reacVal$gam 
    })
    output$gamt <- renderPrint({
      setgam()
    })
    
    setgam_tab <- eventReactive(reacVal$gam_tab,{
      reacVal$gam_tab
    })
    output$gam_tab <- renderPrint({
      setgam_tab()
    })
    
    setgam_tab2 <- eventReactive(reacVal$gam_tab2,{
      reacVal$gam_tab2
    })
    output$gam_tab2 <- renderPrint({
      setgam_tab2()
    })
    
    
    #lda,qda
    ldaplt <- eventReactive(input$buttonlda, {
      if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){
        ctrl <- trainControl(method =  input$lda_Res_met)
        lda <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = input$lda_md,
                     trControl = ctrl)
        lda_Mod <<- lda
        #print(lda$finalModel)
        reacVal$lda <- lda
        pred <- predict(lda,mydata[!names(mydata) %in% input$t_f])
        reacVal$lda_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        pred2 <- predict(lda,testing[!names(mydata) %in% input$t_f])
        reacVal$lda_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
        
  #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
  #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
  #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
  #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
  #                                                                                                                            row.names(df)))
        
        plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
             col = as.integer(mydata[,input$t_f]))
      }
      else
        print('Target is not a factor')
    })
    output$ldaplotpm <- renderPlot({
      ldaplt()
    })
    output$ldabuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('lda_md','method', c( 'lda',"stepQDA",'hdda'),
                    selected = 'lda'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('lda_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" , "none"),
                    selected = 'boot'),
        
        actionButton("buttonlda", "Run Discriminant Analysis")
      )
    })
    
    setlda <- eventReactive(reacVal$lda,{
      reacVal$lda
    })
    output$ldat <- renderPrint({
      setlda()
    })
    
    setlda_tab <- eventReactive(reacVal$lda_tab,{
      reacVal$lda_tab
    })
    output$lda_tab <- renderPrint({
      setlda_tab()
    })
    
    setlda_tab2 <- eventReactive(reacVal$lda_tab2,{
      reacVal$lda_tab2
    })
    output$lda_tab2 <- renderPrint({
      setlda_tab2()
    })
    
    #rf
    rfplt <- eventReactive(input$buttonrf, {
        ctrl <- trainControl(method =  input$rf_Res_met)
        rf <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = input$rf_md,
                     trControl = ctrl)
        rf_Mod <<- rf
        reacVal$rf <- rf
        if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){
          
          pred <- predict(rf,mydata[!names(mydata) %in% input$t_f])
          reacVal$rf_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
          pred2 <- predict(rf,testing[!names(mydata) %in% input$t_f])
          reacVal$rf_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
        }
        else
        {
          pred <- predict(rf,mydata[!names(mydata) %in% input$t_f])
          #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
          #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
          rmse <- RMSE(pred,mydata[,input$t_f])
          rsqua <- R2(pred,mydata[,input$t_f])
          dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
          rownames(dftr) <- 'Training set:    '
          reacVal$rf_tab <- dftr
          
          pred2 <- predict(rf,testing[!names(mydata) %in% input$t_f])
          #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
          #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
          rmse2 <- RMSE(pred2,testing[,input$t_f])
          rsqua2 <- R2(pred2,testing[,input$t_f])
          dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
          rownames(dftr2) <- 'Test set:     '
          reacVal$rf_tab2 <- dftr2
        }

        #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
        #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
        #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
        #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
        #                                                                                                                            row.names(df)))
        if(input$rf_md=='rf')
          varImpPlot(rf$finalModel)
        else if(input$rf_md=='rpart')
          fancyRpartPlot(rf$finalModel)
        
#         plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
#              col = as.integer(mydata[,input$t_f]))
    
    })
    output$rfplotpm <- renderPlot({#use render ui
      rfplt()
    })
    output$rfbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('rf_md','method', c( 'rf',"rpart"),
                    selected = 'rf'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('rf_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" ),
                    selected = 'boot'),
        
        actionButton("buttonrf", "Run Random Forest")
      )
    })
    
    setrf <- eventReactive(reacVal$rf,{
      reacVal$rf
    })
    output$rft <- renderPrint({
      setrf()
    })
    
    setrf_tab <- eventReactive(reacVal$rf_tab,{
      reacVal$rf_tab
    })
    output$rf_tab <- renderPrint({
      setrf_tab()
    })
    
    setrf_tab2 <- eventReactive(reacVal$rf_tab2,{
      reacVal$rf_tab2
    })
    output$rf_tab2 <- renderPrint({
      setrf_tab2()
    })
    
    
    #boost
    boostplt <- eventReactive(input$buttonboost, {
      if(input$boost_md=='ada' ) 
      {
        if(class(mydata[,input$t_f])!='factor'&class(mydata[,input$t_f])!='character'){
        print('Target not a factor')
        return()}
        if(length(unique(mydata[,input$t_f]))>2)
        {print('Target does not have binary Response')
          return()}
      }
      ctrl <- trainControl(method =  input$boost_Res_met)
      boost <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = input$boost_md,
                  trControl = ctrl)
      boost_Mod <<- boost
      reacVal$boost <- boost
      if(class(mydata[,input$t_f])=='factor'|class(mydata[,input$t_f])=='character'){
        
        pred <- predict(boost,mydata[!names(mydata) %in% input$t_f])
        reacVal$boost_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        pred2 <- predict(boost,testing[!names(mydata) %in% input$t_f])
        reacVal$boost_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
      }
      else
      {
        pred <- predict(boost,mydata[!names(mydata) %in% input$t_f])
        #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
        #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
        rmse <- RMSE(pred,mydata[,input$t_f])
        rsqua <- R2(pred,mydata[,input$t_f])
        dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
        rownames(dftr) <- 'Training set:    '
        reacVal$boost_tab <- dftr
        #print(R2(pred,mydata[,input$t_f],formula='traditional'))
        pred2 <- predict(boost,testing[!names(mydata) %in% input$t_f])
        #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
        #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
        rmse2 <- RMSE(pred2,testing[,input$t_f])
        rsqua2 <- R2(pred2,testing[,input$t_f])
        dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
        rownames(dftr2) <- 'Test set:     '
        #print(R2(pred2,testing[,input$t_f],formula='traditional'))
        
        reacVal$boost_tab2 <- dftr2
      }
      
      #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
      #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
      #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
      #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
      #                                                                                                                            row.names(df)))
      if(input$boost_md!='xgbTree')
        plot(boost$finalModel)
      
      #         plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
      #              col = as.integer(mydata[,input$t_f]))
      
    })
    output$boostplotpm <- renderPlot({#use render ui
      boostplt()
    })
    output$boostbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('boost_md','method', c( 'gbm','gamboost',"ada",'xgbTree'),
                    selected = 'rf'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('boost_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV",'none' ),
                    selected = 'boot'),
        
        actionButton("buttonboost", "Run Boosting")
      )
    })
    
    setboost <- eventReactive(reacVal$boost,{
      reacVal$boost
    })
    output$boostt <- renderPrint({
      setboost()
    })
    
    setboost_tab <- eventReactive(reacVal$boost_tab,{
      reacVal$boost_tab
    })
    output$boost_tab <- renderPrint({
      setboost_tab()
    })
    
    setboost_tab2 <- eventReactive(reacVal$boost_tab2,{
      reacVal$boost_tab2
    })
    output$boost_tab2 <- renderPrint({
      setboost_tab2()
    })
    
    #Polynomial Regression
    regplt <- eventReactive(input$buttonreg, {
      if(class(mydata[,input$t_f])=='factor' | class(mydata[,input$t_f])=='character'){
        if(length(unique(mydata[,input$t_f]))>2){
          print('Target should be binary class')
          return()
        }
      }
        
        ctrl <- trainControl(method =  input$reg_Res_met)
        deg=switch(input$reg_md,'Linear'=1,'Quadratic'=2,'Cubic'=3,'Quartic'=4,'Logistic'=-1)
        
        fam=gaussian
        if(deg==-1){
          fam=binomial
          n <- mydata[,!names(mydata) %in% input$t_f]
        }
        else
        {#add degrees
        m <- mydata[,!names(mydata) %in% input$t_f]
        idx <- sapply(m, class)%in%c('numeric','integer')
        n <- as.data.frame(lapply(1:deg,function(i){m[,idx]^i}))
        n <- cbind(m[!colnames(m)%in%colnames(n)],n)
        }
        
        reg <- train(n,mydata[,input$t_f],method = 'glm',family=fam,
                     trControl = ctrl)
        reg_Mod <<- reg
        print(summary(reg))######to be done-different
        reacVal$reg <- summary(reg)
        
        if(deg==-1){#** code not tested
          pred <- predict(reg,mydata[!names(mydata) %in% input$t_f])
          reacVal$reg_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
          pred2 <- predict(reg,testing[!names(mydata) %in% input$t_f])
          reacVal$reg_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
        }
        else
        {
          pred <- predict(reg,n)
          #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
          #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
          rmse <- RMSE(pred,mydata[,input$t_f])
          rsqua <- R2(pred,mydata[,input$t_f])
          dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
          rownames(dftr) <- 'Training set:    '
          reacVal$reg_tab <- dftr
          
          m <- testing[,!names(testing) %in% input$t_f]
          idx <- sapply(m, class)%in%c('numeric','integer')
          n <- as.data.frame(lapply(1:deg,function(i){m[,idx]^i}))
          n <- cbind(m[!colnames(m)%in%colnames(n)],n)
          
          pred2 <- predict(reg,n)
          #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
          #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
          rmse2 <- RMSE(pred2,testing[,input$t_f])
          rsqua2 <- R2(pred2,testing[,input$t_f])
          dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
          rownames(dftr2) <- 'Test set:     '
          reacVal$reg_tab2 <- dftr2
        }
        
        #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
        #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
        #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
        #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
        #                                                                                                                            row.names(df)))
        par(mfrow=c(2,2))
        plot(reg$finalModel)
#         plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
#              col = as.integer(mydata[,input$t_f]))
#       }
#       else
#         print('Target not numeric')
      
    })
    output$regplotpm <- renderPlot({
      regplt()
    })
    output$regbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('reg_md','Type', c( 'Linear',"Quadratic",'Cubic','Quartic','Logistic'),
                    selected = 'Linear'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('reg_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV" , "none"),
                    selected = 'boot'),
        
        actionButton("buttonreg", "Run Multivariate Regression")
      )
    })
    
    setreg <- eventReactive(reacVal$reg,{
      reacVal$reg
    })
    output$regt <- renderPrint({
      setreg()
    })
  
    setreg_tab <- eventReactive(reacVal$reg_tab,{
      reacVal$reg_tab
    })
    output$reg_tab <- renderPrint({
      setreg_tab()
    })
    
    setreg_tab2 <- eventReactive(reacVal$reg_tab2,{
      reacVal$reg_tab2
    })
    output$reg_tab2 <- renderPrint({
      setreg_tab2()
    })
    
    #Naive Bayes
    nbplt <- eventReactive(input$buttonnb, {
        if(class(mydata[,input$t_f])!='factor' & class(mydata[,input$t_f])!='character'){
          print('Target not a factor')
          return()
        }
      ctrl <- trainControl(method =  input$nb_Res_met)
      nb <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = 'nb',
                     trControl = ctrl)
      nb_Mod <<- nb
      reacVal$nb <- nb
        pred <- predict(nb,mydata[!names(mydata) %in% input$t_f])
        reacVal$nb_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
        pred2 <- predict(nb,testing[!names(mydata) %in% input$t_f])
        reacVal$nb_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))

      #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
      #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
      #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
      #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
      #                                                                                                                            row.names(df)))
      #***  par(mfrow=c(2,2)) how many vars
      #***  plot(nb$finalModel)
      
      #         plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
      #              col = as.integer(mydata[,input$t_f]))
      
    })
    output$nbplotpm <- renderPlot({#use render ui
      nbplt()
    })
    output$nbbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
#         selectInput('nb_md','method', c( 'gbm','gamnb',"ada",'xgbTree'),
#                     selected = 'rf'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('nn_tun','Tune Length',0.01,0),
        selectInput('nb_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV",'none' ),
                    selected = 'boot'),
        
        actionButton("buttonnb", "Run Naive Bayes")
      )
    })
    
    setnb <- eventReactive(reacVal$nb,{
      reacVal$nb
    })
    output$nbt <- renderPrint({
      setnb()
    })
    
    setnb_tab <- eventReactive(reacVal$nb_tab,{
      reacVal$nb_tab
    })
    output$nb_tab <- renderPrint({
      setnb_tab()
    })
    
    setnb_tab2 <- eventReactive(reacVal$nb_tab2,{
      reacVal$nb_tab2
    })
    output$nb_tab2 <- renderPrint({
      setnb_tab2()
    })
    
    #knn
    knnplt <- eventReactive(input$buttonknn, {#should be obsrve event
      #if(class(mydata[,input$t_f])!='factor' & class(mydata[,input$t_f])!='character'){
      #  print('Target not a factor')
      #  return()
      #}
      ctrl <- trainControl(method =  input$knn_Res_met)
      knn <- train(mydata[,!names(mydata) %in% input$t_f],mydata[,input$t_f],method = 'knn',
                  trControl = ctrl)
      knn_Mod <<- knn
      reacVal$knn <- knn
      if(class(mydata[,input$t_f])=='factor' | class(mydata[,input$t_f])=='character'){
      pred <- predict(knn,mydata[!names(mydata) %in% input$t_f])
      reacVal$knn_tab <- confusionMatrix(table(mydata[,input$t_f],pred))
      
      pred2 <- predict(knn,testing[!names(mydata) %in% input$t_f])
      reacVal$knn_tab2 <- confusionMatrix(table(testing[,input$t_f],pred2))
      }
      else{
        pred <- predict(knn,mydata[!names(mydata) %in% input$t_f])
        #rmse <- sqrt(sum((mydata[,input$t_f]-pred)^2)/nrow(mydata))
        #rsqua <- sum((pred - mean(mydata[,input$t_f]))^2)/sum((mydata[,input$t_f] - mean(mydata[,input$t_f]))^2)
        rmse <- RMSE(pred,mydata[,input$t_f])
        rsqua <- R2(pred,mydata[,input$t_f])
        dftr <- data.frame(RMSE=rmse,Rsqaured=rsqua)
        rownames(dftr) <- 'Training set:    '
        reacVal$knn_tab <- dftr
        #print(R2(pred,mydata[,input$t_f],formula='traditional'))
        pred2 <- predict(knn,testing[!names(mydata) %in% input$t_f])
        #rmse2 <- sqrt(sum((testing[,input$t_f]-pred2)^2)/nrow(testing))
        #rsqua2 <- sum((pred2 - mean(testing[,input$t_f]))^2)/sum((testing[,input$t_f] - mean(testing[,input$t_f]))^2)
        rmse2 <- RMSE(pred2,testing[,input$t_f])
        rsqua2 <- R2(pred2,testing[,input$t_f])
        dftr2 <- data.frame(RMSE=rmse2,Rsqaured=rsqua2)
        rownames(dftr2) <- 'Test set:     '
        #print(R2(pred2,testing[,input$t_f],formula='traditional'))
        
        reacVal$knn_tab2 <- dftr2
      }
      #       fit <- cmdscale(dist.mat, eig = TRUE, k = 2)
      #       points <- data.frame(x = fit$points[, 1], y = fit$points[, 2])
      #       ggplot(points, aes(x = x, y = y)) + geom_point(data = points, aes(x = x, y = y, 
      #                                                                         color = df$view)) + geom_text(data = points, aes(x = x, y = y - 0.2, label = 
      #                                                                                                                            row.names(df)))
      #plot(knn$finalModel)
      
      #         plot(cmdscale(dist(mydata[,!names(mydata) %in% input$t_f])),
      #              col = as.integer(mydata[,input$t_f]))
      
    })
    output$knnplotpm <- renderPlot({#use render ui
      knnplt()
    })
    output$knnbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        #         selectInput('knn_md','method', c( 'gbm','gamknn',"ada",'xgbTree'),
        #                     selected = 'rf'),
        #selectInput('svm_met','metric', c( 'ROC',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #selectInput('svm_pre','Preprocess', c( 'svmLinear',"svmRadial","svmPoly","svmRadialCost","svmRadialSigma" , "svmRadialWeights"),
        #            selected = 'svmLinear'),
        #numericInput('knn_tun','neighbours',1,0,step = 1),
        selectInput('knn_Res_met','Resampling method', c( "boot", "boot632", "cv", "repeatedcv", "LOOCV", "LGOCV",'none' ),
                    selected = 'boot'),
        
        actionButton("buttonknn", "Run k-Nearest Neighbours")
      )
    })
    
    setknn <- eventReactive(reacVal$knn,{
      reacVal$knn
    })
    output$knnt <- renderPrint({
      setknn()
    })
    
    setknn_tab <- eventReactive(reacVal$knn_tab,{
      reacVal$knn_tab
    })
    output$knn_tab <- renderPrint({
      setknn_tab()
    })
    
    setknn_tab2 <- eventReactive(reacVal$knn_tab2,{
      reacVal$knn_tab2
    })
    output$knn_tab2 <- renderPrint({
      setknn_tab2()
    })
    
    
    #Resamples
    Resplt <- eventReactive(input$buttonRes, {#should be obsrve event
      sm <- list()#for different targets not work, 
      i <- 1
#       if(exists('neuralnet_Mod')){
#         sm[[i]] <- neuralnet_Mod
#         names(sm)[i] <- c('Neural Network')
#         i <- i+1
#       }
      if(exists('svm_Mod')){
        sm[[i]] <- svm_Mod
        names(sm)[i] <- c('SVM')
        i <- i+1
      }
      if(exists('gam_Mod')){
        sm[[i]] <- gam_Mod
        names(sm)[i] <- c('GAM')
        i <- i+1
      }
      if(exists('lda_Mod')){
        sm[[i]] <- lda_Mod
        names(sm)[i] <- c('LDA')
        i <- i+1
      }
      if(exists('rf_Mod')){
        sm[[i]] <- rf_Mod
        names(sm)[i] <- c('Random Forest')
        i <- i+1
      }
      if(exists('boost_Mod')){
        sm[[i]] <- boost_Mod
        names(sm)[i] <- c('Boosting')
        i <- i+1
      }
      if(exists('reg_Mod')){
        sm[[i]] <- reg_Mod
        names(sm)[i] <- c('Regression')
        i <- i+1
      }
      if(exists('nb_Mod')){
        sm[[i]] <- nb_Mod
        names(sm)[i] <- c('Naive Bayes')
        i <- i+1
      }
      if(exists('knn_Mod')){
        sm[[i]] <- knn_Mod
        names(sm)[i] <- c('KNN')
        i <- i+1
      }
      t <- resamples(sm)
      reacVal$Res <- summary(t)
      bwplot(t)
    })
    
    output$Resplotpm <- renderPlot({#use render ui
      Resplt()
    })
    output$Resbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        actionButton("buttonRes", "Summarise")
      )
    })
    
    setRes <- eventReactive(reacVal$Res,{
      reacVal$Res
    })
    output$Rest <- renderPrint({
      setRes()
      #HTML(paste(setRes(), collapse = '<br/>'))
    })
    
#     setRes_tab <- eventReactive(reacVal$Res_tab,{
#       reacVal$Res_tab
#     })
#     output$Res_tab <- renderTable({
#       setRes_tab()
#     },digits=4)

    
    observeEvent(input$buttonTim,{
      timd <- NULL#data.frame()
      i <- 1
      #       if(exists('neuralnet_Mod')){
      #         sm[[i]] <- neuralnet_Mod
      #         names(sm)[i] <- c('Neural Network')
      #         i <- i+1
      #       }
      if(exists('svm_Mod')){
        timd <- rbind(timd,svm_Mod$times[[1]])
        rownames(timd)[i] <- c('SVM')
        i <- i+1
      }
      if(exists('gam_Mod')){
        timd <- rbind(timd,gam_Mod$times[[1]])
        rownames(timd)[i] <- c('GAM')
        i <- i+1
      }
      if(exists('lda_Mod')){
        timd <- rbind(timd,lda_Mod$times[[1]])
        rownames(timd)[i] <- c('LDA')
        i <- i+1
      }
      if(exists('rf_Mod')){
        timd <- rbind(timd,rf_Mod$times[[1]])
        rownames(timd)[i] <- c('Random Forest')
        i <- i+1
      }
      if(exists('boost_Mod')){
        timd <- rbind(timd,boost_Mod$times[[1]])
        rownames(timd)[i] <- c('Boosting')
        i <- i+1
      }
      if(exists('reg_Mod')){
        timd <- rbind(timd,reg_Mod$times[[1]])
        rownames(timd)[i] <- c('Regression')
        i <- i+1
      }
      if(exists('nb_Mod')){
        timd <- rbind(timd,nb_Mod$times[[1]])
        rownames(timd)[i] <- c('Naive Bayes')
        i <- i+1
      }
      if(exists('knn_Mod')){
        timd <- rbind(timd,knn_Mod$times[[1]])
        rownames(timd)[i] <- c('KNN')
        i <- i+1
      }
      reacVal$Tim <- timd[,1:3]
    })
    
    output$Timbuttonpm <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        actionButton("buttonTim", "Show Times")
      )
    })
    
    setTim <- eventReactive(reacVal$Tim,{
      reacVal$Tim
    })
    output$Timt <- renderPrint({
      setTim()
    })
    
    #     val <- reactiveValues()
    #     
    #     obsf <- observe({   
    #       if (is.null(input$file))
    #return()
    #       val$inputi <- input$file$datapath
    #     })
    #     
    #     obsi <- observe({   
#       #data(list=input$dataname)
#       val$inputi<-get(input$dataname)
#     })

    output$dselectp <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
      selectInput('xaxisp','X axis',colnames(reacVal$mydata)),
      selectInput('yaxisp','Y axis',colnames(reacVal$mydata)),
      actionButton("buttonp", "Plot")
      )
    })
    plt <- eventReactive(input$buttonp, {
      #**********
      #depend reactively on xaxisp,yaxisp only first dependency on button
      # or use isolate to avoid dependency on x/yaxisp
#       if(input$buttonp==0)
#         return()
      #g <- isolate({#****not isolate
        g <- ggplot(data = mydata, aes(x=get(input$xaxisp),y=get(input$yaxisp)))
        g = g + geom_point() + xlab(input$yaxisp) + ylab(input$xaxisp)
        
      #})
      g
    })
    output$bplot <- renderPlot({
      plt()
    })
    
    output$dselectp3d <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('xaxisp3d','X axis',colnames(reacVal$mydata)),
        selectInput('yaxisp3d','Y axis',colnames(reacVal$mydata)),
        selectInput('zaxisp3d','Z axis',colnames(reacVal$mydata)),
        actionButton("buttonp3d", "Plot")
      )
    })
    plt3d <- eventReactive(input$buttonp3d, {
      d_d <- as.matrix(mydata[,c(input$xaxisp3d,input$yaxisp3d,input$zaxisp3d)])
      scatterplot3js(d_d)
    })
    output$bplot3d <- renderScatterplotThree({
      plt3d()  
    })

    eplt <- eventReactive(input$buttonep, {
      wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
      for (i in 2:min(c(nrow(mydata),30))) 
        wss[i] <- kmeans(mydata,centers=i)$tot.withinss
      plot(1:min(c(nrow(mydata),30)), wss, type="b", xlab="Number of Clusters",
           ylab="Within groups sum of squares")
    })
    output$eplotck <- renderPlot({
      eplt()
    })
    output$epbuttonc <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      actionButton("buttonep", "Plot Error Elbow")
    })
    
    pamplt <- eventReactive(input$buttonpp, {
      pamk.best <- pamk(mydata)
      #terminal
      cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
      par(mfrow=c(2,1))
      plot(pam(mydata, pamk.best$nc))
    })
    output$pplotck <- renderPlot({
      pamplt()
    })
    output$ppbuttonc <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      actionButton("buttonpp", "Estimate by PAM")
    })
    
    calplt <- eventReactive(input$buttoncalp, {
      fit <- cascadeKM(scale(mydata, center = TRUE,  scale = TRUE), 1, min(c(nrow(mydata),30)), iter = 100)
      plot(fit, sortg = TRUE, grpmts.plot = TRUE)
      calinski.best <- as.numeric(which.max(fit$results[2,]))
      cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
    })
    output$cplotck <- renderPlot({
      calplt()
    })
    output$calpbuttonc <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      actionButton("buttoncalp", "Run Calinski criterion")
    })
    
#     bicplt <- eventReactive(input$buttonbicp, {
#       # See http://www.jstatsoft.org/v18/i06/paper
#       # http://www.stat.washington.edu/research/reports/2006/tr504.pdf
#       d_clust <- Mclust(as.matrix(mydata), G=1:min(c(nrow(mydata),30)))#G=1:20
#       m.best <- dim(d_clust$z)[2]
#       cat("model-based optimal number of clusters:", m.best, "\n")
#       plot(d_clust,what=c('BIC'))
#     })
    bicplt1 <- eventReactive(reacVal$bic_clust,{
      plot(reacVal$bic_clust,what=c('BIC'))
    })
    bicplt2 <- eventReactive(reacVal$bic_clust,{
      plot(reacVal$bic_clust,what=c('classification'))
    })
    observeEvent(input$buttonbicp,{
      d_clust <- Mclust(as.matrix(mydata), G=1:min(c(nrow(mydata),30)))#G=1:20
      m.best <- dim(d_clust$z)[2]
      cat("model-based optimal number of clusters:", m.best, "\n")
      reacVal$bic_clust <- d_clust
    })
    output$bicplotck1 <- renderPlot({
      bicplt1()
    })
    output$bicplotck2 <- renderPlot({
      bicplt2()
    })
    output$bicpbuttonc <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      actionButton("buttonbicp", "Run BIC")
    })
    
        observeEvent(input$buttonnbcp, {#para
          reacVal$msg <- capture.output(reacVal$nb <- NbClust(mydata, diss=NULL, distance = "euclidean", 
                        min.nc=2, max.nc=min(c(nrow(mydata),30)), method = "kmeans", 
                        index = "all", alphaBeale = 0.1))#index=alllong
          cat(reacVal$msg)
        })
        nbcplt <- eventReactive(reacVal$nb,{
          if(is.null(reacVal$nb))
            return()
          hist(reacVal$nb$Best.nc[1,], breaks = max(na.omit(reacVal$nb$Best.nc[1,])))
        })
        nbctext <- eventReactive(reacVal$msg,{
          if(is.null(reacVal$msg))
            return()
          reacVal$msg
        })
        output$nbcplotck <- renderPlot({
          nbcplt()
        })
        output$nbctextck <- renderText({
          nbctext()
        })
        output$nbcpbuttonc <- renderUI({
          if(is.null(reacVal$mydata))
            return()
          actionButton("buttonnbcp", "Run NbClust")
        })
#     nbcplt <- eventReactive(input$buttonnbcp, {#para
#       msg <- capture.output(nb <- NbClust(mydata, diss=NULL, distance = "euclidean", 
#                     min.nc=2, max.nc=min(c(nrow(mydata),30)), method = "kmeans", 
#                     index = "all", alphaBeale = 0.1))#index=alllong
#       cat(msg)
#       hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
#     })
#     output$nbcplotck <- renderPlot({
#       nbcplt()
#     })
#     output$nbcpbuttonc <- renderUI({
#       if(is.null(reacVal$mydata))
#         return()
#       actionButton("buttonnbcp", "Run NbClust")
#     })
        
        hplt <- eventReactive(input$buttonhp, {
          ifelse(input$squa,s<-2,s<-1)
          md_dist <- dist(as.matrix(mydata))^s
          plot(hclust(md_dist,method = input$hcm))#cutree
        })
        output$hplotck <- renderPlot({
          hplt()
        })
        output$hpbuttonc <- renderUI({
          if(is.null(reacVal$mydata))
            return()
          tagList(
            selectInput('hcm','Linkage Method', c("ward.D", 
                        "ward.D2", "single", "complete", "average", 
                        "mcquitty", "median","centroid"),
                        selected = 'complete'),
            checkboxInput('squa','Squared distance'),
            actionButton("buttonhp", "Cluster Dendogram")
          )
        })
    
    output$dselectc <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      tagList(
        selectInput('xaxisc','X axis',colnames(reacVal$mydata)),
        selectInput('yaxisc','Y axis',colnames(reacVal$mydata)),
        numericInput('k','Number of clusters:',3),
        numericInput('itm','Maximum Iterations:',10),
        numericInput('nst','Number of random starts:',1),
        selectInput('kalgo','Algo-Var',c("Hartigan-Wong", "Lloyd", "Forgy",
                                         "MacQueen")),
        actionButton("buttonc", "Go!")
      )
    })
    pltck <- eventReactive(input$buttonc, {
      if(is.numeric(mydata[input$xaxisc][1,1]) && is.numeric(mydata[input$yaxisc][1,1])){
        df <- data.frame(mydata[input$xaxisc],mydata[input$yaxisc])
        kmo <- kmeans(df,centers = input$k,iter.max = input$itm,
                      nstart = input$nst,algorithm = input$kalgo,trace = T)
        plot(df[,1],df[,2],col=kmo$cluster,pch=19,cex=2)
        points(kmo$centers,col=1:input$k,pch=input$k,cex=3,lwd=input$k)
      }
      else
      { 
        cat('Please select Numeric Values,type is:')
        cat(class(mydata[input$xaxisc][1,1]))
        plot(1,1,type='n',main = 'Please select Numeric Values')
        return()
      }
    })
    output$bplotck <- renderPlot({
      pltck()
    })
    
    output$dselectc3d2 <- renderUI({
      if(is.null(reacVal$mydata))
        return()
      vec <- colnames(reacVal$mydata)
      tagList(
        selectInput('xaxisc3d2','X axis',vec[1]),
        selectInput('yaxisc3d2','Y axis',vec,selected = vec[2]),
        selectInput('zaxisc3d2','Z axis',vec,selected = vec[3]),
        numericInput('k2','Number of clusters:',3),
        numericInput('itm2','Maximum Iterations:',10),
        numericInput('nst2','Number of random starts:',1),
        selectInput('kalgo2','Algo-Var',c("Hartigan-Wong", "Lloyd", "Forgy",
                                         "MacQueen")),
        actionButton("buttonc3d2", "Go!")
      )
    })
    pltck2 <- eventReactive(input$buttonc3d2, {
      if(is.numeric(mydata[input$xaxisc3d2][1,1]) && is.numeric(mydata[input$yaxisc3d2][1,1]) && is.numeric(mydata[input$zaxisc3d2][1,1])){
        df <- data.frame(mydata[input$xaxisc3d2],mydata[input$yaxisc3d2],mydata[input$zaxisc3d2])
        kmo <- kmeans(df,centers = input$k2,iter.max = input$itm2,
                      nstart = input$nst2,algorithm = input$kalgo2,trace = T)
        d_d2 <- as.matrix(df[,1:3])
        clr <- rainbow(length(unique(kmo$cluster)))
        nclr <- clr[kmo$cluster]
        scatterplot3js(d_d2,color = nclr)
        #plot(df[,1],df[,2],col=kmo$cluster,pch=19,cex=2)
        #points(kmo$centers,col=1:input$k,pch=input$k,cex=3,lwd=input$k)
      }
      else
      { 
        cat('Please select Numeric Values,type is:')
        cat(class(mydata[input$xaxisc][1,1]))
        plot(1,1,type='n',main = 'Please select Numeric Values')#*edit
        return()
      }
    })
    output$bplotck2 <- renderScatterplotThree({
      pltck2()
    })
    
    
#     
#     
#     
#     output$origimg <- renderPlot({
#       if(is.null(input$dataname) || is.null(input$xaxis)|| is.null(input$yaxis))
#         return()
#       g <- ggplot(data = get(input$dataname), aes(x=get(input$xaxis),y=get(input$yaxis)))
#       g = g + geom_point() + xlab(input$yaxis) + ylab(input$xaxis)
#       g
#     })
#     
#     output$dynamicselectk1 <- renderUI({
#       #if(is.null(input$dataname))
#       #  return()
#       selectInput('xaxisk','X axis',colnames(get(input$dataname)))
#     })
#     
#     output$dynamicselectk2 <- renderUI({
#       selectInput('yaxisk','Y axis',colnames(get(input$dataname)))
#     })
#     
#     output$clusk <- renderPlot({
#       if(is.null(input$dataname) || is.null(input$xaxisk)|| is.null(input$yaxisk))
#         return()
#       df <- data.frame(get(input$dataname)[((input$xaxisk))],get(input$dataname)[((input$yaxisk))])
#       #if(is.numeric(input$xaxisk) || is.numeric(input$yaxisk)){
#       kmo <- kmeans(df,centers = input$k)#max.iter,nstart
#       #par(mar=rep(0.2,4))
#       plot(df[,1],df[,2],col=kmo$cluster,pch=19,cex=2)
#       points(kmo$centers,col=1:input$k,pch=input$k,cex=input$k,lwd=input$k)
#       #}
#       #else
#       #  return()
#     })
    
    output$help <- renderText('This App is used as a data analyser.
                              It shows relations among different features or columns of data.....')
    session$onSessionEnded(function() {
      stopCluster(cl)
      registerDoSEQ()
      cat('Stopping Clusters..')
    })
    }
)
#     output$var<- renderText({
#       if(!is.null(input$dyn))
#         c('R:',as.character(sum(svd1.r$d[1:input$dyn]^2)*100/sum(svd1.r$d^2)),
#           '\nG:',as.character(sum(svd1.g$d[1:input$dyn]^2)*100/sum(svd1.g$d^2)),
#           '\nB:',as.character(sum(svd1.b$d[1:input$dyn]^2)*100/sum(svd1.b$d^2)))
#     })
#     
#     y <- reactive({
#       if(is.null(input$dyn))
#         return(NULL)
#       outfile <<- tempfile(fileext='.jpg')
#       if(input$dyn !=1){
#         approx.r <- descale(svd1.r$u[,1:input$dyn] %*% diag(svd1.r$d[1:input$dyn]) %*% t(svd1.r$v[,1:input$dyn]),attributes(z.r))
#         approx.g <- descale(svd1.g$u[,1:input$dyn] %*% diag(svd1.g$d[1:input$dyn]) %*% t(svd1.g$v[,1:input$dyn]),attributes(z.g))
#         approx.b <- descale(svd1.b$u[,1:input$dyn] %*% diag(svd1.b$d[1:input$dyn]) %*% t(svd1.b$v[,1:input$dyn]),attributes(z.b))
#}
#       else{
#         approx.r <- descale(svd1.r$u[,1:input$dyn] %*% as.matrix(svd1.r$d[1:input$dyn]) %*% t(svd1.r$v[,1:input$dyn]),attributes(z.r))
#         approx.g <- descale(svd1.g$u[,1:input$dyn] %*% as.matrix(svd1.g$d[1:input$dyn]) %*% t(svd1.g$v[,1:input$dyn]),attributes(z.g))
#         approx.b <- descale(svd1.b$u[,1:input$dyn] %*% as.matrix(svd1.b$d[1:input$dyn]) %*% t(svd1.b$v[,1:input$dyn]),attributes(z.b))
#}
#       parr <<- array(c(approx.r,approx.g,approx.b),dim=dim(myO))
#       writeJPEG(parr,outfile,1)
#       sz <<- file.info(outfile)$size
#     })
#     ###########
#     output$finimg <- renderUI({      
#       if(!is.null(y())){
#         output$xyz <- renderImage({
#           list(src = outfile,
#                contentType = 'image/png',
#                width = 550,
#                height = 350,
#                alt = "Processing...")
#},deleteFile = T)
#         imageOutput('xyz')
#}
#     })
#     
#     output$size <- renderText({
#       if(!is.null(y()))  ########
#       paste(round((sz/1024)*100)/100,'KB')
#     })
#     
#     output$sizem <- renderText({
#       if(is.null(input$dyn))
#         return(NULL)
#       format(3*(object.size(svd1.r$u[,1:input$dyn]) +
#                   object.size(svd1.r$d[1:input$dyn])+
#                   object.size(svd1.r$v[,1:input$dyn])),units="KB")
#     })
#     
#     output$downloadData <- downloadHandler(
#       # This function returns a string which tells the client
#       # browser what name to use when saving the file.
#       filename = function() {
#         input$imagename
#},
#       content = function(file) {
#         writeJPEG(parr,file,1)
#}
#     )
#     
#     output$imgr <- renderImage({
#       x()
#       outfile <- tempfile(fileext='.png')
#       png(outfile)
#       par(mar=c(5,4.1,1,1.1))
#       plot(svd1.r$d^2/sum(svd1.r$d^2),xlab="RED",ylab="Variance",type= 'l')
#       dev.off()
#       list(src = outfile,alt = "This is alternate text")
#     },deleteFile = T)
#     output$imgg <- renderImage({
#       x()
#       outfile <- tempfile(fileext='.png')
#       png(outfile)
#       par(mar=c(5,4.1,1,1.1))
#       plot(svd1.g$d^2/sum(svd1.g$d^2),xlab="GREEN",ylab="Variance",type= 'l')
#       dev.off()
#       list(src = outfile,alt = "This is alternate text")
#     },deleteFile = T)
#     output$imgb <- renderImage({
#       x()
#       outfile <- tempfile(fileext='.png')
#       png(outfile)
#       par(mar=c(5,4.1,1,1.1))
#       plot(svd1.b$d^2/sum(svd1.b$d^2),xlab="BLUE",ylab="Variance",type= 'l')
#       dev.off()
#       list(src = outfile,alt = "This is alternate text")
#     },deleteFile = T)
#     
#     # When the client ends the session, suspend the observer.
#     # Otherwise, the observer could keep running after the client
#     # ends the session.
#     session$onSessionEnded(function() {
#       obsf$suspend()
#       obsi$suspend()
#       # Clean up the file
#       unlink(outfile)
#     })
#     




