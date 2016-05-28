library(threejs)
library(doParallel)
cl <- makeCluster(detectCores()-1)#leave one
registerDoParallel(cl)
datalist <- data(package = .packages(all.available = TRUE))
