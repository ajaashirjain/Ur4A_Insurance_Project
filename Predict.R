predict1 = function(Test) {
  set.seed(999)
  library(caret)
  library(h2o)
  
  # tr <- read.csv('train.csv')
  # Intrain <- createDataPartition(y = tr$Response,   # outcome variable
  #                                p = .30,   # % of training data you want
  #                                list = F)
  # # create your partitions
  # tr <- tr[Intrain,]  # training data set
  tr <- read.csv('Final_train.csv')
  te <- Test
  tr$id <- NULL
  tr$V
  tr[,1]<- NULL
  te$id <- NULL
  # str(tr)
  te$Age <- as.numeric(te$Age)
  te$Annual_Premium <- as.numeric(te$Annual_Premium)
  te$Policy_Sales_Channel <- as.numeric((te$Policy_Sales_Channel))
  te$Vintage <- as.numeric((te$Vintage))
  
  s <- c('Gender','Vehicle_Age',"Vehicle_Damage","Driving_License",
         "Previously_Insured","Region_Code")
  
  for (i in 1:length(s)){
    tr[,s[i]] <- as.factor(tr[,s[i]])
    te[,s[i]] <- as.factor(te[,s[i]])
  }
  str(te)
  ## Create Dummies
  #tr set
  library(caret)
  library(dplyr)
  dummies <- dummyVars(Response ~ ., data = tr)     # create dummies for Xs
  ex <- data.frame(predict(dummies, newdata = tr))  # actually creates the dummies
  names(ex) <- gsub("\\.", "", names(ex))          # removes dots from col names
  d <- cbind(tr$Response, ex)                      # combine target var with Xs
  d<- d %>%rename(Response = 'tr$Response')             # name target var 'Response'
  rm(dummies, ex)                                  # clean environment
  
  d$Response <- as.factor(d$Response)
  
  dummies <- dummyVars(~ ., data = te)     # create dummies for Xs
  te <- data.frame(predict(dummies, newdata = te))  # actually creates the dummies
  names(te) <- gsub("\\.", "", names(te))          # removes dots from col names
  rm(dummies)                                  # clean environment
  
  
  ###############################################################################
  library(h2o)
  h2o.init()
  #nthreads=12, max_mem_size="64g"
  # load data into h2o cluster
  data <- as.h2o(d)
  te<- as.h2o(te)
  y <- "Response"                                # target variable to learn
  x <- setdiff(names(d), y)                # features are all other columns
  parts <- h2o.splitFrame(data, 0.8, seed=99) # randomly partition data into 80/20
  train <- parts[[1]]                         # random set of training obs
  valid <- parts[[2]]                          # random set of testing obs
  
  
  auto <- h2o.gbm(x, y, train)
  # auto
  # str(auto)
  
  # make predictions
  p <- h2o.predict(auto, te)
  p <- as.data.frame(p)
  # pl <- h2o.varimp_plot(auto, num_of_features = 11)
  # pl <- h2o.varimp(model, num_of_features = 11)
  # out <- list(p,pl)
  # print("#################################################3")
  # print(pl)
  return(p)
  h2o.shutdown()
}


