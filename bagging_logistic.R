  suppressMessages(library(foreach))
  suppressMessages(library(dplyr))
  
  Hajime <- proc.time()
  
  mydata <- NULL
  N <- 10000
  
  for(i in 1:13){
    assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
    mydata <- cbind(mydata, get(paste("di",i,sep = "")))
    mydata <- as.data.frame(mydata)
    names(mydata)[i] <- paste("di",i,sep = "")
  }
  mydata$inf_exp <- sample(0:1, size = N, replace = TRUE)
  mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))
  mydata_p <- mydata %>%
    filter(inf_exp == 1)
  mydata_u <- mydata %>%
    filter(inf_exp == 0)
  
  mydata_names <- names(mydata)
  f <- as.formula(paste("inf_exp~", paste(mydata_names[mydata_names != "inf_exp"], collapse = "+")))
  
  length_divisor = 4
  sampling <- sample(nrow(mydata_u), size = 2*floor((nrow(mydata_u))/length_divisor))
  mydata_u_train <- mydata_u[sampling,]
  mydata_u_test <- mydata_u[-sampling,]
  mydata_train <- rbind(mydata_u_train, mydata_p)
  mydata_test <- rbind(mydata_u_test, mydata_p)
  
  B = 1000
  predictions <- foreach(m = 1:B, .combine = cbind) %do% {
      training_positions <- sample(nrow(mydata_train), size = floor((nrow(mydata_train)/length_divisor)))
      train_pos <- 1:nrow(mydata_train) %in% training_positions
      glm_fit <- glm(formula = f, data = mydata_train[train_pos,], family = binomial)
      predict(glm_fit, newdata = mydata_test)
    }
  predictions <- rowMeans(predictions)
  rmse <- sqrt((sum((mydata_test$inf_exp - predictions)^2))/nrow(mydata_test))
  rmse
  
  proc.time() - Hajime