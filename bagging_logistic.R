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
  mydata$inf_exp <- sample(0:1, size = N, replace = TRUE, prob = c(3/4,1/4))
  mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))
  mydata_p <- mydata %>%
    filter(inf_exp == 1)
  mydata_u <- mydata %>%
    filter(inf_exp == 0)
  
  mydata_names <- names(mydata)
  f <- as.formula(paste("inf_exp~", paste(mydata_names[mydata_names != "inf_exp"], collapse = "+")))
  
  m = 4
  B = 10
  predictions <- foreach(m = 1:B, .combine = cbind) %do% {
      M <- sample(nrow(mydata_u), size = floor((nrow(mydata_u)/m)))
      glm_fit <- glm(formula = f, 
                     data = rbind(mydata_p, mydata_u[M,]),
                     family = binomial)
      predict(glm_fit, newdata = mydata_u, type = "response")
    }
  predictions <- rowMeans(predictions)
  plot(predictions)
  u_p <- predictions[predictions > 0.5]
  u_n <- predictions[predictions <= 0.5]

  proc.time() - Hajime