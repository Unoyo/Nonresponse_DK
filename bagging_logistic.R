  suppressMessages(library(foreach))
  suppressMessages(library(dplyr))
  suppressMessages(library(doParallel))
  
  Hajime <- proc.time()
  
  mydata <- NULL
  N <- 10000
  
  registerDoParallel(detectCores())

  foreach(i = 1:13, inorder = FALSE) %do%{
    assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
    mydata <- cbind(mydata, get(paste("di",i,sep = "")))
    mydata <- as.data.frame(mydata)
    names(mydata)[i] <- paste("di",i,sep = "")
  }
  mydata$inf_exp <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
  mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))
  mydata_p <- mydata %>%
    filter(inf_exp == 1)
  mydata_u <- mydata %>%
    filter(inf_exp == 0)
  
  mydata_names <- names(mydata)
  f <- as.formula(paste("inf_exp~", paste(mydata_names[mydata_names != "inf_exp"], collapse = "+")))
  
  div <- 2
  B <- 100
  
  predictions <- foreach(j = 1 : B, .combine = cbind, .inorder = FALSE) %dopar% {
      M <- sample(nrow(mydata_u), size = floor((nrow(mydata_u)/div)))
      glm_fit <- glm(formula = f, 
                     data = rbind(mydata_p, mydata_u[M,]),
                     family = binomial)
      predict(glm_fit, newdata = mydata_u, type = "response")
    }
  predictions <- rowMeans(predictions)
  plot(predictions)
  u_p <- predictions[predictions > 0.5]
  u_n <- predictions[predictions <= 0.5]
  length(u_p) 
  length(u_n)

  proc.time() - Hajime
