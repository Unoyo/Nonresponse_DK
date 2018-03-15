suppressMessages(library(kernlab))
suppressMessages(library(KernSmooth))
suppressMessages(library(foreach))
suppressMessages(library(dplyr))
suppressMessages(library(doParallel))

mydata <- NULL
N <- 100000

foreach(i = 1:13, .inorder = FALSE) %do%{
  assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
  mydata <- cbind(mydata, get(paste("di",i,sep = "")))
  mydata <- as.data.frame(mydata)
  names(mydata)[i] <- paste("di",i,sep = "")
}
mydata$inf_exp1 <- sample(0:1, size = N, replace = TRUE, prob = c(1/5,4/5))
mydata$inf_exp3 <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
mydata$inf_exp5 <- sample(0:1, size = N, replace = TRUE, prob = c(3/5,2/5))
mydata$ind <- as.factor(sample(1:3, size = N, replace = TRUE))


## fraction of outlier ##
mydata <- mydata %>%
  mutate(outlier = ifelse(inf_exp1 == 1 & inf_exp3 == 1 & inf_exp5 == 1, 0,
                          ifelse(inf_exp1 == 1 & inf_exp3 == 1 & inf_exp5 == 0, 0,
                                 ifelse(inf_exp1 == 1 & inf_exp3 == 0 & inf_exp5 == 0, 0,
                                        ifelse(inf_exp1 == 0 & inf_exp3 == 0 & inf_exp5 == 0, 0, 1))))) 

## step1 ##
mydata_p <- mydata %>%
  filter(inf_exp1 == 1) %>%
  select(-outlier)
mydata_u <- mydata %>%
  filter(inf_exp1 == 0) %>%
  select(-outlier)

nu <- 0.01 #sum(mydata$outlier)/length(mydata$outlier)
occ_svm <- ksvm(x = inf_exp1~., data = mydata_p, type = "one-svc", kernel = "rbfdot",
                kpar = "automatic", C = 1/nu*N, nu = nu)
occ_pred  <- as.numeric(predict(occ_svm, newdata = mydata_u))
L <- length(occ_pred)

## step2 ##
dif_U <- 1
new_positive <- as.data.frame(occ_pred)
colnames(new_positive) <- c("new_positive")
mydata_u_temp_U <- as.data.frame(mydata_u)

repeat{
  if(dif_U > 0){
    U_size_temp <- length(mydata_u_temp_U$di1)
    mydata_u_temp_N <- mydata_u_temp_U %>%
      cbind(new_positive) %>%
      filter(new_positive == 0) %>%
      select(-new_positive)
    mydata_u_temp_U <- mydata_u_temp_U %>%
      cbind(new_positive) %>%
      filter(new_positive == 1) %>%
      select(-new_positive)
    
    sample(nrow(mydata_p), size = L)
    glm_fit <- glm(formula = inf_exp1~., 
                   data = rbind(mydata_p[L,], mydata_u_temp_N), 
                   family = binomial)
    pred <- predict(glm_fit, newdata = mydata_u_temp_U, type = "response")
    new_positive <- pred >= 0.5
    new_positive <- as.data.frame(new_positive)
    colnames(new_positive) <- c("new_positive")
    dif_U <- U_size_temp - length(mydata_u_temp_U$di1)
  } else {
    break
  }
}

