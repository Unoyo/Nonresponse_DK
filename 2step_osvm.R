suppressMessages(library(kernlab))
suppressMessages(library(KernSmooth))
suppressMessages(library(foreach))
suppressMessages(library(dplyr))

mydata <- NULL
N <- 10000

foreach(i = 1:13, .inorder = FALSE) %do%{
  assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
  mydata <- cbind(mydata, get(paste("di",i,sep = "")))
  mydata <- as.data.frame(mydata)
  names(mydata)[i] <- paste("di",i,sep = "")
}
mydata$inf_exp1 <- sample(0:1, size = N, replace = TRUE, prob = c(1/5,4/5))
mydata$inf_exp3 <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
mydata$inf_exp5 <- sample(0:1, size = N, replace = TRUE, prob = c(3/5,2/5))
mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))


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

nu <- sum(mydata$outlier)/length(mydata$outlier)
occ_svm <- ksvm(x = inf_exp1~., data = mydata_p, type = "one-svc", kernel = "rbfdot",
                kpar = list(sigma = 0.1), nu = nu)
rn  <- predict(occ_svm, newdata = mydata_u)

## step2 ##


