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
mydata$inf_exp <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))

mydata_p <- mydata %>%
  filter(inf_exp == 1)
mydata_u <- mydata %>%
  filter(inf_exp == 0)

occ_svm <- ksvm(x = inf_exp~., data = mydata_p, type = "one-svc", kernel = "rbfdot",
                kpar = list(sigma = 0.1), nu = 0.1)
u_n <- predict(occ_svm, mydata_u)
length(u_n)
sum(u_n)