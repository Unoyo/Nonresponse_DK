suppressMessages(library(EMCluster))

Hajime <- proc.time()


#### Spy EM algorithm  ####

mydata <- NULL
N <- 160000

for(i in 1:13){
  assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
  mydata <- cbind(mydata, get(paste("di",i,sep = "")))
  mydata <- as.data.frame(mydata)
  names(mydata)[i] <- paste("di",i,sep = "")
}
mydata$inf_exp <- sample(0:1, size = N, replace = TRUE)
mydata$ind <- as.factor(sample(1:500, size = N, replace = TRUE))


logit <- glm(formula = inf_exp ~., family = binomial, data = mydata)
summary(logit)




proc.time() - Hajime
