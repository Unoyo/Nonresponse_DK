suppressMessages(library(foreach))
suppressMessages(library(dplyr))
suppressMessages(library(MatchIt))
 
Hajime <- proc.time()
  
mydata <- NULL
N <- 10000
  
for(i in 1:13){
  assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
  mydata <- cbind(mydata, get(paste("di",i,sep = "")))
  mydata <- as.data.frame(mydata)
  names(mydata)[i] <- paste("di",i,sep = "")
}

mydata$inf_exp <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))

var <- c("inf_exp","ind",
         "di1","di2","di3","di4","di5","di6","di7",
         "di8","di9","di10","di11","di12","di13")

mydata <- mydata %>%
  select(one_of(var))

mydata_names <- colnames(mydata)
colnames(mydata)[1] <- "treat"
h <- as.formula(paste("treat~", paste(mydata_names[mydata_names != "treat"], collapse = "+")))

match_res <- matchit(formula = h, data = mydata, method = "exact")

