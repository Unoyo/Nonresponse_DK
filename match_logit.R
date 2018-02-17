suppressMessages(library(pROC))
suppressMessages(library(MatchIt))
suppressMessages(library(dplyr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))

mydata <- NULL
N <- 10000

registerDoParallel(detectCores())

foreach(i = 1:13, .inorder = FALSE) %do%{
  assign(paste("di",i,sep = ""),as.factor(sample(1:3, size = N, replace = TRUE)))
  mydata <- cbind(mydata, get(paste("di",i,sep = "")))
  mydata <- as.data.frame(mydata)
  names(mydata)[i] <- paste("di",i,sep = "")
}
mydata$inf_exp <- sample(0:1, size = N, replace = TRUE, prob = c(2/5,3/5))
mydata$ind <- as.factor(sample(1:100, size = N, replace = TRUE))

var_name <- c("di1","di2","di3","di4","di5",
              "di6","di7","di8","di9","di10",
              "di11","di12","di13","ind")
subset <- combn(x = var_name, m = 6)

res <- NULL

foreach(j = 1:ncol(subset), .inorder = FALSE, .packages = c("dplyr","MatchIt","pROC")) %dopar% {
  mydata <- mydata %>%
  select(inf_exp, one_of(subset[,j])) 

  mydata_names <- names(mydata)
  f <- as.formula(paste("inf_exp~", paste(mydata_names[mydata_names != "inf_exp"], collapse = "+")))
  match <- matchit(formula = f,
                   data = mydata,
                   method = "exact") 

  mydata2 <- mydata %>%
    mutate(temp = max(inf_exp, as.numeric(match$weights != 0))) %>%
    select(temp, one_of(subset[,j]))
  
  mydata2_names <- names(mydata2)
  g <- as.formula(paste("temp~", paste(mydata2_names[mydata2_names != "temp"], collapse = "+")))
  logitres <- glm(formula = g, 
                  family = binomial, 
                  data = mydata2)
  roc <- roc(response = mydata2$temp, predictor = logitres$fitted.values)
  res <- cbind(res, c(ci.auc(roc, conf.level = 0.95), logitres$aic))
}

write.csv(res, file = "res.csv", quote = FALSE)
