suppressMessages(library(pROC))
suppressMessages(library(MatchIt))
suppressMessages(library(dplyr))
suppressMessages(library(foreach))
suppressMessages(library(doParallel))

subset <- combn(x = var_name[var_name != "y"], m = 3)

res <- NULL

for(i in 1:ncol(subset)){
  data <- data %>%
    select(y,one_of(subset[,i])) %>% 
    mutate(z = NULL) %>%
    na.omit
  
  match <- matchit(formula = y~.,
                   data = data,
                   method = "exact") 
  data$y_temp <- data$y + as.numeric(match$weights != 0)
  data$y_temp[data$y_temp == 2] <- 1
  
  logitres <- glm(formula = y~., 
                  family = binomial, 
                  data = data)
  roc <- roc(response = data$y_temp, predictor = logitres$fitted.values)
  res <- cbind(res, c(ci.auc(roc, conf.level = 0.95), logitres$aic))
}
write.csv(res, file = "res.csv", quote = FALSE)

