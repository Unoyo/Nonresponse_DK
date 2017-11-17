suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

Hajime <- proc.time()

outlook <- NULL

for(y in c(1,3,5)){
  assign(paste("op",y,sep = ""),sample(1:10, size = 160000, replace = TRUE))
  assign(paste("gp",y,sep = ""),sample(1:13, size = 160000, replace = TRUE))
  outlook <- cbind(outlook, get(paste("op",y,sep = "")))
  outlook <- cbind(outlook, get(paste("gp",y,sep = "")))
}

yymm <- sample(1:15, size = 160000, replace = TRUE)
ID <- sample(1:10000, size = 160000, replace = TRUE)
outlook <- cbind(outlook,yymm,ID)


outlook <- as.data.frame(outlook)
names(outlook) <- c("op1","gp1","op3","gp3","op5","gp5","yymm","ID")

# identifying satisficer
satisficer <- outlook[outlook$op1==10 &
                        outlook$op3==10 &
                        outlook$op5==10 &
                        outlook$gp1==10,]

satisficer_t <- satisficer %>%
  group_by(yymm) %>%
  summarise(count = n())

satisficer_t
satisficer

proc.time() - Hajime