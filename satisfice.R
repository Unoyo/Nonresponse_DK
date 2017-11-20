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

satisficer_t$chosa <- c("2014/03","2014/06","2014/09","2014/12",
                        "2015/03","2015/06","2015/09","2015/12",
                        "2016/03","2016/06","2016/09","2016/12",
                        "2017/03","2017/06","2017/09")

pdf(file="satisficer.pdf",paper='A4r')
par(mfrow = c(2,1),oma = c(0,0,6,0))
par(lwd = 2)
plot(x = 1:15, y = satisficer_t$count, type = "l", xlab = "", ylab = "", main = "", xaxt = "n", cex.main = 2)
axis(side=1, 1:15,labels=satisficer_t$chosa)
mtext(text = "# of obs.",adj=0)
dev.off()

proc.time() - Hajime
