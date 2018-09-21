suppressMessages(library(dplyr))

folder <- "C:/Users/Yosuke/Documents/GitHub/Nonresponse_DK/"
act_value <- read.csv(paste(folder,"nme_R031.24563.20180310090217.01.csv",sep = ""), header = FALSE)
act_value <- act_value %>%
  filter(V1 != "2017/12") %>%
  rename(time = V1, op1 = V2, op3 = V3, op5 = V4, gp1 = V5, gp3 = V6, gp5 = V7)
act_value <- act_value[-1,]

chosa <- c("14/03","14/06","14/09","14/12",
           "15/03","15/06","15/09","15/12",
           "16/03","16/06","16/09","16/12",
           "17/03","17/06","17/09")

names(pdfFonts())
pdf(file = "fig_6.pdf", paper='A4', family = "Japan1")
par(mfrow = c(2,3))
plot(x = c(1:15), y = as.numeric(as.character(act_value$gp1)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3), main = "あい�?えお", cex.main = 2)
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$gp1))+0.1, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = 0:3, labels = c(0,1,2,3), las = 1, cex.axis = 1.5)

plot(x = c(1:15), y = as.numeric(as.character(act_value$gp3)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3), main = "gp3")
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$gp3))-0.15, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = 0:3, labels = c(0,1,2,3), las = 1, cex.axis = 1.5)

plot(x = c(1:15), y = as.numeric(as.character(act_value$gp5)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3), main = "gp5")
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$gp5))-0.15, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0,3))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = 0:3, labels = c(0,1,2,3), las = 1, cex.axis = 1.5)
legend("topright", legend = c("公表�?計値", "推定値", "95%信頼区�?"), 
       lwd = c(3,2,1),
       lty = c("solid","solid","dashed"),
       col = c("green","red","black"),
       bty = "n",
       cex = 1.3)


plot(x = c(1:15), y = as.numeric(as.character(act_value$op1)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-2,4), main = "あい�?えお", cex.main = 1.7)
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$op1))+0.15, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-2,4))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = -2:4, labels = c(-2,-1,0,1,2,3,4), las = 1, cex.axis = 1.5)

plot(x = c(1:15), y = as.numeric(as.character(act_value$op3)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-3,6), main = "op3")
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$op3))-0.2, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-3,6))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = -3:6, labels = c(-3,-2,-1,0,1,2,3,4,5,6), las = 1, cex.axis = 1.5)

plot(x = c(1:15), y = as.numeric(as.character(act_value$op5)), type = "l", lwd = 3, col = "green",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-8,8), main = "op5")
par(new = TRUE)
plot(x = c(1:15), y = as.numeric(as.character(act_value$op5))-0.15, type = "l", lwd = 2, col = "red",
     axes = FALSE, xlab = "", ylab = "", ylim = c(-8,8))
mtext(text = "%",adj = 0)
axis(side = 1, at = 1:15, labels = chosa, cex.axis = 1.4)
axis(side = 2, at = -8:8, labels = c(-8,"",-6,"",-4,"",-2,"",0,"",2,"",4,"",6,"",8), las = 1, cex.axis = 1.5)


dev.off()