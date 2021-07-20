# Script for Part B

# 4 Countries:
# NGA -> Nigeria
# NOR -> Norway
# KNA -> Saint Kitts and Nevis
# IND -> India

# Question 2
countries <- read.csv("realgdp.csv")
nga <- ts(countries[,"NGA"], frequency = 1, start = 1970)
nor <- ts(countries[,"NOR"], frequency = 1, start = 1970)
kna <- ts(countries[,"KNA"], frequency = 1, start = 1970)
ind <- ts(countries[,"IND"], frequency = 1, start = 1970)
# log scale now
lnga <- log(nga)
lnor <- log(nor)
lkna <- log(kna)
lind <- log(ind)
plot(lnga, main = "Real Per Capita GDP", ylim=c(6,11),
     ylab="real per capita GDP (log value)",
     xlab="Time(Years)")
lines(lnor, lty=2, col=2)
lines(lkna, lty=3, col=3)
lines(lind, lty=4, col=4)
#grid(nx=10,ny=10,col="darkgray")
legend("bottomright", bty = "n", 
       c("Nigeria", "Norway","Saint Kitts and Nevis",
         "India"),
       col = c(1,2,3,4), lty = c(1,2,3,4), cex = 0.5)

# Question 3
# Nigeria Quadratic Trend
tnga <- time(lnga, offset=0.5)
tnga2 <- tnga^2
fitnga <- lm(lnga~tnga+tnga2)
coefnga <- coef(fitnga)
qtrendnga <- coefnga[1] + coefnga[2]*tnga + coefnga[3]*tnga2
cyclicalnga <- lnga - qtrendnga
plot(cyclicalnga, main="Cyclical components"
     ,ylab="real per capita GDP")
lines(cyclicalnor, lty=2, col=2)
lines(cyclicalkna, lty=3, col=3)
lines(cyclicalind, lty=4, col=4)
legend("bottomright", bty = "n",
       c("Nigeria", "Norway","Saint Kitts and Nevis", "India"),
       lty=c(1,2,3,4), col=c(1,2,3,4), cex=0.6)

# Norway Quadratic Trend
tnor <- time(lnor, offset=0.5)
tnor2 <- tnor^2
fitnor <- lm(lnor~tnor+tnor2)
coefnor <- coef(fitnor)
qtrendnor <- coefnor[1] + coefnor[2]*tnor + coefnor[3]*tnor2
cyclicalnor <- lnor - qtrendnor
plot(cyclicalnor)

# SK&N Quadratic Trend
tkna <- time(lkna, offset=0.5)
tkna2 <- tkna^2
fitkna <- lm(lkna~tkna+tkna2)
coefkna <- coef(fitkna)
qtrendkna <- coefkna[1] + coefkna[2]*tkna + coefkna[3]*tkna2
cyclicalkna <- lkna - qtrendkna
plot(cyclicalkna)

# India Quadratic Trend
tind <- time(lind, offset=0.5)
tind2 <- tind^2
fitind <- lm(lind~tind+tind2)
coefind <- coef(fitind)
qtrendind <- coefind[1] + coefind[2]*tind + coefind[3]*tind2
cyclicalind <- lind - qtrendind
plot(cyclicalind)



# Question 4
#calculating growth rate using exact formula
# Nigeria
grnga <- diff(nga)/lag(nga,-1)
grnga <- (1+grnga)^12 - 1
agrnga <- grnga
avggrnga <- mean(agrnga)
lnga70 <- ts(lnga, start = 1970, end = 1970)

# Norway
grnor <- diff(nor)/lag(nor,-1)
grnor <- (1+grnor)^12 - 1
agrnor <- grnor
avggrnor <- mean(agrnor)
lnor70 <- ts(lnor, start = 1970, end = 1970)

# Saint Kitts & Nevis
grkna <- diff(kna)/lag(kna,-1)
grkna <- (1+grkna)^12 - 1
agrkna <- grkna
avggrkna <- mean(agrkna)
lkna70 <- ts(lkna, start = 1970, end = 1970)

# India 
grind <- diff(ind)/lag(ind,-1)
grind <- (1+grind)^12 - 1
agrind <- grind
avggrind <- mean(agrind)
lind70 <- ts(lind, start = 1970, end = 1970)

# plotting
plot(lnga70,avggrnga, pch = 0, col = 0, main="Scatter plot",
     xlim=c(0,15), ylim=c(0,15), ylab="Average Growth Rate",
     xlab="real per capita GDP(1970, log value)")
points(x=lnga70, y=avggrnga, pch=21, col=2)
points(x=lnor70, y=avggrnor, pch=21, col=3)
points(x=lkna70, y=avggrkna, pch=21, col=4)
points(x=lind70, y=avggrind, pch=21, col=7)
legend("topleft", bty = "n", 
       c("Nigeria", "Norway","Saint Kitts and Nevis",
         "India"), pch = 21, col = c(2,3,4,7), cex=0.8)

# Question 5
pcgdp <- ts(countries[,-1], start = 1970)
thousands <- pcgdp/1000
pcgdp83 <- window(thousands, 1983, 1983)
hist(pcgdp83, breaks=25, 
     xlab="Real per capita GDP 
     (thousands of international dollars of 2011)",
     ylab="No. Of Countires", 
     main="Distribution of real per 
     capita GDP across country in 1983",
     cex=0.8, labels=TRUE, col="gray", ylim = c(0,40))
pcgdp13 <- window(thousands, 2013, 2013)
hist(pcgdp13, breaks=25, 
     xlab="Real per capita GDP 
     (thousands of international dollars of 2011)",
     ylab="No. Of Countires", 
     main="Distribution of real per 
     capita GDP across country in 2013",
     cex=0.8, labels=TRUE, col="gray", ylim = c(0,50))

# Question 6
logval <- log(pcgdp)
loggdp83 <- window(logval, 1983, 1983)
hist(loggdp83, breaks=25, 
     xlab="Real per capita GDP 
     (log of international dollars of 2011)",
     ylab="No. Of Countires", 
     main="Distribution of real per 
     capita GDP across country in 1983",
     cex=0.8, labels=TRUE, col="grey", ylim=c(0,14))
loggdp13 <- window(logval, 2013, 2013)
hist(loggdp13, breaks=25, 
     xlab="Real per capita GDP 
     (log of international dollars of 2011)",
     ylab="No. Of Countires", 
     main="Distribution of real per 
     capita GDP across country in 2013",
     cex=0.8, labels=TRUE, col="gray", ylim = c(0,14))
