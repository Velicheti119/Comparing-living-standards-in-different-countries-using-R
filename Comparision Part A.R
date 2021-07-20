# Part A

# Question 1
dat <- read.csv("Wage146.csv")
mdat <- ts(dat[,2], frequency = 1, start = 1997)
fdat <- ts(dat[,3], frequency = 1, start = 1997)
p <- ts(dat[,4], frequency = 1, start = 1997)

#plotting
plot(mdat, main = "Hourly nominal wages in
      Business, building and other
     support services (British Columbia)",
     xlab = "Year", ylab = "Wages (dollars/hour)")
lines(fdat, lty = 2, col = 2)
grid(nx=10,ny=10,col="darkgray")
legend("topleft", bty = "n", c("Male", "Female"),
       col = 1:2, lty = 1:2, lwd= 1:1)

# Question 2
#calc
realmdat <- mdat/(p/100)
realfdat <- fdat/(p/100)

#plotting
plot(realmdat, main = "Hourly real wages in
      Business, building and other
     support services (British Columbia)",
     xlab = "Year", ylab = "Wages (dollars/hour)")
lines(realfdat, lty = 2, col = 2)
grid(nx=10,ny=10,col="darkgray")
legend("topleft", bty = "n", c("Male", "Female"),
       col = 1:2, lty = 1:2, lwd= 1:1)

# Question 3
# trend for males
mt <- time(realmdat, offset = 0.5)
mfit <- lm(realmdat~mt)
ma <- coef(mfit)
mtrend <- ma[1] + ma[2]*mt

#trend for females
ft <- time(realfdat, offset = 0.5)
ffit <- lm(realfdat~ft)
fa <- coef(ffit)
ftrend <- fa[1] + fa[2]*ft

#plotting the trends
plot(realmdat, main = "Hourly real wages in
      Business, building and other
     support services (British Columbia)",
     xlab = "Year", ylab = "Wages (dollars/hour)")
lines(realfdat, lty = 2, col = 2)
grid(nx=10,ny=10,col="darkgray")
lines(mtrend, col = "blue", lty = 3)
lines(ftrend, col = "green", lty = 4)
legend("topleft", cex=0.7, bty="n",
       c("Male","Female","Male Lin.Trend","Female Lin.Trend")
       , col = c(1,2,3,4), lty = c(1,2,3,4))

# Question 4
mdetrend <- realmdat-mtrend
fdetrend <- realfdat-ftrend
plot(mdetrend,fdetrend, xy.labels = FALSE,
     xy.lines = FALSE,
     xlab="Male real wage detrended series",
     ylab="Femlae real wage detrended series",
     main="Scatterplot between 
     both genders detrended series")
#plot(fdetrend, mdetrend, xy.labels = FALSE,
#    xy.lines = FALSE)
