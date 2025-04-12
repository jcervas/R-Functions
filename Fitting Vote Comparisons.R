


### Fitting Congressional District vote
require("readstata13")

cong <- read.dta13("/Users/cervas/Google Drive/School/UCI/Papers/Nationalization of Congressional Races/Data/data/congressionalelectionresults.dta")

year1 <- "2004"
year2 <- "2006"
cong.04 <- subset(cong, year==year1)
cong.08 <- subset(cong, year==year2)
period1 <- paste0(year1, " Democrat Two-Party Vote")
period2 <- paste0(year2, " Democrat Two-Party Vote")
cong.dat <- merge(cong.04, cong.08, by=c("state","district"))
cong.dat <- subset(cong.dat, cong.dat$demvotesmajorpercent.x >0 & cong.dat$demvotesmajorpercent.x <100)
cong.dat <- subset(cong.dat, cong.dat$demvotesmajorpercent.y >0 & cong.dat$demvotesmajorpercent.y <100)


cong.x <- cong.dat$demvotesmajorpercent.x/100
cong.y <- cong.dat$demvotesmajorpercent.y/100

## Plot data
plot(cong.x, cong.y, xlim=c(0,1), ylim=c(0,1), xlab=period1, ylab= period2, bty="n", cex=.65, pch = 10, col="gray60") 
	

	abline(0,1, lty=2, col="gray80")
		rect(-.05,-.05,0,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #left
		rect(0,0,1.05 ,-.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #bottom
		rect(1,0,1.05,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #right
		rect(0,1,1,1.05, col = rgb(0.1,0.1,0.1,1/2), density=40, border = "transparent") #top



#	legend(.655, .18, legend=c("Equality Line","Linear Fit", "log(y)=log(x) Fit", "log(1-y) = log(1-x) Fit"), col=c("gray80","gray70","gray70","black"), lty=c(2,1,1,1), lwd=c(1,1,1,1), cex=.8)




## Linear Fit

summary(reg <- lm(cong.y~cong.x))
#R-sqr = .932

	abline(reg, col='gray2', lwd=1)
	
	
## Fit with simple exponent function

summary( reg2 <- lm( log(cong.y) ~ log(cong.x) ))
## R-sqr = .89
funct <- function (x) x^summary(reg2)$coefficients[2]




plot(funct, from= 0, to = 1, add=TRUE,lwd=1, col="gray30", lty=1)



## Predictions overcompensate for outlying points but respects the anchor points, lets try [log(1-y) = log(1-x)]



summary( reg3 <- lm( I(log(1 - cong.y)) ~ I(log( 1 - cong.x) )))
# R-sqr = .9603
funct2 <- function (x) x^(1/summary(reg3)$coefficients[2])

plot(funct2, from= 0, to = 1, add=TRUE, lwd=2.5, col="black") ## Much better fit
# text(.89,.2, paste0("y<- x^(1/", round(summary(reg3)$coefficients[2], digits=3),")"))


## Cubic

# # summary( reg4 <- lm( cong.y ~ poly(cong.x,3)))
# funct3 <- function (x) summary(reg4)$coefficients[1] + (summary(reg4)$coefficients[2]*x) + (summary(reg4)$coefficients[3]*x^2) + (summary(reg4)$coefficients[4]*x^3)
# plot(funct3, from= 0, to = 1, add=TRUE, lwd=2.5, col="black") ## 

# S-Shape Curve

summary( reg5 <-  lm( I(log(cong.y/(1-cong.y))) ~ I(log(cong.x/(1-cong.x))) ))

funct4 <- function (x) (x^(summary(reg5)$coefficients[2])) / + ((x^(summary(reg5)$coefficients[2])) + (1-(x^(summary(reg5)$coefficients[2]))))

plot(funct4, from= 0, to = 1, add=TRUE, lwd=2.5, col="black") ## 





