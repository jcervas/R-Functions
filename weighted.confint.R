
weighted.confint <- function(data, weight){
n <- length(data)
m <- weighted.mean(data, weight, na.rm=TRUE)

sd <- sqrt(sum(((data-m)^2)/n, na.rm=TRUE))
# sd <- sd(data)

se <- sd / sqrt(n)
low <- m - se * qt(0.975,n-1)
high <- m + se * qt(0.975,n-1)
con <- cbind((m),(low),(high))
colnames(con) <- c("mean","low","high")
return(con)
}

