library(foreign)
# library(visreg)
library(stargazer)
greycol <- rgb(red = 190, green = 190, blue = 190, alpha = 170, maxColorValue = 255)

source("/Users/cervas/Google Drive/School/UCI/R Functions/weighted.confint.R")
cmps <- read.dta("/Users/cervas/Google Drive/Data/CMPS/2016/CMPSclean.dta", convert.factors=FALSE)




#For Analyses that only include citizen respondents
cmps.citizen <- cmps[cmps$citizen==1,]

#Create subsets for the Racial Groups
cmps.white <- cmps.citizen[cmps.citizen$ethnic_quota == "White",]
cmps.hispanic <- cmps.citizen[cmps.citizen$ethnic_quota=="Hispanic",]
cmps.black <- cmps.citizen[cmps.citizen$ethnic_quota=="Black",]
cmps.asian <- cmps.citizen[cmps.citizen$ethnic_quota=="Asian",]



base.form <- formula(. ~ age + age70 + female + faminc + educ + pidS + leader)



	pdf("/Users/jcervas/Desktop/NormVotingLogitPredicitonsJune1.pdf", width=14, height=10)
	par(mfrow=c(3,4), oma= c(3,1.5,0,0), mar=c(2,2,3,1), mgp= c(2,1,0))
	
datasource <- list(cmps.white, cmps.black, cmps.hispanic, cmps.asian)
variable <- c("normvote", "normdonating", "normprotest", "normsocialmedia", "normcontact", "normproblemsolving")
DV <- c("vote", "donating", "protest", "socialmedia", "contact", "problemsolving")
DVt <- c("Voting", "Donating", "Protesting", "Social Media Usage", "Contacting Officials", "Problem Solving w/ Community")
race <- c("White", "Black", "Hispanic", "Asian")
ivlist <- c("constant", toString(variable[t]), "age" , "age70" , "female" ,"educ" , "pidS", "faminc",  "leader")




PredictedProbabilities <- list()
WeightedMeans <- list()
probs <- matrix(NA, nrow=length(race), ncol=4)
w.means <- matrix(NA, nrow=length(race), ncol=4)
rownames(probs) <- race
colnames(probs) <- c(0,1,2,3)
rownames(w.means) <- race
colnames(w.means) <- c(0,1,2,3)
for (t in 1:length(DV)){
for (j in 1:length(datasource)){
  f1 <- glm(update(base.form, paste0(DV[t], "~ ", variable[t], "+ .")), weight=weight, data=datasource[[j]], family="binomial", na.action=na.omit)
	
intervals <- seq(min(datasource[[j]][,variable[t]], na.rm=TRUE) , max(datasource[[j]][,variable[t]], na.rm=TRUE) , 0.01)
intmeans <- seq(min(datasource[[j]][,variable[t]], na.rm=TRUE), max(datasource[[j]][,variable[t]], na.rm=TRUE), 0.01)

inputs <- list()
carole <- matrix(NA, nrow=length(intervals), ncol = 3)
colnames(carole) <- c("low", "point", "high")
rownames(carole) <- intervals
for (i in 1:length(intervals)) {
	int <- intervals[i]
inputs[[i]] <- datasource[[j]]
inputs[[i]][, variable[t]] <- int
	a <- predict(f1, newdata=inputs[[i]], se.fit = TRUE, type = "link")
	carole[i,"high"] <- weighted.mean(plogis(a$fit+a$se.fit*1.96), datasource[[j]][, "weight"], na.rm=TRUE)
	carole[i,"low"]<- weighted.mean(plogis(a$fit-a$se.fit*1.96), datasource[[j]][, "weight"], na.rm=TRUE)
	carole[i,"point"] <- weighted.mean(plogis(a$fit), datasource[[j]][, "weight"], na.rm=TRUE)

if (int %in% c(0,1,2,3)) { 
	probs[j, toString(int)] <- carole[i,"point"]
	w.means[j,toString(int)] <- weighted.mean(datasource[[j]][,DV[t]][datasource[[j]][,variable[t]]== int], datasource[[j]][, "weight"][datasource[[j]][,variable[t]]== int], na.rm=TRUE)
	}
if (int==3){ 
	PredictedProbabilities[[t]] <- probs
	WeightedMeans[[t]] <-  w.means
	}
### At Means 
meansinputs <- as.data.frame(cbind(constant=1, v = intmeans, age=weighted.mean(datasource[[j]][,"age"], datasource[[j]][,"weight"],na.rm=TRUE), age70=weighted.mean(datasource[[j]][,"age70"], datasource[[j]][,"weight"],na.rm=TRUE), female=weighted.mean(datasource[[j]][,"female"],datasource[[j]][,"weight"],na.rm=TRUE), educ=weighted.mean(datasource[[j]][,"educ"],datasource[[j]][,"weight"],na.rm=TRUE), pidS=weighted.mean(datasource[[j]][,"pidS"],datasource[[j]][,"weight"],na.rm=TRUE), faminc=weighted.mean(datasource[[j]][,"faminc"],datasource[[j]][,"weight"],na.rm=TRUE), leader=weighted.mean(datasource[[j]][,"leader"],datasource[[j]][,"weight"],na.rm=TRUE)))
colnames(meansinputs)[colnames(meansinputs)=="v"] <- variable[t]
### At Means ^^^

if (int == max(intervals) ){ 
	plot(intervals,carole[,"point"], type="n", lwd=2, lty=1, ylim=c(0,1), yaxt="n", xaxt="n", bty="n", ylab="", xlab="", main=paste0(race[j], " Respondents"))

abline(h=seq(0,1,.2), lty=2, col="gray80", cex=0.85)
if (j==1){ mtext(side=2, line=2, paste0("P(",DVt[t], ")"), cex=0.65) }
	lines(intervals,carole[,"point"])
	polygon(x= c(intervals, rev(intervals)),  y= c(carole[,"high"], rev(carole[,"low"])), col=greycol, border=NA)
	# lines(intervals, carole[,"low"], lty=3, col="gray70")
	# lines(intervals, carole[,"high"], lty=3, col="gray70")
	lines(intmeans, plogis(predict(f1, newdata=meansinputs, type="link")), lwd=2, col="gray20")
	legend("topleft", legend=c(
	paste0("constant =", round(coef(f1)[1],digits=2), " (", round(coef(summary(f1))[1,2], digits=2), ")"), 
	paste0(toString(variable[t])," =", round(coef(f1)[2],digits=2), " (", round(coef(summary(f1))[2,2], digits=2), ")"), 
	paste0("Age =", round(coef(f1)[3],digits=2), " (", round(coef(summary(f1))[3,2], digits=2), ")") , 
	paste0("Age > 70 =", round(coef(f1)[4], digits=2), " (",  round(coef(summary(f1))[4,2], digits=2), ")"), 
	paste0("Female =", round(coef(f1)[5],digits=2), " (",  round(coef(summary(f1))[5,2], digits=2), ")") ,
	paste0("Education Level =", round(coef(f1)[6],digits=2), " (",  round(coef(summary(f1))[6,2], digits=2), ")") ,
	paste0("PIDs =", round(coef(f1)[7],digits=2), " (",  round(coef(summary(f1))[7,2], digits=2), ")"), 
	paste0("Income =", round(coef(f1)[8],digits=2), " (",  round(coef(summary(f1))[8,2], digits=2), ")"),  
	paste0("Represented by Leader =", round(coef(f1)[9],digits=2), " (",  round(coef(summary(f1))[9,2], digits=2), ")")),
	cex=.85, box.col = "white", xjust = .5, yjust = 1, bg = "white")
	axis(side=2, las=1, cex.axis=0.65)
	axis(side=1, at=c(0,3), labels=c("Lowest Norms", "Highest Norms"), cex.axis=0.65)
	 }
}

}
}
dev.off()
names(PredictedProbabilities) <- DVt
names(WeightedMeans) <- DVt


write.csv(do.call("rbind", lapply(PredictedProbabilities, as.data.frame)), "/Users/jcervas/Desktop/probs.csv")
write.csv(do.call("rbind", lapply(WeightedMeans, as.data.frame)), "/Users/jcervas/Desktop/weightedmeans.csv")