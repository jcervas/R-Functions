

YEARS <- seq(1868,2016,4)

ec.dta <- numeric()
for (j in 1:length(YEARS)) {
  tmp <- cbind.data.frame(year = YEARS[j], VOTE = electiondata[[j]]$pctdem, ecvotes = electiondata[[j]]$ecvotes)
  ec.dta <- rbind(ec.dta, tmp)
  }

coefs <- array(NA, c(length(YEARS), 2)) #create empty matrix to store coefficients
  rownames(coefs) <- YEARS
resid.errors <- rep(NA, length(YEARS)) #empty vector to store residual errors
for (i in 1:length(YEARS)){#loop over each 
    fit <- lm(default.unc(VOTE) ~ 1)
    coefs[i,] <- coef(fit)
    resid.errors[i] <- arm::sigma.hat(fit)
    coefs <- replaceNA(coefs)
        }
 

        #now use only 1958 on, non redistricting years (02)
house.data.replicate <- house0618no02[house0618no02$year > 2005,]
house.data.replicate <- house.data.replicate[!house.data.replicate$year %in% c(seq(2002,2022,10), 2006, 2018),]

unique.year.all <- unique(pres_tmp$year)
adv <- rep(NA, length(unique(unique.year.all)))
house.data.replicate$dvoteimputed <- default.unc(house.data.replicate$VOTE)
house.data.replicate$REP_lag <- default.unc(house.data.replicate$REP_lag)
YEARS <- unique(YEARS)


for (i in 1:length(unique.year.all))
{
  y.tmp <- unique.year.all[i]
    adv[i] <- mean(VOTE[pres_tmp$year == y.tmp], na.rm=T)#get average for each year
}

dem.seats <- c(seats.2004, seats.2006,seats.2008,seats.2010,seats.2012,seats.2016, seats.2018)

keep <- ifelse(unique.year.all > 1956 & unique.year.all !=2002 & unique.year.all != 2004 & unique.year.all != 2006 & #use indicator to pull out adv and seats 
    unique.year.all != 2012 & unique.year.all != 2018, 1, 0)
adv.rep <- adv[keep==1]
dem.seats.rep <- dem.seats[keep==1]

#set up vectors and parameter estimates for validation loop
n.sims <- 1000 #number of simulations
inc.impute <- .75 #for uncontested races
sbar.rep <- rep (NA, n.sims)#vector for predicted seats -- redrawn for each year.
axis.size <- 1 #control size of axis labels
predicted.seats <- rep(NA, length(YEARS))#predicted number of seats, based on actual adv
predictive.error <- rep(NA, length(YEARS))#predictive error of model
predictive.sd <- rep(NA, length(YEARS)) #standard deviation of seats at actual adv
partisan.bias <- rep(NA, length(YEARS))#partisan bias in each year (not used in paper)
################################################################################
#note: this loop will take a long time to run with 1,000 sims 
    #(could lower to 100 with no substantive interpretative loss)
#create Figure 2
pdf("Seats_Votes_Curves_Over_Time_all_years.pdf", height = 8, width = 10)
#use layout command to set up different size panels, so that we can plot y-axis labels in 1st column
    #and x-axis labels on bottom row (i.e. not separate labels for each year.
    #For more on layout command see Murrell's "R Graphics" book
left.panel.width <- .8 # control width of left-hand panel (see "widths" in layout command)
layout(rbind(c(20, 1, 2,3,4,5), c(21, 6, 7,8,9,10), #order so that each year first, then y-axis labels, then x-axis lables
    c(22,11, 12, 13, 14, 15), c(23, 16, 17, 18, 19, 28),
    c(30, 24, 25, 26, 27, 29)),                          # then column labels, then row labels
     widths = cbind(c(left.panel.width,2,2,2,2,2), c(left.panel.width,2,2,2,2,2),c(left.panel.width,2,2,2,2,2),
        c(left.panel.width,2,2,2,2,2),c(left.panel.width,2,2,2,2,2)),
    heights = c(1,1,1,1,.6))#make last row smaller since it's just a label, no graph
#layout.show(30)#this shows how layout looks in R Window (but must be commented out when creating PDF)
for (i in 1:length(YEARS))
  {#loop over validation years

rho <-  mean(coefs[i:(i+1),2])#get rho by taking mean coef from 5 years leading up to election year
sigma <- mean(resid.errors[i:(i+1)])#get mean residual standard error from past 5 years
phi <- 0 #unique(house.data.replicate$incumb.effect[house.data.replicate$year == YEARS[i]]) #get estimated incumbency advantage for particular election year
v.lag <- house.data.replicate$REP_lag[house.data.replicate$year==YEARS[i]]#imputed vote

inc.lag <- rep(0, length(house.data.replicate$REP_lag)) #house.data.replicate$incumb.lag[house.data.replicate$year==YEARS[i]]#lagged incumbency
inc <-  rep(0, length(house.data.replicate$REP_lag)) #house.data.replicate$incumb[house.data.replicate$year==YEARS[i]]#incumbency status
unc <- rep(0, length(house.data.replicate$REP_lag)) #house.data.replicate$uncontested[house.data.replicate$year==YEARS[i]]#uncontested status
vbar.rep.lag <- mean(v.lag)#mean of vote from last election
vbar.rep.range <- round(vbar.rep.lag,2) + seq(-.1,.1,.0015) #create range from .45 to 55, at every .015 of seats
sbar.rep.expected <- rep (NA, length(vbar.rep.range))#set up vector for predicted seats over each interval in range
sbar.rep.sd <- rep (NA, length(vbar.rep.range))#set up vector for standard deviation of predicted seats
for (j in 1:length(vbar.rep.range)){#loop over intervals of vbar.rep
 vbar.rep <- vbar.rep.range[j]
 for (s in 1:n.sims){#loop ove simulations
   v.adj.lag <- v.lag - phi*inc.lag #adjusted vote, taking out incumbency
   normvote <- .5 + rho*(v.adj.lag - .5) #normal vote
   locfree <- normvote + phi*inc #location free: normal vote plus adjusted vote
   locfreenoisy <- rnorm(length(locfree), locfree, sigma) #add noise to loc.free var
   withuncs <- ifelse(unc==-1, 1-inc.impute,#add in uncontesteds (.25 and .75)
                           ifelse (unc==1, inc.impute, locfreenoisy))
   swingfree <- withuncs + mean(v.lag) - mean(withuncs) #take out swing
   v.predict <- swingfree + vbar.rep - mean(swingfree) #get predicted vote
   sbar.rep[s] <- mean(v.predict>.5)#predicted seats = percent of sims where predicted vote > .5
 }

sbar.rep.expected[j] <- mean(sbar.rep) #for each interval, median predicted seats across simulations
sbar.rep.sd[j] <- sd(sbar.rep) #for each interval, get standard deviation of seats given votes across sims
}

predicted.seats[i]<- mean(sbar.rep.expected[round(vbar.rep.range,2)==round(adv.rep[i],2)])#use mean b/c it's possible for multiple values 
                                                                                           #of vbar.rep.range to equal adv in a given year
predictive.error[i] <- dem.seats.rep[i] - predicted.seats[i]
partisan.bias[i] <- mean(2*(sbar.rep.expected[round(vbar.rep.range,2)==.50]-.5))
predictive.sd[i] <- mean(sbar.rep.sd[round(vbar.rep.range,2)==round(adv.rep[i],2)])


}
cbind.data.frame(year=YEARS, predictedseats= predicted.seats, partisanbias=partisan.bias, predictiveerror=predictive.error, predictivesd=predictive.sd)

predicted.seats
predictive.error
partisan.bias
predictive.sd



rmse.predict <- sqrt(sum(predictive.error^2)/length(YEARS))#Get RMSE for prediction; 
                                                                     #this is used for generating probabilities for 2006 and 2008
rmse.sd <- sqrt(sum(predictive.sd^2)/length(YEARS))#RMSE of standard deviation of seats




`uncparty` <- 
  function(inp) ifelse(inp>0.75, 1, ifelse(inp<0.25, -1, 0))

bias.gelman <- numeric()

for (k in files16)
  {
plan.inp <- district_votes[, k]

n.sims <- 1000
v2016 <- default.unc(plan.inp)
vbar.2016 <- mean(v2016)

vbar.range <- round(vbar.2016,2) + seq(-.1,.1,.002) #create range from 45 to 55
sbar.50 <- rep (NA, length(vbar.range))#vector for predicted seats (based on medians)
prob <- rep (NA, length(vbar.range)) #set up vector for prob. of winning house
  for (j in 1:length(vbar.range))
    {#loop over intervals of vbar
   vbar <- vbar.range[j]
   sbar <- rep (NA, n.sims) 
   for (i in 1:n.sims){
     v.adj2016 <- v2016 - 0 #phi*i2004 #adjusted vote, taking out incumbency
     normvote2016 <- .5 + rho*(v.adj2016 - .5) #normal vote
     locfree2016 <- normvote2016 + 0 #phi*i2006 #location free: normal vote plus adjusted vote
     locfreenoisy2016 <- rnorm(length(locfree2016), locfree2016, sigma) #add noise to loc.free var
     withuncs2016 <- ifelse (uncparty(v2016)==-1, 1-inc.impute,
                             ifelse (uncparty(v2016)==1, inc.impute, locfreenoisy2016))
     swingfree2016 <- withuncs2016 + mean(v2016) - mean(withuncs2016) #take out swing
     v.2016 <- swingfree2016 + vbar - mean(swingfree2016) 
     v.2016 <- default.unc(v.2016)
     #V2006 <- withuncs2016 + vbar + mean(v2016) - mean(withuncs2006) - mean(swingfree2006) 
     sbar[i] <- mean(v.2016>.5)
   }
   sbar.50[j] <- mean(sbar)
   prob[j] <- pnorm((sbar.50[j] - 0.5)/rmse.predict)#use empirical predictive error for historical regressions (above)
  }
  bias.gelman[k] <- mean(2*(sbar.50[round(vbar.range,2)==.50]-.5))
}



#get v.bars when prob. == 10, 50, 90%
ten.percent.value <- mean(vbar.range[round(prob,1) ==.10])
fifty.percent.value <- mean(vbar.range[round(prob,1) ==.50])
ninety.percent.value <- mean(vbar.range[round(prob,1) ==.90])
print(ten.percent.value)
print(fifty.percent.value)
print(ninety.percent.value)

#get predicted seats and probability based on adv. for 2006
pred.seats.2016 <- mean(sbar.50[vbar.range==round(vbar.2016,3)])
seats.error.2016 <- seats(find.winner(votes.2016.tp)) - pred.seats.2016
bias.2016 <- mean(2*(sbar.50[round(vbar.range,2)==.50]-.5))
prob.2016 <- prob[vbar.range == round(vbar.2016,2)]
#get probability dems would have won house if they got the same adv as GOP in 1994
prob.1994 <- prob[vbar.range == round(1-adv[unique.year.all==1994],2)]
#get number of seats GOP would have won with dems average district vote.
gop.seats.pred <- 1-mean(sbar.50[vbar.range==round(1-vbar.2016,3)])
#gop.seats.pred*435 = 249




