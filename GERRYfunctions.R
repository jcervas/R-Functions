cat(
  "\n
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
    LOADING FUNCTIONS. . . . . . . . . . . . . . . . . . . . . . 
•••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••\n")
# Transparent Colors
't_col' <- function(color, percent = 50) {
  rgb.val <- col2rgb(color)
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
    max = 255,
    alpha = (100 - percent) * 255 / 100)
t.col
}

latex.special.chr <- function(x) {
  x <- gsub("$\\hat{\\mkern6mu}$", "^", x,fixed=TRUE)
  x <- gsub("\\textasteriskcentered ", "*", x,fixed=TRUE)
  x <- gsub("\\textbackslash ", "\\", x,fixed=TRUE)
  x <- gsub("\\{", "{", x,fixed=TRUE)
  x <- gsub("\\}", "}", x,fixed=TRUE)
  x <- gsub("\\$", "$", x,fixed=TRUE)
}

 `Figure` <- function(path=NULL, caption="", label="", footnote="")
  {
    x <- paste0("\n
% =====================================================================
% ▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟
% ---------------------------------------------------------------------\n",
ifelse(is.null(path),
paste0("\\begin{center} \\textbf{", caption, "} \\end{center}"),
paste0("\\input{", path, "}")),

"\n \\begin{center} INSERT FIGURE \\ref{", label, "} ABOUT HERE \\end{center}
% ---------------------------------------------------------------------
% ▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄▀▄▀▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄▀▄▀▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄
% ===================================================================== \n",

"\n% =====================================================================
% ▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟G̟U̟R̟E̟ ▄▀▄▀▄▀▀▄▀▄▀▄ F̟I̟
% ---------------------------------------------------------------------
% " , caption,
"% •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
% 
\\begin{figure}
    \\begin{center}
    \\caption{",caption, "}
    \\label{", label, "}
    \\includegraphics[width=1\\textwidth]{", path, "}
    \\end{center}
    \\tabnotes{", footnote, "}
\\end{figure}
% ---------------------------------------------------------------------
% ▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄▀▄▀▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄▀▄▀▀▄▀▄▀▄ E͎N͎D͎ F͎I͎G͎U͎R͎E͎ ▄▀▄
% =====================================================================\n \n"
)
  cat(paste(x, collapse = "\n"), "\n")
  }

# Functions
convertTitle <- function(m) gsub("(?<=\\p{L})(\\p{L}+)", "\\L\\1", m, perl = TRUE)
deleteSpaces <- function(m) gsub(" ", "", m, fixed = TRUE)
substrRight <- function(x, n) substr(as.character(x), nchar(as.character(x))-n+1, nchar(as.character(x)))
substrLeft <- function(x, n) substr(as.character(x), 1, n)
substrPunct <- function(m) gsub("[[:punct:]]", "", m)
standardizeText <- function(m) substrLeft(deleteSpaces(tolower(substrPunct(m))),30)
leadingZeroes <- function(m,d=0) formatC(m, width = d, format = "d", flag = "0")
readFixed <- function(x, y) { 
  new <- numeric()
  tmp <- readLines(x)
  tmp <- do.call(rbind, as.list(tmp))
  for (i in y) {
    new <- cbind(new, substr(tmp, 1,i))
    tmp <- substr(tmp, i+1,nchar(tmp))
  }
  return(as.data.frame(new))
}

rep.data.frame <- function(x, times) {
  rnames <- attr(x, "row.names")
  x <- lapply(x, rep.int, times = times)
  class(x) <- "data.frame"
  if (!is.numeric(rnames)) 
    attr(x, "row.names") <- make.unique(rep.int(rnames, times))
  else attr(x, "row.names") <- .set_row_names(length(rnames) * times)
  x
}

`new.list` <-
function(len) {
  out <- list(NA)
  if (len<1) stop ("Can't make a list of length ",len,".")
  for (ii in 1:len) out[[ii]] <- NA
  return(out)
}

`vote.shift` <- # Seats at c vote
function(x, w=NULL, c=0.5) {
    if(is.null(w)) w <- rep(1,length(x))
  tmp <- mean.w(x, w = w) - c
    tmp.votes <- x - tmp
    return(seats.mean(tmp.votes))
}


`cube` <- function (x)  x^3/(1-3*x+3*x^2)

seats.display <- function(x) {
  tmp <- numeric()
    for (i in 1:dim(x)[2]) {
      rep <- seats.mean(x[,i]) * length(x[,i])
      dem <- length(x[,i]) - rep
      tmp <- rbind(tmp, paste0(rep,"R-", dem, "D"))
    }
    return(tmp)
  }

`two_party` <- 
  function(D, R) {
    replaceNA(as.numeric(D))/(replaceNA(as.numeric(D))+replaceNA(as.numeric(R)))
    }

`find.winner` <- 
  function(inp) 0*(inp<0.5)+1*(inp>0.5)

`dummies` <- 
  function(inp) -1*(inp<0.5)+1*(inp>0.5)

`seats.mean` <- 
  function(inp,w=NULL) {
    if(is.null(w)) {w <- rep(1,length(inp))} 
    return(mean.w(inp>.5,w)) }

`seats.w` <- function(x,w) {
  if(is.null(w)) {w <- rep(1,length(x))} 
    return(sum(find.winner(x)*w)/sum(w)) }

`make.weights` <-  
  function(x, d = 5) {
    r(x/sum(x, na.rm=T), d = d)}

`f.num` <- function(x, d=2) format(round(x, d=d), nsmall=d)

`percent` <- 
  function(x, d = 1) {
    paste0(f.num(x * 100, d= d), "%")
  }

`mean.w` <- # WEIGHTED MEAN
function (x, weight=NULL, na.rm = TRUE, ...) {
    if (is.null(weight)) weight <- rep(1, length(x))
      w <- as.double(weight)
    if (na.rm) {
        i <- !is.na(x)
        w <- w[i]
        x <- x[i]
    }
    sum((x * w)[w != 0])/sum(w)}

inv <- function(x) {
  (exp(x) / ( 1 + exp(x)))
}

sv.curve <- function(s,v) {
  reg <- lm(log(sv(s)) ~ log(sv(v)))
  vote <- seq(0.01,0.99, by=.01)
  seatvotes <-  reg$coefficients[2]*log(vote/(1-vote)) + reg$coefficients[1]
  return(inv(seatvotes))
}

sv <- function(x) (x / (1 - x))

`seats.print` <- function(x) paste0(sum(find.winner(x)), "R-", length(x)-sum(find.winner(x)), "D")

`agg.precinct` <- function(data, var, id) {
        cbind.data.frame(
          REP = aggregate(as.numeric(data[,paste0(var, "R")]), by=list(id=data[,id]), FUN=sum)[,2], 
          DEM = aggregate(as.numeric(data[,paste0(var, "D")]), by=list(id=data[,id]), FUN=sum)[,2]
        )
      }

comp.raw <- function(data) {
  (two_party(as.numeric(data[,"T16PRESR"]), as.numeric(data[,"T16PRESD"])) +
  two_party(as.numeric(data[,"T16SENR"]), as.numeric(data[,"T16SEND"])) +
  two_party(as.numeric(data[,"T16ATGR"]), as.numeric(data[,"T16ATGD"])) +
  two_party(as.numeric(data[,"T16AUDR"]), as.numeric(data[,"T16AUDD"])) +
  two_party(as.numeric(data[,"T16TREASR"]), as.numeric(data[,"T16TREASD"]))
  ) / 5
  }

composite <- function(data, id) {
  data <- data[order(data[,id]),]
  cbind.data.frame(
        CONG = two_party(aggregate(as.numeric(data[,"T16CONGR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16CONGD"]), by=list(id=data[,id]), FUN=sum)[,2]),
        PRES = two_party(aggregate(as.numeric(data[,"T16PRESR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16PRESD"]), by=list(id=data[,id]), FUN=sum)[,2]), 
        USSEN = two_party(aggregate(as.numeric(data[,"T16SENR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16SEND"]), by=list(id=data[,id]), FUN=sum)[,2]) ,
        ATTGEN = two_party(aggregate(as.numeric(data[,"T16ATGR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16ATGD"]), by=list(id=data[,id]), FUN=sum)[,2]) ,
        AUDITOR = two_party(aggregate(as.numeric(data[,"T16AUDR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16AUDD"]), by=list(id=data[,id]), FUN=sum)[,2]) ,
        TREASURER = two_party(aggregate(as.numeric(data[,"T16TREASR"]), by=list(id=data[,id]), FUN=sum)[,2], aggregate(as.numeric(data[,"T16TREASD"]), by=list(id=data[,id]), FUN=sum)[,2]) ,
        COMPOSITE = composite.sum(data, id)
        )
  }

composite.sum <- function(data, id) {
  two_party(
            (
              aggregate(as.numeric(data[,"T16PRESR"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16SENR"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16ATGR"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16AUDR"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16TREASR"]), by=list(id=data[,id]), FUN=sum)[,2]
            ),
            (
              aggregate(as.numeric(data[,"T16PRESD"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16SEND"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16ATGD"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16AUDD"]), by=list(id=data[,id]), FUN=sum)[,2] + 
              aggregate(as.numeric(data[,"T16TREASD"]), by=list(id=data[,id]), FUN=sum)[,2]
            )
          )
  }

std <- function(x) {
  (mean(x[!is.na(x)]) - x) / sd(x[!is.na(x)])
  }

# ci <- function(x)
#   {
#     r <- summary(lm(x~1))
#       m <- coef(r)[1]
#       se <- coef(r)[2]
#       return(qt(0.975, df=length(x)-1) * se)
#   }

`quick.summary` <- function (x) 
  {
  `qsum` <- function (set) cbind(mean(set[!is.na(set)]),
                            sd(set[!is.na(set)]),
                            var(set[!is.na(set)]),
                            min(set[!is.na(set)]),
                            max(set[!is.na(set)]),
                            sum(1*!is.na(set)),
                            sum(1*is.na(set)))
    out <- qsum(x)
  colnames(out) <- c("Mean","SD","Variance","Min","Max","Valid","Missing")
    return(out)
  }

`quick.sum` <- 
  function (x) {
  `qsum` <- 
    function (set) cbind(
                      mean(set[!is.na(set)]),
                      mean(set[!is.na(set)]) - ci(set[!is.na(set)]),
                      mean(set[!is.na(set)]) + ci(set[!is.na(set)]),
                      sd(set[!is.na(set)]),
                      min(set[!is.na(set)]),
                      quantile(set[!is.na(set)], 0.025),
                      quantile(set[!is.na(set)], 0.975),                            
                      max(set[!is.na(set)])
                      )

    out <- qsum(x)
  colnames(out) <- c("Mean","CI_lower","CI_upper","SD","Min","2.5%","97.5%","Max")
    return(out)}

  comb <-combn(c(0:9,LETTERS[1:6]),2)
  opacity <- c(paste0(comb[1,1:120], comb[2,1:120]), paste0("FF"))

`unc` <- function(x) -1 * (x <= .25) + 1 * (x >= .75)

`replaceNA` <- 
  function (x, value=0) {
   x[is.na(x)] <- value 
   return(x)}
   
# `delete.unc` <-
# function(vs, uncL=0.25, uncU=0.75) {
#   #replaces uncontested vote values with "missing".
#  f1 <- function (a,b,c) ifelse (is.na(a),NA,
#                                  ifelse(a<b,NA, ifelse(a>c,NA,a)))
#   return(sapply (vs,f1,uncL,uncU)) }

`replace.unc` <-
function(vs, l, u, lr, ur, na.rm = TRUE) {
  f1 <- function(a) {
    if (is.na(a)) return(NA)
    if (a < l) a <- lr
    else if (a > u) a <- ur
    a
  }

  result <- sapply(vs, f1)
  if (na.rm) result[is.na(result)] <- 0
  result
}

`default.unc` <-
function (vs, uncL = 0.25, uncU = 0.75, uncLR = 0.25, uncUR = 0.75) {
  vs <- replace.unc(vs, uncL, uncU, uncLR, uncUR)
  return(vs)}

'appl' <- function(vs) {
  if (is.vector(vs)) {
    vs[vs == 0] <- 0.25
    vs[vs == 1] <- 0.75
    return(vs)
  } else {
    stop("Input must be a vector.")
  }
}



# `mean.unc` <- # uncL and uncU to replace outside bounds, otherwise just NAs
# function(vs, uncL = 0.0001, uncU = 0.99999) {
#   vs <- delete.unc(vs, uncL=uncL, uncU=uncU)
# tmp <- vs[!is.na(vs)]
# reg <- summary(lm(tmp ~ 1))$coef[1] # mean
# reg_sig <- summary(lm(tmp ~ 1))$coef[2] # sigma
#   new.mean <- truncate(rnorm(length(vs[is.na(vs)]), reg, reg_sig))
#   vs[is.na(vs)] <- new.mean
#   return(vs)}

# Helper function to delete uncontested vote values
delete.unc <- function(a, b=0.25, c=0.75) {
  ifelse(is.na(a), NA,
         ifelse(a < b | a > c, NA, a))
}

mean.unc <- function(vs, uncL = 0.0001, uncU = 0.99999) {

  # Delete uncontested vote values
  tmp <- delete.unc(vs, uncL, uncU)

  # Compute mean and sigma from non-NA values
  reg <- mean(tmp, na.rm = TRUE)
  reg_sig <- sd(tmp, na.rm = TRUE)

  # Generate new values from a normal distribution with mean and sigma
  new.mean <- rnorm(sum(is.na(vs)), mean = reg, sd = reg_sig)

  # Replace NA values in the original vector with new.mean
  vs[is.na(vs)] <- new.mean

  return(vs)
}

`impute.weights` <- # for use when turnout in districts is abnormally high or low (w = # of standard deviations from median)
function(t1, t2, w = 2) {
  t <- t1+t2
  uncL <- (median(t, na.rm=T) -  (w * sd(t, na.rm=T)))
  uncU <- (median(t, na.rm=T) + (w * sd(t, na.rm=T)))
  t1 <- delete.unc(t, uncL=uncL, uncU=uncU)
tmp <- t1[!is.na(t1)]
reg <- summary(lm(tmp ~ 1))$coef[1] # mean
reg_sig <- summary(lm(tmp ~ 1))$coef[2] # sigma
  new.reg <- r(rnorm(length(t1[is.na(t1)]), reg, reg_sig), d=0)
  t1[is.na(t1)] <- new.reg
  return(t1)}


`truncate` <-
function(r1) {
  below <- r1<=0
  above <- r1>=1
  between <- !(below|above)
  (below*0.000001)+(r1*between)+0.999999*above
}


sim.election <- function(votes= NULL, center=house.2016.votes, incumbency=NULL, yr=2018, sims=1000, sigma=sigma) {
  if (is.null(sims)) sims <- 1000
      equal.vote <- mean(votes) - mean(center)
      sims.year <- new.list(sims)
    for (k in 1:sims)
      {
    sims.year[[k]] <- 
        rnorm(length(votes), votes - equal.vote, 
          sigma)
      }
  return(sims.year)
  }

r <- function(r, d=2) round(r, digits=d)



# =================================================================
# -- GERRYMANDER MEASURES -- GERRYMANDER MEASURES -- GERRYMANDER ME
# =================================================================
# ••• DELINATION ••••••••••••••••••••••••••••••••••••••••••••••••••
`declination` <- # Warrington, Gregory S. 2018. “Quantifying Gerrymandering Using the Vote Distribution.” Election Law Journal 17(1): 39–57. www.liebertpub.com (Accessed February 22, 2019).
  function(votes) {
    abo = votes[votes > 0.5]   # districts won by party A
    bel = votes[votes <= 0.5]  # districts won by party B
  # declination is undefined if one party wins all seats.
    if (length(bel) == 0 | length(abo) == 0) {
      return(NaN)
    }
  # angle for party B
    theta = atan((1-(2*mean(bel))) / (length(bel) / length(votes)))
  # angle for party A
    gamma = atan((2*mean(abo)-1) / (length(abo) / length(votes)))
  # normalize from radians to values betwen -1 and 1
  # A little extra precision just in case :)
    return(-1 * (2.0*(gamma-theta)/3.1415926535))}

`declin2` <- # Simplified, not transformed from: Katz, Jonathan N. et al. 2018. Theoretical Foundations and Empirical Evaluations of Partisan Fairness in District-Based Democracies *. https://gking.harvard.edu/files/psym_2.pdf (Accessed March 16, 2019).
  function(vs) {
    abo = vs[vs > 0.5]   # districts won by party A
    bel = vs[vs <= 0.5]  # districts won by party B
    (-1 * (mean(abo) - 0.5) / (sum(find.winner(abo))/length(vs))) - ((0.5 - mean(bel)) / (1 - sum(find.winner(abo))/length(vs)))
  }
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# ••• MEAN/MEDIAN••••••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
`meanmedian` <- # Best, Robin E., Shawn J. Donahue, Jonathan Krasno, Daniel B. Magleby, et al. 2018. “Considering the Prospects for Establishing a Packing Gerrymandering Standard.” Election Law Journal: Rules, Politics, and Policy 17(1): 1–20. http://www.liebertpub.com/doi/10.1089/elj.2016.0392 (Accessed July 24, 2018).
  function(votes) median(votes) - mean(votes)
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••• EFFICIENCY GAP••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# `eff_gap` <- # Stephanopoulos, Nicholas, and Eric Mcghee. 2014. “Partisan Gerrymandering and the Efficiency Gap.” University of Chicago Law School 82. http://ssrn.com/abstract=2457468.. (Accessed September 10, 2018).
#   function(D, R) {
#     total <- D+R
#     half.vote <- total/2
#     r.w <- 0 + (1 * R>D)
#     d.w <- 0 + (1 * R<D)
#     d.wasted <- (D*r.w) + ((D*d.w)-(half.vote*d.w))
#     r.wasted <- (R*d.w) + ((R*r.w)-(half.vote*r.w))
#       return((sum(r.wasted)-sum(d.wasted)) / sum(total))
#     }

eg_TP <- function(votes) 
  {
    y <- cbind.data.frame(votes, 1-votes)
      return(efficiency_gap(y[1], y[2]))
  }

efficiency_gap  <- function(party1, party2) {
    wv <- wasted_votes(party1,party2)
    total_votes = sum(party1)+sum(party2)
    numerator = wv[2]-wv[1]
    return (numerator / total_votes)
}

wasted_votes <- function(party1, party2) {
    total_votes = party1+party2
    party1_waste <- party2_waste <- rep(NA,length(total_votes))
        party1_waste[party1 > party2] <- party1[party1 > party2] - total_votes[party1 > party2] / 2
        party2_waste[party1 > party2] <- party2[party1 > party2]
        party2_waste[party1 < party2] <- party2[party1 < party2] - total_votes[party1 < party2] / 2
        party1_waste[party1 < party2] <-  party1[party1 < party2]

        party1_waste[party1 == party2] <- 0
        party2_waste[party1 == party2] <- 0
    return (c(sum(party1_waste),sum(party2_waste)))
}

# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# •••• GERRY DISPLAY ••••••••••••••••••••••••••••••••••••••••••••••
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
  gerry <- function(x, toggle=TRUE)
    {
    Seats = paste0(" [", seats.print(x), "]")
    SeatPER = percent(seats.mean(x))
    Votes = percent(mean(default.unc(x)))
    Bias = r(seatsvotes(x)$bias)
    EfficiencyGap = r(eg_TP(x))
    MeanMedian = r(meanmedian(x))
    Declination = r(declination(x))
    a <- rbind.data.frame(
      Seats, SeatPER, Votes, Bias, EfficiencyGap, MeanMedian, Declination)
    rownames(a) <- c("Seats","Seat %","Votes","Bias","Efficiency Gap","Mean/Median","Declination")
    colnames(a) <- "Summary"

    if (toggle!=TRUE) { # TO RETURN UNFORMATTED MEASUREMENTS
    SeatsT = NA
    SeatPERT = r(seats.mean(x))
    VotesT = r(mean(default.unc(x)))
    BiasT = r(seatsvotes(x)$bias)
    EfficiencyGapT = r(eg_TP(x))
    MeanMedianT = r(meanmedian(x))
    DeclinationT = r(declination(x))
      return(rbind.data.frame(
      SeatsT, SeatPERT, VotesT, BiasT, EfficiencyGapT, MeanMedianT, DeclinationT))}
    return(a)
    }

p_value <- function(x){
  l <- quantile(x[!is.na(x)], 0.025)
  u <- quantile(x[!is.na(x)], 0.975)
  t <- mean(x)/sd(x)
    return(2*pt(-abs(t),df=length(x)-1))
  }

strs <- function(s) {
  strs <- 
  ifelse(s <= 0.001, "***", 
    ifelse(0.001 < s & s <= 0.01, "**", 
      ifelse(0.01 < s & s <= 0.05, "*", "")))
  return(paste0("$^{", strs, "}$"))
}

nintyfive <- function(x, percent=FALSE) {
  if (percent==TRUE){ return(paste0("(", percent(r(quantile(x[!is.na(x)], 0.025),d=1)), ", ", percent(r(quantile(x[!is.na(x)], 0.975),d=1)), ")")) }
  return(paste0("{\\small\\textit{[", r(quantile(x[!is.na(x)], 0.025)), ", ", r(quantile(x[!is.na(x)], 0.975)), "]}}"))
  }

gtab <- function(x) {
  p <- (paste0(x, ".sims.5050"))
  p.tmp <- get(p)
  s.tmp <- maps.sims.seats.5050[[x]]
  v.tmp <- maps.sims.votes.5050[[x]]
  x.bias <- get(paste0(p, ".bias"))
  x.eg <- get(paste0(p, ".eg"))
  x.mm <- get(paste0(p, ".meanmedian"))
  x.declin <- get(paste0(p, ".declination"))
    return(c(
      paste0(r(mean(x.bias)), strs(p_value(x.bias))), 
        nintyfive(x.bias), 
      paste0(r(mean(x.eg)), strs(p_value(x.eg))),
        nintyfive(x.eg), 
      paste0(r(mean(x.mm)), strs(p_value(x.mm))),
        nintyfive(x.mm),
      paste0(r(mean(x.declin)), strs(p_value(x.declin))),
        nintyfive(x.declin)
      ))
    }

ptab <- function(x) {
  p <- (paste0(x, ".sims.5050"))
  p.tmp <- get(p)
  s.tmp <- maps.sims.seats.5050[[x]]
  v.tmp <- maps.sims.votes.5050[[x]]
    return(c(
      paste0(r(sum(s.tmp*18)/(1000*18)*18, d=1), "R-", r(18-sum(s.tmp*18)/(1000*18)*18, d=1), "D"),
      paste0("{\\small\\textit{[", r(quantile(s.tmp, 0.025),d=1)*18, "R-", 18-r(quantile(s.tmp, 0.025),d=1)*18, "D, ", r(quantile(s.tmp, 0.975),d=1)*18, "R-", 18-r(quantile(s.tmp, 0.975),d=1)*18, "D]}}"),
      paste0(median(s.tmp)*18, "R-", 18-median(s.tmp)*18, "D"),
      percent(sum(1 * do.call(rbind, lapply(p.tmp, function(x) mean(find.winner(x)))) > 0.5) / 1000),
      percent(sum(1 * do.call(rbind, lapply(p.tmp, function(x) mean(find.winner(x)))) < 0.5) / 1000),
      percent(sum(1 * do.call(rbind, lapply(p.tmp, function(x) mean(find.winner(x)))) == 0.5) / 1000)
      ))
} 

 # •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••   



`circle.new` <- 
  function(xorig, yorig, radius, add=F, ...){
    x <- seq(-radius, radius, length.out = 1000)
  # Euclidian distance to the origin
  y <- sapply(x, function(z) sqrt(radius^2 - z^2))
  if(add == TRUE){
    plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
      type = "l", add=T)
   } else {
   plot(xorig + c(x, rev(x)), c(yorig + y, yorig + rev(-y)),
    type = "l",  
    xlab="", 
    ylab="", 
    xaxt="n", 
    yaxt="n", 
    bty="n")
 
   }
}

`circle` <- 
function (x, y, radius, nv = 1000, border = NULL, col = NA, lty = 1, main="", lwd = 1, add=F) {
    xylim <- par("usr")
    plotdim <- par("pin")
    ymult <- getYmult()
    angle.inc <- 2 * pi/nv
    angles <- seq(0, 2 * pi - angle.inc, by = angle.inc)

        xv <- cos(angles) * radius + x
        yv <- sin(angles) * radius * ymult + y
        if (add==T) {
          polygon(xv, yv, 
            col = col, 
            lty = lty, 
            lwd = lwd)
        } else {
          plot(xv, yv, 
            type="l", 
            border = border, 
            col = col, 
            lty = lty,
            lwd = lwd,
            xlab="", 
            ylab="", 
            xaxt="n", 
            yaxt="n", 
            bty="n",
            main=main)
      }
    invisible(list(x = xv, y = yv))
}
# •••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••••
# COMPACTNESS MEASURES

POLSBYPOPPER <- function (x) r(mean((4 * 3.1415926535 * x$area) / (x$perimeter^2)), d = 3)
REOCK <- function (x) r(mean(x$area/(x$smallestcircle^2 * 3.1415926535)), d = 3)


poly.math <- function (x) {
    compactness <- NULL
  if (class(x)=="character") x <- get(x)
      shapeFile <- x
      mapObject <- fortify(shapeFile)
      mapObject$id <- as.character(as.numeric(mapObject$id) + 1)
      mapObject <- data.frame(mapObject, shapeFile@data[mapObject$id, ])
      mapObject$piece <- as.character(mapObject$piece)
      uniqueCDs <- sort(unique(as.numeric(mapObject$id)))
      for(id in uniqueCDs)
        {
          cdShape <- mapObject[mapObject$id == id, ]
          cdPoly <- SpatialPolygons(list(Polygons(lapply(split(cdShape[, c("long", "lat")], cdShape$piece), Polygon), ID = "b")))
          owinObject <- try(as(cdPoly, "owin"))
          compactness[[id]] <-  data.frame(area=area.owin(owinObject), perimeter=perimeter(owinObject), smallestcircle=boundingradius(owinObject))
        }
        x <- do.call("rbind", compactness)
        cat(
          "\n REOCK:        ", REOCK(x), "\n",
          "POLSBY-POPPER:", POLSBYPOPPER(x), "\n \n"
          )
        return(x)
  }





getYmult<-function() {
 if(dev.cur() == 1) {
  warning("No graphics device open.")
  ymult<-1
 }
 else {
  # get the plot aspect ratio
  xyasp<-par("pin")
  # get the plot coordinate ratio
  xycr<-diff(par("usr"))[c(1,3)]
  ymult<-xyasp[1]/xyasp[2]*xycr[2]/xycr[1]
 }
 return(ymult)
}




ShapleyShubik <- function (quota, y, Names = NULL) 
{
    n <- length(y)
    res1 <- permutations(n, n)
    res2 <- apply(res1, 1, function(x) {
        x[sum(cumsum(y[x]) < quota) + 1]
    })
    res2 <- as.numeric(res2)
    Power <- matrix(NA, ncol = 1, nrow = n)
    for (i in 1:n) {
        Power[i, 1] <- sum(res2 == i)
    }
    SHI <- Power/factorial(n)
    TABLE <- rbind(y, y/sum(y), t(SHI))
    rownames(TABLE) <- c("Votes", "Votes (%)", "Shapley-Shubik")
    colnames(TABLE) <- Names
    Output <- list(Results = TABLE, Distribution = y, C, Method = "PowerIndex", 
        Quota = quota, Names = Names)
    class(Output) <- "ShapleyShubik"
    return(Output)
}

'permutations' <- function (n) 
{
    if (n == 1) 
        return(matrix(1))
    else if (n < 2) 
        stop("n must be a positive integer")
    z <- matrix(1)
    for (i in 2:n) {
        x <- cbind(z, i)
        a <- c(1:i, 1:(i - 1))
        z <- matrix(0, ncol = ncol(x), nrow = i * nrow(x))
        z[1:nrow(x), ] <- x
        for (j in 2:i - 1) {
            z[j * nrow(x) + 1:nrow(x), ] <- x[, a[1:i + j]]
        }
    }
    dimnames(z) <- NULL
    z
}

