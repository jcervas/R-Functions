source("/Users/cervas/My Drive/GitHub/R-Functions/Leeper/slopegraph.R")
source("/Users/cervas/My Drive/GitHub/R-Functions/Leeper/segmentize.R")
source("/Users/cervas/My Drive/GitHub/R-Functions/Leeper/bump_overlaps.R")
options(stringsAsFactors=F)


d <- read.csv("/Users/cervas/My Drive/GitHub/Data/Elections/Presidential/Pres by State/president_state.csv")
	 d$tptotal <- d$dem+d$rep
	 d$pctREP <- d$rep/d$tptotal
	 d$pctDEM <- d$dem/d$tptotal
pctREP <- data.frame(state=d$state[d$year==2016])
years <- paste0("y", seq(1868,2016,4))
pctREP[years] <- NA
rownames(pctREP) <- pctREP$state
pctREP <- pctREP[,-1]
colnames(pctREP) <- seq(1868,2016,4)
d <- d[d$year>1867,] 

head(d)
for (i in 1:length(unique(d$year))){
	
	d1 <- d[d$year== unique(unique(d$year)[i]),]
	d1 <- d1[order(d1$pctREP),]
	d1$rank <- seq(1,length(d1$state),1)
pctREP[, i] <- match(rownames(pctREP), d1$state)
}




pdf("/Users/jcervas/Google Drive/School/UCI/R Functions/Leeper/state_order_1790_1870.pdf", width=10, height=8)
           
 par(mfrow=c(1,6), oma= c(2,4,0,4), mar=c(3,2,1,2), xpd=T)
          
           par(mar=c(3,10,0,0))
           slopegraph(pctREP[,32:33], ylim = c(51,0), offset.x = 0.00, offset.lab.l= .06,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=NULL, col.num=NULL, mai=c(1,0,0,0))
           
           par(mar=c(3,0,0,0))
           slopegraph(pctREP[,33:34], ylim = c(51,0), offset.x = 0.00, offset.lab.l= 0.65,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=NULL, labpos.left=3, col.num=NULL, mai=c(1,.05,0,0), offset.down=0)
           
                      par(mar=c(3,0,0,0))
           slopegraph(pctREP[,34:35], ylim = c(51,0), offset.x = 0.00, offset.lab.l= 0.65,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=NULL, labpos.left=3, col.num=NULL, mai=c(1,.05,0,0))
           
           par(mar=c(3,0,0,0))
           slopegraph(pctREP[,35:36], ylim = c(51,0), offset.x = 0.00, offset.lab.l= 0.65,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=NULL, labpos.left=3, col.num=NULL, mai=c(1,.05,0,0))
           
                      par(mar=c(3,0,0,0))
           slopegraph(pctREP[,36:37], ylim = c(51,0), offset.x = 0.00, offset.lab.l= 0.65,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=NULL, labpos.left=3, col.num=NULL, mai=c(1,.05,0,0))
           
           par(mar=c(3,0,0,0))
           slopegraph(pctREP[,37:38], ylim = c(51,0), offset.x = 0.00, offset.lab.l= 0.65,
           #col.lines = cols, col.lab = cols, 
           #main = 'Relative Rank of U.S. by Two-Party Vote, 1868-2016', 
           cex.num=0.5, cex.lab=1, x.cex.axis= 0.5, labpos.right=4, labpos.left=3, col.num=NULL, mai=c(1,.05,0,0))
           

           dev.off()
           
           
           
           
           
           