OpaVote <- function(opa="/Users/cervas/My Drive/GitHub/R-Functions/OpaVote/data/opa/2023-05-16-voted.txt", amberson="/Users/cervas/My Drive/GitHub/R-Functions/OpaVote/data/2023 Council Voter List - List for EC.csv") {

emails <- read.csv(amberson)
opa <- read.csv(opa, header=F)
return(
     list(
          emails$Unit[emails$Email %in% opa[,1]],
          emails$Unit[!emails$Email %in% opa[,1]])
)
}
 