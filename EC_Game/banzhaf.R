banzhaf <- function(member, weights, quota) {
  n <- length(weights)  # Total number of voters (or members)
  swingsPerIndex <- numeric(n)  # Initialize a vector to count swing votes for each member
  
  # Iterate through all possible coalition sizes (from 1 to n)
  for (setSize in 1:n) {
    # Generate all combinations of members of size 'setSize'
    sets <- utils::combn(n, setSize, simplify = TRUE)
    
    # If the combination function returns a vector (setSize == 1), convert it to a 1-row matrix
    if (setSize == 1) {
      sets <- matrix(sets, nrow = 1)
    }
    
    numSets <- ncol(sets)  # Total number of such coalitions
    
    # Calculate the total voting weight for each coalition
    totals <- colSums(matrix(weights[sets], nrow = setSize))
    
    # Identify which coalitions meet or exceed the quota (i.e., winning coalitions)
    winners <- which(totals >= quota)
    
    # For each winning coalition, check which members are swing voters
    for (w in winners) {
      coalition <- sets[, w]        # Get the indices of members in the current coalition
      totalVotes <- totals[w]       # Get the total weight of that coalition
      
      for (i in coalition) {
        # If removing member i makes the coalition lose (i.e., fall below the quota),
        # then i is a swing voter in this coalition
        if ((totalVotes - weights[i]) < quota) {
          swingsPerIndex[i] <- swingsPerIndex[i] + 1  # Count the swing
        }
      }
    }
  }
  
  # Normalize swing counts to get Banzhaf power index for each member
  return(data.frame(name = member, score = swingsPerIndex / sum(swingsPerIndex)))
}

  # Example
  ## banzhaf(
            # member = c("A","B","C","D","E","F","G"), 
            # weights = c(3,5,8,13,21,34,55), 
            # quota = 70)
