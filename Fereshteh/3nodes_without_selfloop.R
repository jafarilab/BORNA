
# ====================================================================================================
# My script for Network consist of 3 nodes and without self loop
# ====================================================================================================
#
library(BoolNet)

nodes <- c("A", "B", "C")

generate_logic <- function(target, inputs) {
  stopifnot(length(inputs) == 2)  # e.g., B and C for A
  
  ops <- c("&", "|")
  nots <- c("", "!")
  rules <- c()
  
  for (op in ops) {
    for (ni in nots) {
      for (nj in nots) {
        rule <- paste0(target, ", (", ni, inputs[1], " ", op, " ", nj, inputs[2], ")")
        rules <- c(rules, rule)
      }
    }
  }
  
  return(rules)
}





logicA <- generate_logic("A", c("B", "C"))
logicB <- generate_logic("B", c("A", "C"))
logicC <- generate_logic("C", c("A", "B"))


all_networks <- list()
network_id <- 1
all_results <- list()


for (ra in logicA) {
  for (rb in logicB) {
    for (rc in logicC) {
      rules <- c("targets, factors", ra, rb, rc)
      
      
      tmpfile <- tempfile(fileext = ".txt")
      writeLines(rules, tmpfile)
      
      try({
        net <- loadNetwork(tmpfile)
        attractors <- getAttractors(net, type = "synchronous")
        transitions <- getTransitionTable(attractors)
        
        cat(paste0("\n Network ", network_id, ":\n"))
        cat("Rules:\n", ra, "\n", rb, "\n", rc, "\n\n")
        
        cat(" Attractors:\n")
        print(attractors$attractors)
        
        cat(" Transition Table:\n")
        print(transitions)
        cat("--------------------------------------------------\n\n")
        
        all_results[[network_id]] <- list(
          rules = rules,
          attractors = attractors,
          transitions = transitions
        )
        #
        
        network_id <- network_id + 1
      }, silent = TRUE)
    }
  }
}
# ____________________________________________________________________________
# For my Test
# _________________________________________________________________
all_results[[500]]
atr = all_results[[500]]$attractors
plotAttractors(atr)

net_1 = all_results[[500]]$rules
# Write the rules to a temporary file
tmpfile <- tempfile(fileext = ".txt")
writeLines(net_1, tmpfile)

# Load the network using BoolNet
library(BoolNet)
net_1 <- loadNetwork(tmpfile)

# Now, plot the network wiring
plotNetworkWiring(net_1)
# ____________________________________________________________________________
# _________________________________________________________________

