# Boolean Network Analysis Script
# 
# Provide number of nodes at the last line, num_nodes
#
#


library(BoolNet) 
library(gtools)   

#########################
# 1. Matrix Hash
#########################
# This function generates a hash for a given adjacency matrix to uniquely identify it.

matrix_hash <- function(mat) {
  n     <- nrow(mat)
  perms <- gtools::permutations(n, n, 1:n) 
  hashes <- apply(perms, 1, function(p) {
    paste(mat[p, p], collapse = ",") 
  })
  min(hashes) 
}

#########################
# 2. Adjacency Matrices (Unique)
#########################
# This function generates all possible adjacency matrices for the ***given number of nodes*** by the user,
# filters out duplicate matrices using `matrix_hash`, and returns only unique matrices.
adjmat <- function(n) {
  vals   <- c(-1, 0, 1)              # -1 (inhibitory), 0 (no connection), 1 (activating)
  combos <- expand.grid(rep(list(vals), n * n))  
  mats   <- lapply(seq_len(nrow(combos)), function(i) 
    matrix(as.numeric(combos[i, ]), nrow = n, byrow = FALSE) 
  )
  hashes <- vapply(mats, matrix_hash, character(1)) 
  mats[!duplicated(hashes)]
}

#########################
# 3. Boolean Expression Generation
#########################

to_bool <- function(adj_matrix) {
  nodes <- colnames(adj_matrix) 
  if (is.null(nodes)) nodes <- LETTERS[1:nrow(adj_matrix)] 

  lapply(nodes, function(node) {
    act <- nodes[adj_matrix[, node] == 1] 
    inh <- nodes[adj_matrix[, node] == -1] 
    act_str <- if (length(act)) paste(act, collapse = " | ") else ""  
    inh_str <- if (length(inh)) paste(paste0("!", inh), collapse = " & ") else ""
    if (nzchar(act_str) && nzchar(inh_str)) {
      paste(act_str, "&", inh_str) 
    } else if (nzchar(act_str)) {
      act_str 
    } else if (nzchar(inh_str)) {
      inh_str  
    } else {
      "FALSE"  
    }
  }) -> exprs
  names(exprs) <- nodes  
  exprs  
}

#########################
# 4. Network Builder
#########################
# Generates a Boolean network
# If a node has no connectios, it creates a self-loop
net_generator <- function(expressions, path) {
  rules <- vapply(names(expressions), function(g) {
    expr <- expressions[[g]]
    if (expr == "FALSE")
      paste0(g, ", ", g, " & ", g)  
    else
      paste0(g, ", ", expr)  
  }, character(1))

  writeLines(c("targets, functions", rules), con = path)  
}

#########################
# 5. Network Analysis Usign BoolNet
#########################

analyze_network <- function(file_path) {
  net <- loadNetwork(file_path, bodySeparator = ",")  
  attr <- getAttractors(net, type = "synchronous", returnTable = TRUE)  
  list(
    attractors = attr$attractors,  
    transitionTable = getTransitionTable(attr)  
  )
}

#########################
# 6. Main
#########################

process_networks <- function(num_nodes,
                             output_dir = "network_rules",
                             log_file   = "output.txt") {
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)  
  sink(log_file, split = TRUE)  
  on.exit(sink())  

  mats <- adjmat(num_nodes)  
  results <- vector("list", length(mats))  

  for (i in seq_along(mats)) {
    mat <- mats[[i]]
    nn <- nrow(mat)
    names <- LETTERS[1:nn] 
    dimnames(mat) <- list(names, names)

    exprs <- to_bool(mat)  
    file_path <- file.path(output_dir, paste0("network_", i, ".txt"))  
    net_generator(exprs, file_path)  

    res <- analyze_network(file_path)  
    results[[i]] <- list(
      matrix  = mat,
      expressions = exprs,
      attractors  = res$attractors,
      transition_table = res$transitionTable
    )

    
    cat(sprintf("\n=== Network %d ===\n", i))
    cat("Rules:\n")
    for (line in readLines(file_path)[-1]) cat(line, "\n")
    cat("Attractors:\n"); print(res$attractors)
    cat("Transition Table:\n"); print(res$transitionTable)
    cat(strrep("-", 40), "\n")  
  }

  sink()
  cat("Log file written to:", log_file, "\n")
  invisible(results)  
}

#########################
# 7. Usage
#########################
# ***** Provide number of nodes *****
results <- process_networks(num_nodes = 2) 
