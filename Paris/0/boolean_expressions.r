#
#  This code returns "0" for nodes that have no inbound edges.
#  For instance,
#    A -> B
#  becomes
#    A, 0
#    B, A
#



library(gtools)     
library(digest)     
library(combinat)   

#########################
# 1. Matrix Hash Generator for edges
#########################

get_canonical_matrix_hash <- function(mat) {
  n <- nrow(mat)
  perms <- combinat::permn(n)  
  hash_variants <- sapply(perms, function(p) {
    permuted_mat <- mat[p, p, drop = FALSE]
    paste(as.vector(permuted_mat), collapse = ",")
  })
  return(min(hash_variants))
}

#########################
# 2. Adjacency Matrix Generation 
#########################

generate_adjacency_matrices_hashed <- function(n) {
  total_edges <- n * n
  total_graphs <- 3^total_edges
  cat("Total graphs to generate:", total_graphs, "\n")
  
  
  int_to_base3 <- function(num, len) {
    base3 <- integer(len)
    for (i in 1:len) {
      base3[i] <- num %% 3
      num <- num %/% 3
    }
    return(rev(base3))
  }
  
  # Converts base-3 numbers to signed edge values (0, -1, 1)
  base3_to_signed <- function(vec) {
    sapply(vec, function(x) if (x == 0) 0 else if (x == 1) -1 else 1)
  }
  
  
  canonical_hashes <- new.env(hash = TRUE, parent = emptyenv())
  unique_matrices <- list()
  
  for (i in 0:(total_graphs - 1)) {
    vec <- int_to_base3(i, total_edges)
    adj_vals <- base3_to_signed(vec)
    mat <- matrix(adj_vals, nrow = n, byrow = TRUE)
    
    
    sig <- get_canonical_matrix_hash(mat)
    h <- digest(sig)
    
    if (!(h %in% ls(envir = canonical_hashes))) {
      assign(h, TRUE, envir = canonical_hashes)
      unique_matrices[[length(unique_matrices) + 1]] <- mat
    }
  }
  
  cat("Number of unique canonical matrices:", length(unique_matrices), "\n")
  return(unique_matrices)
}

#########################
# 3. Boolean Expression Generation
#########################

generate_boolean_expressions <- function(adj_matrix) {
  nodes <- colnames(adj_matrix)
  if (is.null(nodes)) {
    nodes <- LETTERS[1:nrow(adj_matrix)]
  }
  
  expressions <- list()
  for (node in nodes) {
    activating_indices <- which(adj_matrix[, node] == 1)
    inhibitory_indices <- which(adj_matrix[, node] == -1)
    
    activating_part <- if (length(activating_indices) > 0) {
      paste(nodes[activating_indices], collapse = " | ")
    } else {
      ""
    }
    
    inhibitory_part <- if (length(inhibitory_indices) > 0) {
      paste(paste0("!", nodes[inhibitory_indices]), collapse = " & ")
    } else {
      ""
    }
    
    if (activating_part != "" && inhibitory_part != "") {
      expr <- paste(activating_part, "&", inhibitory_part)
    } else if (activating_part != "") {
      expr <- activating_part
    } else if (inhibitory_part != "") {
      expr <- inhibitory_part
    } else {
      expr <- "FALSE"
    }
    
    expressions[[node]] <- expr
  }
  
  return(expressions)
}

#########################
# 4. Save Boolean Expressions to a single text file
#########################

save_boolean_expressions_to_file <- function(results, file_name) {
  file_conn <- file(file_name, open = "w")
  for (result in results) {
    expressions <- result$expressions
    for (node in names(expressions)) {
      current_expr <- expressions[[node]]
      # Replace "FALSE" with "0" for nodes with no activations or inhibitions
      if (current_expr == "FALSE") {
        current_expr <- "0"
      }
      line <- paste0(node, ", ", current_expr)
      writeLines(line, file_conn)
    }
    
    writeLines("", file_conn)
  }
  close(file_conn)
  cat("Finished writing to file:", file_name, "\n")
}

#########################
# 5. Main Function: Generate and Save Expressions
#########################
# This function ties everything together:
#   - It generates unique adjacency matrices (using the hashed method)
#   - It assigns node names
#   - It creates Boolean expressions for each matrix
#   - It saves the results to a text file.
generate_and_save_expressions <- function(num_nodes, file_name) {
  adj_matrices <- generate_adjacency_matrices_hashed(num_nodes)
  
  results <- list()
  for (matrix in adj_matrices) {
    nNodes <- nrow(matrix)
    letter_names <- LETTERS[1:nNodes]
    colnames(matrix) <- rownames(matrix) <- letter_names
    
    expressions <- generate_boolean_expressions(matrix)
    results <- append(results, list(list(matrix = matrix, expressions = expressions)))
  }
  
  save_boolean_expressions_to_file(results, file_name)
  return(results)
}



#########################
# 7. Usage Example
#########################
# For a network with two nodes, for example if there's an edge A -> B, the expected output is:
# B, A
# A, 0
# Provide number of nodes in num_nodes
           
results <- generate_and_save_expressions(num_nodes = 2, file_name = "boolean_expressions.txt")
verify_boolean_expressions(results)

