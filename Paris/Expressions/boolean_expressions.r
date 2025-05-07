# ==============================================================================
# 92 network for 2 nodes and separate txt files
# ==============================================================================

library(gtools)  





#########################
# 1. Hash Generator
#########################


get_canonical_matrix_hash <- function(mat) {
  n <- nrow(mat)                       
  perms <- permutations(n = n, r = n, v = 1:n)  
  
  hash_variants <- apply(perms, 1, function(p) {
    permuted_mat <- mat[p, p]          
    paste(permuted_mat, collapse = ",") 
  })
  
  return(min(hash_variants))            
}





#########################
# 2. Adjacency Matrices
#########################

generate_adjacency_matrices_efficient <- function(n) {
  possible_values <- c(-1, 0, 1)  
  num_cells <- n * n              
  all_combinations <- expand.grid(rep(list(possible_values), num_cells))  
  
  canonical_hashes <- new.env(hash = TRUE, parent = emptyenv())  
  unique_matrices <- list()  
  
  
  for (i in seq_len(nrow(all_combinations))) {
    matrix_data <- matrix(as.numeric(all_combinations[i, ]), nrow = n, ncol = n)
    canonical_hash <- get_canonical_matrix_hash(matrix_data)
    
    
    if (!exists(canonical_hash, envir = canonical_hashes)) {
      assign(canonical_hash, TRUE, envir = canonical_hashes)
      unique_matrices <- append(unique_matrices, list(matrix_data))
    }
  }
  
  cat("Number of unique canonical matrices:", length(unique_matrices), "\n")
  return(unique_matrices)  
}



#########################
# 3. Boolean Expression Generator (All Possible Variations)
#########################

generate_boolean_expressions <- function(adj_matrix) {
  nodes <- colnames(adj_matrix)
  if (is.null(nodes)) {
    nodes <- LETTERS[1:nrow(adj_matrix)]  
  }
  
  
  if (all(adj_matrix == 1)) {
    expressions <- list()
    expr_str <- paste(nodes, collapse = " | ")
    for (node in nodes) {
      expressions[[node]] <- expr_str
    }
    return(expressions)
  }
  
  expressions <- list()
  
  
  for (node in nodes) {
    
    act_idx <- which(adj_matrix[, node] == 1)
    inh_idx <- which(adj_matrix[, node] == -1)
    
    
    act_variants <- c()
    if (length(act_idx) > 0) {
      if (length(act_idx) == 1) {  
        act_variants <- nodes[act_idx]
      } else {
        
        act_variants <- c(paste(nodes[act_idx], collapse = " & "),
                          paste(nodes[act_idx], collapse = " | "))
      }
    }
    

    inh_variants <- c()
    if (length(inh_idx) > 0) {
      inhibited <- paste0("!", nodes[inh_idx])
      if (length(inh_idx) == 1) {
        inh_variants <- inhibited
      } else {
        inh_variants <- c(paste(inhibited, collapse = " & "),
                          paste(inhibited, collapse = " | "))
      }
    }
    
    
    candidates <- c()
    if (length(act_variants) > 0 && length(inh_variants) > 0) {
      
      for (a in act_variants) {
        for (i in inh_variants) {
          candidates <- c(candidates,
                          paste(a, "&", i),
                          paste(a, "|", i))
        }
      }
    } else if (length(act_variants) > 0) {
      candidates <- act_variants
    } else if (length(inh_variants) > 0) {
      candidates <- inh_variants
    } else {
      candidates <- "FALSE" 
    }
    
    
    expressions[[node]] <- unique(candidates)
  }
  
  return(expressions)  
}





#########################
# 3.5 Generate Complete Network Variants
#########################

generate_network_variants <- function(expr_list) {
 
  grid <- expand.grid(expr_list, stringsAsFactors = FALSE)
  
  variants <- apply(grid, 1, function(row) { as.list(row) })
  return(variants)
}



#########################
# 4. Save to Separate Files with Header
#########################

save_networks_to_files <- function(results) {
  network_index <- 1  
  
  for (result in results) {
    expr_variations <- result$expressions   
    network_variants <- generate_network_variants(expr_variations)
    
    
    for (variant in network_variants) {
      file_name <- paste0("network_", network_index, ".txt")  
      file_conn <- file(file_name, open = "w")  
      
      
      writeLines("targets, factors", file_conn)
      
      
      for (node in names(variant)) {
        current_expr <- variant[[node]]
        
        if (current_expr == "FALSE") {
          current_expr <- "0"
        }
        line <- paste0(node, ", ", current_expr)
        writeLines(line, file_conn)
      }
      
      writeLines("", file_conn)  
      close(file_conn)  
      network_index <- network_index + 1  
    }
  }
  cat("Finished writing all networks to separate files with headers.\n")
}



#########################
# 5. Main Function        
#########################
# This function integrates the generation of adjacency matrices and Boolean 
# expression variants, then saves each complete network variant to separate files.


generate_and_save_expressions <- function(num_nodes) {
  
  adj_matrices <- generate_adjacency_matrices_efficient(num_nodes)
  
  results <- list()  
  for (matrix in adj_matrices) {
    nNodes <- nrow(matrix)
    letter_names <- LETTERS[1:nNodes]  
    colnames(matrix) <- rownames(matrix) <- letter_names  
    
    
    expressions <- generate_boolean_expressions(matrix)
    results <- append(results, list(list(matrix = matrix, expressions = expressions)))
  }
  
  
  save_networks_to_files(results)
  return(results)
}



#########################
# 7. Usage
#########################
# 
# This will generate the networks and create separate text files for each one.
results <- generate_and_save_expressions(num_nodes = 2)
verify_boolean_expressions(results)
