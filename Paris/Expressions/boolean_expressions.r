
# this code generates Boolean rules for the Adjacency Matrices
# 
# all variations are generated
# 
# 


library(combinat)  
library(digest)    

#########################
# 1. Adjacency Matrices Generation
#########################
generate_adjacency_matrices_efficient <- function(n) {
  total_edges <- n * n
  total_graphs <- 3 ^ total_edges
  cat("Total graphs to generate:", total_graphs, "\n")
  
  # Function to map base-3 digits to allowed signed values: 0 -> 0, 1 -> 1, 2 -> -1
  base3_to_signed <- function(vec) {
    sapply(vec, function(x) if (x == 0) 0 else if (x == 1) 1 else -1)
  }
  
  
  int_to_base3 <- function(num, len) {
    base3 <- integer(len)
    for (i in 1:len) {
      base3[i] <- num %% 3
      num <- num %/% 3
    }
    return(rev(base3))
  }
  
  
  perms <- combinat::permn(n)
  
  
  canonical_signature <- function(mat, perms) {
    sigs <- sapply(perms, function(p) {
      permuted_mat <- mat[p, p, drop = FALSE]
      paste(as.vector(permuted_mat), collapse = ",")
    })
    return(min(sigs))
  }
  
  
  canonical_hashes <- new.env(hash = TRUE, parent = emptyenv())
  unique_matrices <- list()
  
  for (i in 0:(total_graphs - 1)) {
    vec <- int_to_base3(i, total_edges)
    adj_vals <- base3_to_signed(vec)
    mat <- matrix(adj_vals, nrow = n, byrow = TRUE)
    
    sig <- canonical_signature(mat, perms)
    h <- digest::digest(sig)
    
    if (!exists(h, envir = canonical_hashes)) {
      assign(h, TRUE, envir = canonical_hashes)
      unique_matrices[[length(unique_matrices) + 1]] <- mat
    }
  }
  
  cat("Number of unique canonical matrices:", length(unique_matrices), "\n")
  return(unique_matrices)
}

#########################
# 2. Boolean Expression Generator
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
          candidates <- c(candidates, paste(a, "&", i), paste(a, "|", i))
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
# 3. Generate Complete Network Variants
#########################
generate_network_variants <- function(expr_list) {
  grid <- expand.grid(expr_list, stringsAsFactors = FALSE)
  variants <- apply(grid, 1, function(row) { as.list(row) })
  return(variants)
}

#########################
# 4. Save Boolean Expressions to File (Numbered Networks)
#########################
save_boolean_expressions_to_file <- function(results, file_name) {
  file_conn <- file(file_name, open = "w")
  network_index <- 1
  
  for (result in results) {
    expr_variations <- result$expressions
    network_variants <- generate_network_variants(expr_variations)
    
    for (variant in network_variants) {
      writeLines(paste("Network", network_index, ":"), file_conn)
      for (node in names(variant)) {
        current_expr <- variant[[node]]
        if (current_expr == "FALSE") {
          current_expr <- "0"
        }
        line <- paste0(node, ", ", current_expr)
        writeLines(line, file_conn)
      }
      writeLines("", file_conn)  
      network_index <- network_index + 1
    }
  }
  close(file_conn)
  cat("Finished writing all variations to file:", file_name, "\n")
}

#########################
# 5. Generating and Saving Boolean Expressions
#########################
generate_and_save_expressions <- function(num_nodes, file_name) {
  
  adj_matrices <- generate_adjacency_matrices_efficient(num_nodes)
  
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
# 7. Usage
#########################
# 
# produces Boolean expressions for each, writes them to a text file
# provide number of nodes in num_nodes

results <- generate_and_save_expressions(num_nodes = 2, file_name = "boolean_expressions.txt")

