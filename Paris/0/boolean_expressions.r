#
#  This code returns "0" for nodes that have no inbound edges.
#  For instance,
#    A -> B
#  becomes
#    A, 0
#    B, A
#

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
  possible_values <- c(-1, 0, 1) # -1 for inhibitory,  0 for no connections, 1 for activating
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
# 3. Boolean Expression
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
# 4. Save to File (Modified)
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
# 5. Main
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
# 6. Boolean Expression Verifier
#########################
verify_boolean_expressions <- function(results) {
  expression_set <- new.env(hash = TRUE)
  syntax_errors <- 0
  duplicates <- 0
  
  has_syntax_error <- function(expr) {
    tryCatch({
      parse(text = expr)
      FALSE
    }, error = function(e) TRUE)
  }
  
  for (result in results) {
    exprs <- unlist(result$expressions)
    expr_string <- paste(exprs, collapse = ";")
    
    # Check for duplicates
    if (exists(expr_string, envir = expression_set)) {
      duplicates <- duplicates + 1
    } else {
      assign(expr_string, TRUE, envir = expression_set)
    }
    
    # Check syntax
    if (any(sapply(exprs, has_syntax_error))) {
      cat("Invalid R parse in expression:", paste(exprs, collapse = ", "), "\n")
      syntax_errors <- syntax_errors + 1
    }
  }
  
  cat("=== Verification Summary ===\n")
  cat("Total networks checked:", length(results), "\n")
  cat("Duplicates found:", duplicates, "\n")
  cat("Syntax errors found:", syntax_errors, "\n")
  cat("============================\n")
}

#########################
# 7. Usage
#########################
# Example: For a network with two nodes, if there's an edge A -> B then the expected output is:
# B, A
# A, 0
results <- generate_and_save_expressions(num_nodes = 2, file_name = "boolean_expressions.txt")
verify_boolean_expressions(results)
