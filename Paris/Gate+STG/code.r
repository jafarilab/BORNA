


# ---------------------------
# Load required packages
# ---------------------------
library(combinat)
library(digest)
library(igraph)
library(BoolNet)
library(grid)    

# ---------------------------
# 1. Define S4 Classes
# ---------------------------
setClass("BooleanNetwork",
         slots = c(
           id          = "numeric",
           adj         = "matrix",
           rules       = "list",
           truth_table = "character",
           gates       = "character",
           attractors  = "ANY",
           plots       = "list"    
         ))

createBooleanNetwork <- function(id, adj, rules, truth_table, gates, attractors = NULL, plots = list()) {
  new("BooleanNetwork", id = id, adj = adj, rules = rules,
      truth_table = truth_table, gates = gates, attractors = attractors, plots = plots)
}

setClass("BooleanNetworkCollection",
         slots = c(
           networks = "list"
         ))

setMethod("[[", "BooleanNetworkCollection", function(x, i, j, ...) {
  networks_list <- x@networks
  idx <- which(sapply(networks_list, function(net) net@id) == i)
  if (length(idx) == 0)
    stop("No network with id ", i)
  return(networks_list[[idx[1]]])
})

# ---------------------------
# New: Accessor for individual BooleanNetwork objects
# ---------------------------
setMethod("[[", "BooleanNetwork", function(x, i, j, ...) {
  if (is.character(i)) {
    return(slot(x, i))
  } else if (is.numeric(i)) {
    slots_vec <- slotNames(x)
    if (i > length(slots_vec))
      stop("Index out of bounds")
    return(slot(x, slots_vec[i]))
  } else {
    stop("Invalid index type. Use a character slot name or numeric index.")
  }
})

# ---------------------------
# 2. Helper Functions for Truth Table and Boolean Expression Generation
# ---------------------------
get_input_combinations <- function(vars) {
  expand.grid(rep(list(c(FALSE, TRUE)), length(vars)), stringsAsFactors = FALSE) |>
    setNames(vars)
}

get_truth_table <- function(expr, vars) {
  inputs <- get_input_combinations(vars)
  apply(inputs, 1, function(row) {
    env <- as.list(row)
    tryCatch(as.logical(eval(parse(text = expr), envir = env)),
             error = function(e) NA)
  })
}

generate_standard_gates <- function(vars) {
  df <- get_input_combinations(vars)
  and_v <- apply(df, 1, all)
  or_v  <- apply(df, 1, any)
  xor_v <- if (length(vars) == 2)
    apply(df, 1, function(r) sum(r) %% 2 == 1) else NULL
  list(
    AND  = and_v,
    OR   = or_v,
    XOR  = xor_v,
    NAND = !and_v,
    NOR  = !or_v,
    XNOR = if (!is.null(xor_v)) !xor_v else NULL
  )
}

identify_gate <- function(expr, vars) {
  tt  <- get_truth_table(expr, vars)
  stds <- generate_standard_gates(vars)
  for (g in names(stds)) {
    if (!is.null(stds[[g]]) && all(tt == stds[[g]]))
      return(g)
  }
  NA
}

normalize_candidates <- function(cands, vars) {
  unique_cands <- character(0)
  candidate_tts <- character(0)
  for (cand in cands) {
    tt <- paste(get_truth_table(cand, vars), collapse = "")
    if (!(tt %in% candidate_tts)) {
      candidate_tts <- c(candidate_tts, tt)
      unique_cands <- c(unique_cands, cand)
    }
  }
  unique_cands
}

# ---------------------------
# 2A. Revised Boolean Expression Generation (Limited to !, &, |)
# ---------------------------
generate_boolean_expressions <- function(adj) {
  n <- nrow(adj)
  nodes <- LETTERS[1:n]
  rownames(adj) <- nodes
  colnames(adj) <- nodes
  out <- list()
  
  for (tgt in nodes) {
    act <- nodes[adj[tgt, ] ==  1]
    inh <- nodes[adj[tgt, ] == -1]
    cands <- character(0)
    
    if (length(act) > 0 && length(inh) > 0) {
      act_expr <- if (length(act) == 1) act else paste(act, collapse = " & ")
      inh_expr <- if (length(inh) == 1) paste0("!", inh) else paste(paste0("!", inh), collapse = " & ")
      cand1 <- paste(act_expr, "&", inh_expr)
      
      act_expr2 <- if (length(act) == 1) act else paste(act, collapse = " | ")
      inh_expr2 <- if (length(inh) == 1) paste0("!", inh) else paste(paste0("!", inh), collapse = " | ")
      cand2 <- paste(inh_expr2, "|", act_expr2)
      cands <- c(cand1, cand2)
      
    } else if (length(act) > 0) {
      if (length(act) == 1) {
        cands <- act
      } else {
        cand1 <- paste(act, collapse = " & ")
        cand2 <- paste(act, collapse = " | ")
        cands <- c(cand1, cand2)
      }
    } else if (length(inh) > 0) {
      if (length(inh) == 1) {
        cands <- paste0("!", inh)
      } else {
        cand1 <- paste(paste0("!", inh), collapse = " & ")
        cand2 <- paste(paste0("!", inh), collapse = " | ")
        cands <- c(cand1, cand2)
      }
    } else {
      cands <- c("0", "1")
    }
    
    uniq <- normalize_candidates(cands, nodes)
    if (length(uniq) == 0) {
      uniq <- c("0", "1")
    }
    out[[tgt]] <- uniq
  }
  out
}

generate_network_variants <- function(expr_list) {
  grid <- expand.grid(expr_list, stringsAsFactors = FALSE)
  apply(grid, 1, function(row) as.list(row))
}

get_state_transitions <- function(rules, vars) {
  inputs <- get_input_combinations(vars)
  apply(inputs, 1, function(row) {
    from <- paste0(as.integer(unlist(row)), collapse = "")
    env <- as.list(row)
    to_vals <- sapply(vars, function(g) {
      as.integer(eval(parse(text = rules[[g]]), envir = env))
    })
    to <- paste0(to_vals, collapse = "")
    paste0(from, " => ", to)
  })
}


get_constant_signature <- function(truth_table) {
  line <- truth_table[1]  # all lines are the same for a constant network
  parts <- strsplit(line, "=>")[[1]]
  if(length(parts) < 2) {
    return(NA)
  }
  out_state <- trimws(parts[2])
  sorted_chars <- paste(sort(strsplit(out_state, "")[[1]]), collapse = "")
  sorted_chars
}


is_degenerate_network <- function(bn) {
  all(bn@adj == 0)
}

# ---------------------------
# 3. Generation of Unique Adjacency Matrices
# ---------------------------
generate_adjacency_matrices_efficient <- function(n) {
  total <- 3^(n * n)
  perms <- combinat::permn(n)
  seen  <- new.env(hash = TRUE)
  mats  <- list()
  
  to_base3 <- function(x, len) {
    v <- integer(len)
    for (i in seq_len(len)) {
      v[i] <- x %% 3
      x <- x %/% 3
    }
    rev(v)
  }
  
  for (i in 0:(total - 1)) {
    b3 <- to_base3(i, n * n)
    mat <- matrix(ifelse(b3 == 2, -1, b3), nrow = n, byrow = TRUE)
    sig <- min(sapply(perms, function(p) paste(as.vector(mat[p, p]), collapse = ",")))
    if (!exists(sig, envir = seen)) {
      assign(sig, TRUE, envir = seen)
      mats[[length(mats) + 1]] <- mat
    }
  }
  cat("DEBUG: Unique adjacency matrices generated:", length(mats), "\n")
  mats
}

# ---------------------------
# 4. Generate Networks
# ---------------------------
generate_networks <- function(num_nodes) {
  vars <- LETTERS[1:num_nodes]
  adj_mats <- generate_adjacency_matrices_efficient(num_nodes)
  all_networks <- list()
  net_id <- 1
  
  for (adj in adj_mats) {
    exprs   <- generate_boolean_expressions(adj)
    variants <- generate_network_variants(exprs)
    
    for (v in variants) {
      trans_list <- get_state_transitions(v, vars)
      gates <- na.omit(unique(sapply(v, function(e) identify_gate(e, vars))))
      gate_str <- if (length(gates) == 0) "NONE" else paste(gates, collapse = ", ")
      
      bn <- createBooleanNetwork(
        id           = net_id,
        adj          = adj,
        rules        = v,
        truth_table  = trans_list,
        gates        = gate_str
      )
      all_networks[[length(all_networks) + 1]] <- bn
      cat("DEBUG: Created network with ID", net_id, "\n")
      net_id <- net_id + 1
    }
  }
  sorted_networks <- all_networks[order(sapply(all_networks, function(net) {
    paste(as.vector(net@adj), collapse = ",")
  }))]
  sorted_networks
}

# ---------------------------
# 5. Compute Attractors and Build STG
# ---------------------------
compute_attractors_from_transitions <- function(transitions) {
  
  transition_map <- list()
  for (line in transitions) {
    parts <- strsplit(line, " => ")[[1]]
    if (length(parts) < 2) next
    state_from <- parts[1]
    state_to <- parts[2]
    transition_map[[state_from]] <- state_to
  }
  
  attractors <- list()
  visited_states <- character(0)
  
  for (start_state in names(transition_map)) {
    if (start_state %in% visited_states)
      next
    path <- character(0)
    current_state <- start_state
    while (!current_state %in% path) {
      path <- c(path, current_state)
      current_state <- transition_map[[current_state]]
    }
    
    cycle_start_index <- match(current_state, path)
    cycle <- path[cycle_start_index:length(path)]
    attractors[[start_state]] <- cycle
    visited_states <- union(visited_states, path)
  }
  attractors
}

compute_attractors_for_network <- function(bn) {
  vars <- names(bn@rules)
  transitions <- get_state_transitions(bn@rules, vars)
  bn@truth_table <- transitions
  
  # If the network has no edges, use manual attractor computation.
  if (is_degenerate_network(bn)) {
    cat("DEBUG: Using manual attractor computation for network with no edges, ID", bn@id, "\n")
    att <- compute_attractors_from_transitions(transitions)
    return(att)
  }
  
  tmp_file <- tempfile(fileext = ".tmp")
  rules_lines <- c("targets, factors")
  for (node in names(bn@rules)) {
    line <- paste0(node, ", ", bn@rules[[node]])
    rules_lines <- c(rules_lines, line)
  }
  writeLines(rules_lines, con = tmp_file)
  
  bn_boolnet <- loadNetwork(tmp_file)
  att <- tryCatch({
    getAttractors(bn_boolnet, type = "synchronous", method = "exhaustive", returnTable = TRUE)
  }, error = function(e) {
    cat("DEBUG: Error in getAttractors for network ID", bn@id, ": ", e$message, "\n")
    compute_attractors_from_transitions(transitions)
  })
  unlink(tmp_file)
  cat("DEBUG: Attractor info for network ID", bn@id, ":", class(att), "\n")
  att
}

# ---------------------------
# Modified STG Graph Construction
# ---------------------------
compute_STG_graph <- function(att, fallback_transitions = NULL) {
  stg <- tryCatch({
    tt_text <- suppressWarnings(capture.output(getTransitionTable(att)))
    transitions <- grep("=>", tt_text, value = TRUE)
    transitions
  }, error = function(e) {
    NULL
  })
  
  if (is.null(stg) && !is.null(fallback_transitions)) {
    transitions <- fallback_transitions
  }
  
  if (is.null(transitions)) {
    return(NULL)
  }
  
  edges_list <- lapply(transitions, function(line) {
    parts <- strsplit(line, "=>")[[1]]
    if (length(parts) < 2) return(NULL)
    from_state <- trimws(parts[1])
    tokens <- strsplit(trimws(parts[2]), "\\s+")[[1]]
    if (length(tokens) < 1) return(NULL)
    to_state <- tokens[1]
    c(from = from_state, to = to_state)
  })
  edges_list <- Filter(Negate(is.null), edges_list)
  if (length(edges_list) == 0) return(NULL)
  edges_mat <- do.call(rbind, edges_list)
  edges_df <- as.data.frame(edges_mat, stringsAsFactors = FALSE)
  edges_df <- edges_df[edges_df$from != "" & edges_df$to != "", ]
  if (nrow(edges_df) == 0) return(NULL)
  g <- graph_from_data_frame(edges_df, directed = TRUE)
  if (vcount(g) == 0) return(NULL)
  g
}

# ---------------------------
# 6. Generate PDF
# ---------------------------
generate_pdf_report <- function(networks, pdf_file) {
  pdf(pdf_file, height = 11, width = 8.5)
  
  # Group networks by their full adjacency matrix.
  groups <- split(networks, sapply(networks, function(net) paste(as.vector(net@adj), collapse = ",")))
  
  # Loop over each group (each unique adjacency matrix)
  for (group_key in names(groups)) {
    group_networks <- groups[[group_key]]
    
    # If this group represents a degenerate network (no edges), filter duplicate constant outputs.
    if (all(as.numeric(unlist(strsplit(group_key, ","))) == 0)) {
      signatures <- sapply(group_networks, function(bn) get_constant_signature(bn@truth_table))
      unique_signatures <- unique(signatures)
      filtered_networks <- list()
      for (sig in unique_signatures) {
        idx <- which(signatures == sig)[1]
        filtered_networks[[length(filtered_networks) + 1]] <- group_networks[[idx]]
      }
      group_networks <- filtered_networks
      cat("DEBUG: In degenerate group, reduced", length(signatures), "variants to", length(group_networks), "\n")
    }
    
    # Use the first network to build the common structure graph.
    bn_first <- group_networks[[1]]
    g_net <- graph_from_adjacency_matrix(
      t(bn_first@adj),  # Transpose: regulators as incoming nodes.
      mode     = "directed",
      weighted = TRUE,
      diag     = TRUE
    )
    V(g_net)$name <- as.character(seq_len(vcount(g_net)))
    orig_weights <- E(g_net)$weight
    if (length(orig_weights) > 0) {
      E(g_net)$weight <- abs(orig_weights)
      E(g_net)$color <- ifelse(orig_weights == 1, "green", ifelse(orig_weights == -1, "red", "black"))
    }
    bn_first@plots[["igraph"]] <- g_net
    
    num_variants <- length(group_networks)
    layout_mat <- rbind(c(1, 1))
    for (i in 1:num_variants) {
      layout_mat <- rbind(layout_mat, c(2*i, 2*i + 1))
    }
    layout(layout_mat, widths = c(0.4, 0.6), heights = c(1.5, rep(3, num_variants)))
    
    par(mar = c(2, 2, 2, 2))
    common_adj_text <- paste(apply(bn_first@adj, 1, function(row) paste(row, collapse = " ")), collapse = "\n")
    plot(g_net,
         main = sprintf("Common Structure\nAdjacency Matrix:\n%s", common_adj_text),
         vertex.size = 30,
         vertex.label = V(g_net)$name,
         vertex.label.cex = 1,
         edge.curved = 0.3)
    
    for (i in seq_along(group_networks)) {
      bn <- group_networks[[i]]
      
      att <- compute_attractors_for_network(bn)
      bn@attractors <- att
      
      stg_graph <- compute_STG_graph(att, fallback_transitions = bn@truth_table)
      
      par(mar = c(2, 2, 2, 2))
      if (is.null(stg_graph) || vcount(stg_graph) == 0) {
        plot.new()
        text(0.5, 0.5, sprintf("Network %d\n(No STG)", bn@id), cex = 0.8)
      } else {
        plot(stg_graph,
             main = sprintf("Network %d STG", bn@id),
             vertex.shape = "circle",
             vertex.size = 30,
             vertex.color = "lightblue",
             vertex.label.color = "black",
             edge.arrow.size = 0.5)
      }
      
      par(mar = c(2, 2, 2, 2))
      plot.new()
      info_lines <- c(
        sprintf("Network %d (GATES: %s)", bn@id, bn@gates),
        "Boolean Update Rules:"
      )
      for (node in names(bn@rules)) {
        info_lines <- c(info_lines, sprintf("  %s: %s", node, bn@rules[[node]]))
      }
      info_lines <- c(info_lines, "Truth Table:")
      info_lines <- c(info_lines, bn@truth_table)
      text(0.5, 0.5, paste(info_lines, collapse = "\n"), cex = 0.6, adj = c(0.5, 0.5))
    }
  }
  
  dev.off()
  cat("DEBUG: PDF report written to", pdf_file, "\n")
}

# ---------------------------
# 7. TXT Output Function
# ---------------------------
write_txt_output <- function(networks, file_name) {
  con <- file(file_name, open = "w")
  
  total_networks <- length(networks)
  writeLines(sprintf("Total networks present: %d", total_networks), con)
  
  for (bn in networks) {
    writeLines(sprintf("\n\nNetwork %d (GATES: %s)", bn@id, bn@gates), con)
    writeLines("Adjacency Matrix:", con)
    apply(bn@adj, 1, function(row) writeLines(paste("  ", paste(row, collapse = "\t")), con))
    
    writeLines("Boolean Update Rules:", con)
    for (node in names(bn@rules)) {
      writeLines(sprintf("  %s: %s", node, bn@rules[[node]]), con)
    }
    
    writeLines("Truth Table:", con)
    for (line in bn@truth_table) {
      writeLines(paste("  ", line), con)
    }
    
    if (!is.null(bn@attractors)) {
      writeLines("Attractors:", con)
      att_text <- capture.output(print(bn@attractors))
      for (ln in att_text) {
        writeLines(paste("  ", ln), con)
      }
    }
  }
  
  close(con)
  cat("DEBUG: TXT report written to", file_name, "\n")
}

# ---------------------------
# 8. Main Execution
# ---------------------------

##  ##  ##  ##   provide number of nodes
num_nodes <- 2  # Adjust as needed
txt_file <- "network_truth_tables.txt"
pdf_file <- "network_report_with_STG---grouped-.pdf"

networks <- generate_networks(num_nodes)

write_txt_output(networks, txt_file)
generate_pdf_report(networks, pdf_file)


names(networks) <- sapply(networks, function(net) net@id)
networks_collection <- new("BooleanNetworkCollection", networks = networks)
