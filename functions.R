# # Bootstrapping node selection ---- NOT SURE IF I NEED SO COMMENTING OUT FOR NOW
# packages <- c(
#   "papaja", 
#   "tidyverse", 
#   "ftExtra", 
#   "janitor",
#   "skimr",
#   "patchwork",
#   "ggpubr",
#   "GGally",
#   "wesanderson",
#   "huxtable",
#   "qgraph",
#   "igraph",
#   "bootnet",
#   "boot",
#   "NetworkComparisonTest",
#   "here"
# )
# lapply(packages, require, character.only = TRUE)

# Descriptives
get_demos <- function(data, variable, name, ...) {
  data <- data %>%
    distinct(id, !!sym(variable)) %>% 
    tabyl(var1 = {{variable}}, show_na = TRUE, show_missing_levels = FALSE, ...)  %>% 
    arrange(desc(percent))
  data <- data %>%
    mutate(
      percent = format(round(percent * 100, 1), nsmall = 1),
      n = paste(n, " (", percent, ")", sep = ""),
      n = str_replace_all(n, fixed("( "), "(")
    ) %>%
    select({{variable}}, "N (%)" = n) %>% 
    mutate(" " = NA)
  data[, {{variable}}] <- ifelse(is.na(data[, {{variable}}]), "Missing", data[, {{variable}}])
  names(data)[1] <- name
  data %>%
    huxtable()
}
inline_hist_ <- function(x){
  inline_hist(x, 7)
}
my_skim <- skim_with(numeric = sfl(hist = inline_hist_))

# Bootstrapping Functions ----------------------------------------------------------------
## For model accuracy
### Get CIs
get_CIs <- function(booted, stat, conf_level) { 
  booted %>% 
    filter(type == stat) %>%
    group_by(node1, node2) %>%
    reframe(statistic = quantile(value, probs = conf_level)) %>%
    mutate(quantile = rep(conf_level, nrow(.) / 2)) %>% 
    pivot_wider(names_from = quantile, values_from = statistic) %>%
    select(node1, node2, conf_lower = as.character(conf_level[1]), conf_upper = as.character(conf_level[2]))
}
### Centrality
boot_centrality_func <- function(data, ind){
  data <- data[ind, ]
  data <- unnest(data, cols = c(data))
  data1 <- data %>% 
    filter(issue == "Smoking") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  data2 <- data %>% 
    filter(issue == "Guns") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  net1 <- estimateNetwork(data1, default = "EBICglasso", corMethod = "spearman")
  cent1 <- centrality(net1, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE)
  net2 <- estimateNetwork(data2, default = "EBICglasso", corMethod = "spearman")
  cent2 <- centrality(net2, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE)
  diffs_in_influence <- cent1$InExpectedInfluence - cent2$InExpectedInfluence
  return(diffs_in_influence)
}
### Edges
boot_edges_func <- function(data, ind){ 
  data <- data[ind, ]
  data <- unnest(data, cols = everything())
  data1 <- data %>%
    filter(issue == "Smoking") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  data2 <- data %>%
    filter(issue == "Guns") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  net1 <- estimateNetwork(data1, default = "EBICglasso", corMethod = "spearman")
  edges1 <- net1$graph
  net2 <- estimateNetwork(data2, default = "EBICglasso", corMethod = "spearman")
  edges2 <- net2$graph
  diffs_in_edges <- edges1 - edges2
  return(diffs_in_edges)
}
### Shortest Path Lengths
boot_path_len_func <- function(data, ind){ 
  # Get resamples by cluster
  data <- data[ind, ]
  data <- unnest(data, cols = everything())
  data1 <- data %>%
    filter(issue == "Smoking") %>% 
    select(Anger:Policy)
  data2 <- data %>%
    filter(issue == "Guns") %>% 
    select(Anger:Policy) 
  # fit networks
  net1 <- estimateNetwork(data1, default = "EBICglasso", corMethod = "spearman")
  net2 <- estimateNetwork(data2, default = "EBICglasso", corMethod = "spearman")
  # get shortest paths
  paths1 <- centrality(net1, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE)$ShortestPathLengths
  paths2 <- centrality(net2, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE)$ShortestPathLengths
  # limit to focal comparisons
  paths1 <- paths1[rownames(paths1) != "Policy", ncol(paths1)]
  paths2 <- paths2[rownames(paths2) != "Policy", ncol(paths2)]
  # replace Infs and NAs with 0's
  names_1 <- names(paths1)
  names_2 <- names(paths2)
  replace_bad_paths <- function(data, names){
    data <- case_when(data == Inf | data == -Inf ~ 0, is.na(data) ~ 0, TRUE ~ as.numeric(data))
    names(data) <- names
    return(data)
  }
  paths1 <- replace_bad_paths(paths1, names_1)
  paths2 <- replace_bad_paths(paths2, names_2)
  # get the differences in paths
  diffs_in_paths <- paths1 - paths2
  return(diffs_in_paths)
}
### Global Strength Invariance Test
boot_global_strength_func <- function(data, ind){
  data <- data[ind, ]
  data <- unnest(data, cols = everything())
  data1 <- data %>%
    filter(issue == "Smoking") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  data2 <- data %>%
    filter(issue == "Guns") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  net1 <- estimateNetwork(data1, default = "EBICglasso", corMethod = "spearman")$graph
  net2 <- estimateNetwork(data2, default = "EBICglasso", corMethod = "spearman")$graph
  net1[lower.tri(net1)] <- NA
  net2[lower.tri(net2)] <- NA
  diffs_in_global_strength <- abs(sum(net1 - net2, na.rm = TRUE))
  return(diffs_in_global_strength)
}
### Max Edge Difference (Global Structure Invariance) ----
boot_max_edge_func <- function(data, ind){
  data <- data[ind, ]
  data <- unnest(data, cols = everything())
  data1 <- data %>%
    filter(issue == "Smoking") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  data2 <- data %>%
    filter(issue == "Guns") %>% 
    select(Anger:Policy) %>% 
    mutate(across(everything(), ~ as.numeric(as.character(.x)))) 
  net1 <- estimateNetwork(data1, default = "EBICglasso", corMethod = "spearman")
  net2 <- estimateNetwork(data2, default = "EBICglasso", corMethod = "spearman")
  diffs_in_edges <- max(abs(c(net1$graph) - c(net2$graph)))
  return(diffs_in_edges)
}
### Test consistency between bootstrap and NCT
test_consistency_bs_nct <- function(data, n_tests) {
  data %>% 
    rename(p = p.value) %>% 
    mutate(
      alpha = 0.05 / n_tests,
      "Sig p?" = ifelse(p < alpha, "Yes", "No"),
      prod = conf.low * conf.high,
      "Sig CI?" = ifelse(prod > 0, "Yes", "No"),
      "Results Agree?" = ifelse(`Sig p?` == `Sig CI?`, "Yes", "No"),
      p = scales::pvalue(p)
    ) %>% 
    select(-prod, -`Sig p?`, -`Sig CI?`)
}
### Function to format path lengths
get_paths <- function(data, net){ 
  data %>% 
    as_tibble(rownames = "From") %>% 
    pivot_longer(
      cols = -From, names_to = "Node2", values_to = paste("Length to Policy", net)
      ) %>%
    filter(
      !str_detect(From, "Policy"), str_detect(Node2, "Policy")
    ) %>% 
    select(-Node2)
}


## Bootstrap Sensitivity Analysus ----
bootstrap_minus_n <- function(
    dataset, n_replicates, nodes, full_net, n_minus = 1, focal_node = NULL, ...
    ) {
  
  # function to extract statistics from each bootstrapped network ----
  get_edges <- function(data = dataset, ...) {
    # sample from "population" of nodes
    sample_nodes <- function(nodes_in_data, n_nodes_to_subtract,...) {
      sample_nodes <- c(sample(
        nodes_in_data[-sample(1:length(nodes_in_data), size = n_nodes_to_subtract)], 
        replace = FALSE
      ))
      return(sample_nodes)
    }
    nodes_sampled <- sample_nodes(nodes_in_data = nodes, n_nodes_to_subtract = n_minus)
    # sample from Ps
    Ps_sampled <- sample(x = 1:nrow(dataset), size = nrow(dataset), replace = TRUE)
    # fit and return the network statistics
    dataset <- dataset %>% 
      slice(Ps_sampled) %>% 
      select(all_of(nodes_sampled)) %>% 
      as.matrix()
    bootedges <- suppressMessages(estimateNetwork(dataset, default = "EBICglasso"))$graph
    centrality <- centrality(
      bootedges, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE
      )
    expected_influence <- centrality$InExpectedInfluence
    shortest_paths <- centrality$ShortestPathLengths
    # format output
    ## add missing columns caused from randomly subtracting nodes
    add_missing_columns <- function(edges_or_shortest_paths, ...){ 
      # add column to the matrix
      add_col <- matrix(c( rep(rep(NA, nrow(edges_or_shortest_paths)), n_minus)), ncol = n_minus)
      edges_or_shortest_paths <- cbind(edges_or_shortest_paths, add_col)
      # add row to the matrix
      add_row <- matrix(
        c(rep(rep(NA, ncol(edges_or_shortest_paths)), n_minus)), nrow = n_minus
        )
      edges_or_shortest_paths <- rbind(edges_or_shortest_paths, add_row)
      # get name of missing col and row and add them
      missing_nodes <- pull(
        filter(
          tibble(nodes = nodes), 
          !(nodes %in% colnames(edges_or_shortest_paths))),
        nodes
        )
      column_names_current <- as.data.frame(edges_or_shortest_paths) %>% 
        select(any_of(colnames(edges_or_shortest_paths))) %>% 
        names()
      colnames(edges_or_shortest_paths) <- c(column_names_current, missing_nodes) 
      rownames(edges_or_shortest_paths) <- colnames(edges_or_shortest_paths)
      # arrange the colnames and rownames to ensure matrix operations match up
      edges_or_shortest_paths <- edges_or_shortest_paths[, sort(colnames(edges_or_shortest_paths))]
      formatted_edges_or_shortest_paths <- edges_or_shortest_paths[ order(row.names(edges_or_shortest_paths)), ]
      return(formatted_edges_or_shortest_paths)
    }
    ## add missing rows caused from randomly subtracting nodes
    add_missing_nodes <- function(exp_inf){ 
      missing_node <- pull(filter(tibble(nodes = nodes), !(nodes %in% names(exp_inf))), nodes)
      exp_inf[missing_node] <- NA
      formatted_exp_inf <- sort(exp_inf)
      return(formatted_exp_inf)
    }
    
    edges_formatted <- add_missing_columns(edges_or_shortest_paths = bootedges)
    shortest_paths_formatted <- add_missing_columns(edges_or_shortest_paths = shortest_paths)
    ## convert Inf path lengths to zero
    shortest_paths_formatted <- ifelse(
      shortest_paths_formatted == -Inf | shortest_paths_formatted == Inf, 
      NA, 
      shortest_paths_formatted
      )
    expected_influence_formatted <- add_missing_nodes(exp_inf = expected_influence)
    
    return(list(
      expected_influence = expected_influence_formatted, # reorder these for convenience
      shortest_paths = shortest_paths_formatted, 
      edges = edges_formatted
    ))
  }
  
  # run the bootstrap: applies get_edges() for each bootstrap ----
  bootstraps <- pbmcapply::pbmclapply(
    1:n_replicates, # 1:number of bootstrap replicates
    mc.cores = parallel::detectCores(), # use as many cores as computer hass
    mc.style = "txt", # type of progress bar (old school)
    function(x) {
      set.seed(x)
      edges <- get_edges(x = data, nodes = nodes, n_minus = n_minus)
      return(edges = edges)
    }
  ) 
  # get the results ----
  edges_results <- lapply(bootstraps, `[[`, "edges")
  expected_influence_results <- lapply(bootstraps, `[[`, "expected_influence")
  shortest_paths_results <- lapply(bootstraps, `[[`, "shortest_paths")

  # format results ----
  ## for later:
  to_tibble <- function(x, name, ...) {
    x %>%
      as_tibble(rownames = "node1") %>%
      pivot_longer(-node1, names_to = "node2", values_to = name)
  }
  check_agree <- function(x) {
      x <- x %>% 
        mutate(
          prod = conf_lower * conf_upper,
          "Simulation Agrees with Full Model?" = case_when(
            weight > 0 & conf_lower > 0 ~ "Yes",
            weight < 0 & conf_upper < 0 ~ "Yes",
            weight == 0 & (prod < 0 | conf_lower == 0 | conf_upper == 0) ~ "Yes",
            (weight > 0 | weight < 0) & prod < 0 ~ "No",
            TRUE ~ "No"
          )
        ) %>% 
        select(-prod)
    return(x)
  }
  
  ## edges ----
  ### get m, bias, se, and weight from full_net
  M_edge <- apply(simplify2array(edges_results), 1:2, mean, na.rm = TRUE)
  Bias_edge <- M_edge - full_net
  SE_edge <- apply(simplify2array(edges_results), 1:2, sd, na.rm = TRUE)
  Weight_edge <- to_tibble(full_net, "weight") 
  M_edge <- to_tibble(M_edge, "weight_boot") 
  Bias_edge <- to_tibble(Bias_edge, "bias") 
  SE_edge <- to_tibble(SE_edge, "se") 
  edges_stats <- reduce(
    list(Weight_edge, M_edge, SE_edge, Bias_edge), left_join, by = c("node1", "node2")
    ) %>% 
    mutate(key = paste0(pmin(node1, node2), pmax(node1, node2), sep = "")) %>% 
    distinct(key, .keep_all = TRUE) %>% 
    arrange(node1, node2) %>% 
    filter(node1 != node2) %>% 
    select(-key)
  ### get CIs
  edge_conf_lower <- 0.05 / nrow(edges_stats)
  edge_conf_upper <- 1 - (0.05 / nrow(edges_stats))
  CIs_edge <- apply(
    simplify2array(edges_results),
    1:2,
    quantile,
    probs = c(edge_conf_lower, edge_conf_upper),
    na.rm = TRUE
    )
  CIs_edge <- plyr::alply(CIs_edge, 3, .dims = TRUE)
  CIs_edge <- lapply(CIs_edge, function(x) as_tibble(x, rownames = "perc")) %>% 
    bind_rows() %>% 
    mutate(node1 = rep(names(CIs_edge), each = 2)) %>% 
    pivot_longer(-c(perc, node1), names_to = "node2") %>% 
    pivot_wider(names_from = perc, values_from = value)  %>% 
    filter(node1 != node2)
  ### combine stats and CIs
  edges_table <- left_join(edges_stats, CIs_edge, by = c("node1", "node2")) 
  ### prep for check_agree, then check
  edges_table <- edges_table %>% 
    rename(conf_lower = colnames(.)[ncol(.) - 1], conf_upper = colnames(.)[ncol(.)])
  edges_table <- check_agree(edges_table)
  
  ## shortest paths ----
  ### get mean orders ----
  shortest_paths_boot <- pbmcapply::pbmclapply(
    1:length(shortest_paths_results), 
    mc.cores = parallel::detectCores(),
    mc.style = "txt",
    function(i){
      shortest_paths_formatted <- shortest_paths_results[[i]] %>% 
        as.data.frame()  %>% 
        as_tibble(rownames = "from") %>%  
        pivot_longer(-from, names_to = "to", values_to = "shortest_path") %>% 
        filter(from != to, to == focal_node) %>% 
        arrange(shortest_path) %>%
        mutate(order = 1:nrow(.), boot = i) %>% 
        select(-to)
    }) %>% 
    bind_rows() 
  shortest_paths_boot_orders <- shortest_paths_boot %>%
      group_by(from) %>%
      summarize(
      boot_order_m = mean(order, na.rm = TRUE),
      boot_order_se = sd(order, na.rm = TRUE)
      )
  # get paths from full net, m, bias, and se ----
  full_net_paths <- centrality(
    full_net, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE
    )$ShortestPathLengths
  M_paths <- apply(simplify2array(shortest_paths_results), 1:2, mean, na.rm = TRUE)
  Bias_paths <- M_paths - full_net_paths
  SE_paths <- apply(simplify2array(shortest_paths_results), 1:2, sd, na.rm = TRUE)
  ### format stats
  M_paths <- to_tibble(M_paths, "path_boot")
  Bias_paths <- to_tibble(Bias_paths, "bias")
  SE_paths <- to_tibble(SE_paths, "se")
  Path <- to_tibble(full_net_paths, "shortest_path")
  path_stats <- reduce(
    list(Path, M_paths, SE_paths, Bias_paths), left_join, by = c("node1", "node2")
    ) %>%
    filter(node1 != node2, node2 == focal_node) %>% 
    select(-node2)
  ### get CIs
  path_conf_lower <- 0.05 / nrow(path_stats)
  path_conf_upper <- 1 - (0.05 / nrow(path_stats))
  CIs_paths <- apply(
    simplify2array(shortest_paths_results),
    1:2,
    quantile,
    probs = c(path_conf_lower, path_conf_upper),
    na.rm = TRUE
  )
  CIs_paths <- plyr::alply(CIs_paths, 3, .dims = TRUE)
  CIs_paths <- lapply(CIs_paths, function(x) as_tibble(x, rownames = "perc")) %>% 
    bind_rows() %>%
    mutate(node1 = rep(names(CIs_paths), each = 2)) %>%
    pivot_longer(-c(perc, node1), names_to = "node2") %>%
    pivot_wider(names_from = perc, values_from = value) %>%
    filter(node1 != node2, node2 == focal_node, node1 != focal_node)
  ### combine stats and CIs
  shortest_paths_table <- left_join(path_stats, CIs_paths, by = "node1")
  shortest_paths_table <- shortest_paths_table %>%
    select(-node2) %>%
    rename(conf_lower = colnames(.)[ncol(.) - 1], conf_upper = colnames(.)[ncol(.)]) %>%
    arrange(shortest_path) %>%
    mutate(order = 1:nrow(.)) %>%
    arrange(path_boot) %>%
    mutate(boot_order = 1:nrow(.)) %>% 
    rename(from = node1)
  shortest_paths_table <- left_join(
    shortest_paths_table, shortest_paths_boot_orders, by = "from"
    ) %>% 
    arrange(order)
  
  ## expected influence ----

  ### get boot indices and orders
  expected_influence_results <- bind_rows(expected_influence_results) %>% 
    mutate(boot = 1:nrow(.)) %>% 
    pivot_longer(cols = -boot, names_to = "node", values_to = "expected_influence") %>% 
    arrange(boot, desc(abs(expected_influence))) %>% 
    group_by(boot) %>% 
    mutate(order = 1:n(), order = ifelse(is.na(expected_influence), NA, order))
    
  exp_infl_stats <- expected_influence_results %>% 
    group_by(node) %>% 
    summarize(
      expected_influence_boot = mean(expected_influence, na.rm = TRUE),
      se = sd(expected_influence, na.rm = TRUE), 
      boot_order_m = mean(order, na.rm = TRUE),
      boot_order_se = sd(order, na.rm = TRUE)
    )
  ### get confidence intervals
  exp_inf_conf_lower <- 0.05 / nrow(exp_infl_stats)
  exp_inf_conf_upper <- 1 - (0.05 / nrow(exp_infl_stats))
  CIs_exp_inf <- expected_influence_results %>% 
    group_by(node) %>%
    reframe(
      conf_lower = quantile(
        expected_influence, probs = c(exp_inf_conf_lower, exp_inf_conf_upper), na.rm = TRUE
      )[1],
      conf_upper = quantile(
        expected_influence, probs = c(exp_inf_conf_lower, exp_inf_conf_upper), na.rm = TRUE
      )[2]
    ) 
  ### get original from full_net
  exp_inf_full <- centrality(
    full_net, all.shortest.paths = TRUE, weighted = TRUE, signed = TRUE
    )$InExpectedInfluence %>% 
    as_tibble(rownames = "node") %>% 
    rename(expected_influence = value)
  ### combine in table
  exp_inf_table <- reduce(
    list(exp_inf_full, exp_infl_stats, CIs_exp_inf), left_join, by = "node"
    ) %>% 
    arrange(desc(expected_influence)) %>% 
    mutate(order = 1:nrow(.)) %>% 
    arrange(desc(expected_influence_boot)) %>%
    mutate(boot_order = 1:nrow(.)) %>% 
    arrange(order)

  # format conf levels ----
  conf_levels <- tibble(
    edges = edge_conf_upper * 100,
    shortest_paths = path_conf_upper * 100,
    expected_influence = exp_inf_conf_upper * 100
  ) %>% 
    pivot_longer(cols = everything(), names_to = "statistic", values_to = "confidence_level")
  
  # Return ----
  return(list(
    conf_levels = conf_levels,
    edges_boot = edges_results,
    edges_table = edges_table, 
    exp_infl_boot = expected_influence_results, 
    exp_inf_table = exp_inf_table,
    shortest_paths_boot = shortest_paths_boot,
    shortest_paths_boot_table = shortest_paths_results,
    shortest_paths_table = shortest_paths_table
    ))
}

# Plotting ----
## Plot Networks
plot_network <- function(
    fitted_network, node_names, labels, pred_values, panel, filetype, filename
    ) {
  network_plot <- qgraph( # insert image in MS
    fitted_network,
    # formatting edges
    curveAll = TRUE,
    posCol = "#00A08A",
    negCol = "#C93312",
    edge.labels = TRUE,
    # formatting nodes
    nodeNames = node_names,
    labels = labels,
    layout = "circle",
    pie = pred_values,
    legend = FALSE,
    # general formatting and saving
    normalize = TRUE,
    title = panel,
    filetype = filetype, 
    filename = filename
  )
}
## Plot Accuracy
plot_accuracy <- function(data, x, y_lab, x_lab, ...) {
  difference_plot <- ggplot(
    data,
    aes(
      y = value,
      x = !!sym(x),
      ...
    )
  ) +
    geom_hline(
      yintercept = 0, 
      color = wes_palette("Moonrise1")[3],
      linetype = "dashed"
      ) +
    geom_line(
      position = position_dodge(width = 0.25)
    ) +
    geom_ribbon(
      aes(ymin = conf_lower, ymax = conf_upper, color = NULL),
      position = position_dodge(width = 0.25),
      alpha = 0.20,
      show.legend = FALSE
    ) +
    geom_point(
      position = position_dodge(width = 0.25)
    ) +
    scale_x_discrete(expand = c(0, 0.4)) +
    coord_flip() +
    labs(
      x = x_lab,
      y = y_lab
    ) +
    ggthemes::theme_tufte() +
    theme(
      axis.ticks = element_line(color = "grey92"),
      panel.grid.major.y = element_line(color = "grey92"),
      axis.title.x = element_text(hjust = 0.35),
      panel.spacing.x = unit(6, "mm")
    )
  return(difference_plot)
}

# In-Text Statistics
get_in_text <- function(table, statistic, statistic_name, CI = TRUE){
  statistic <- table %>% pull(statistic)
  statistic <- format(round(statistic, 2), nsmall = 2)
  statistic_name <- paste(statistic_name, "\\(\\Delta\\)", sep = "")
  p <- table %>% pull(p)
  if(CI) {
  LWR <- table %>% 
    pull(any_of(c("conf.low", "conf_lower"))) 
  LWR <- format(round(LWR, 2), nsmall = 2)
  UPR <- table %>% pull(any_of(c("conf.high", "conf_upper")))
  UPR <- format(round(UPR, 2), nsmall = 2)
  conf_level <- table %>% pull(alpha)
  conf_level <- (1 - conf_level) * 100
  stats <- paste(
    statistic_name,
    " = ",
    statistic,
    ", ",
    conf_level,
    "% Cluster-Bootstrapped CIs [$",
    LWR,
    ", ",
    UPR,
    "$], $p = ",
    p,
    "$",
    sep = ""
  )
  } else {
    stats <- paste(
      statistic_name, " = ", statistic, ", ", "$], $p = ", p, "$", sep = ""
    )
  }
  return(stats)
}
