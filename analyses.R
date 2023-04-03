#### in MS, look for INSERT HERE
# Load Packages and Data ----------------------------------------------------------------------
packages <- c(
  "papaja",
  "tidyverse",
  "ftExtra",
  "janitor",
  "skimr",
  "patchwork",
  "ggpubr",
  "GGally",
  "wesanderson",
  "huxtable",
  "qgraph",
  "igraph",
  "bootnet",
  "boot",
  "NetworkComparisonTest",
  "here",
  "grid",
  "gridExtra",
  "png"
)
lapply(packages, require, character.only = TRUE)
source(here("functions.R"))
dataset <-
  read_csv(here("data", "fake_data.csv")) %>%
  pivot_wider(names_from = issue, values_from = Anger:Policy) %>%
  filter(complete.cases(.)) %>% # needed for network comparisons
  pivot_longer(cols = c(ends_with("Guns"), ends_with("Smoking"))) %>%
  separate(name, into = c("node", "issue")) %>%
  pivot_wider(names_from = node, values_from = value)
nodes <- c(
  "Anger",
  "AuthInd",
  "BlackWhite",
  "Disgust",
  "Fluency",
  "Suffer",
  "Upset",
  "Wrong",
  "Policy"
)
nodes_select <- paste(nodes, collapse = "|")
smoking_data <- dataset %>%
  filter(issue == "Smoking") %>%
  select(starts_with(nodes))
gun_data <- dataset %>%
  filter(issue == "Guns") %>%
  select(starts_with(nodes))

# Setup and Visualize Literature --------------------------------------------------------------
## This stuff is needed for visualizing the lit AND network model-fitting
### Get network variables
zeroes <- read_csv(here("zero_edge_list.csv")) %>%
  unite("edges")
possible_edges <- t(combn(nodes, 2)) %>%
  as_tibble() %>%
  unite("edges") %>%
  filter(!(edges %in% zeroes$edges)) %>%
  separate(col = edges, into = c("node1", "node2"))
hypo_plot <-
  qgraph(possible_edges[, 1:2], directed = FALSE, DoNotPlot = TRUE)
edges_from_qgraph <- hypo_plot$Edgelist %>%
  as_tibble() %>%
  select(from, to)
possible_edges <- cbind(possible_edges, edges_from_qgraph)
# create tibble that specifies hypothesized edge colors
possible_edges <- possible_edges %>%
  mutate(
    edge_color = case_when(
      node1 == "Anger" & node2 == "AuthInd" ~ "#00A08A",
      # green
      node1 == "Anger" & node2 == "BlackWhite" ~ "#00A08A",
      # green
      node1 == "Anger" & node2 == "Fluency" ~ "#00A08A",
      # green
      node1 == "Anger" & node2 == "Suffer" ~ "#00A08A",
      # green
      node1 == "Anger" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "AuthInd" &
        node2 == "BlackWhite" ~ "#00A08A",
      # green
      node1 == "AuthInd" & node2 == "Disgust" ~ "#00A08A",
      # green
      node1 == "AuthInd" & node2 == "Fluency" ~ "#00A08A",
      # green
      node1 == "AuthInd" & node2 == "Suffer" ~ "#00A08A",
      # green
      node1 == "AuthInd" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "BlackWhite" &
        node2 == "Disgust" ~ "#00A08A",
      # green
      node1 == "BlackWhite" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "Disgust" & node2 == "Fluency" ~ "#C93312",
      # red
      node1 == "Disgust" & node2 == "Suffer" ~ "#00A08A",
      # green
      node1 == "Disgust" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "Fluency" & node2 == "Suffer" ~ "#00A08A",
      # green
      node1 == "Fluency" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "Suffer" & node2 == "Upset" ~ "#00A08A",
      # green
      node1 == "Suffer" & node2 == "Wrong" ~ "#00A08A",
      # green
      node1 == "Upset" & node2 == "Wrong" ~ "#00A08A",
      # green
      str_detect(node1, "Policy") |
        str_detect(node2, "Policy") ~ "#899DA4"
    ),
    linetype = ifelse(node1 == "Policy" |
                        node2 == "Policy", "dotted", "solid")
  )
# create vectors that indicate:
## a) heuristic: cognition or emotion
## b) node wording that goes with each node (for legend)
## node wording, nodes, and heuristic have to be in same order that qgraph puts them
## otherwise in legend, wording and nodes will be mismatched
node1 <- possible_edges %>%
  distinct(node1, from) %>%
  rename(Node = node1, node_number = from)
node2 <- possible_edges %>% distinct(node2, to)
nodes_with_numbers <-
  bind_rows(node1, setNames(node2, names(node1))) %>%
  distinct(Node, .keep_all = TRUE)
match_order <- unique(possible_edges$node1)
node_wordings <- read_csv(here("data", "code_book.csv")) %>%
  filter(Variable %in% colnames(smoking_data) &
           str_starts(`Variable Name`, "vape_")) %>%
  full_join(nodes_with_numbers, by = c("Variable" = "Node")) %>%
  arrange(match(Variable, match_order)) %>%
  select(Variable, Wording_for_Plot, Heuristic)
### Visualize the literature ----
# hypo_plot <- qgraph(
#   # faster to save image, plot stored in Figures/literature_network.png
#   possible_edges[, 1:2],
#   directed = FALSE,
#   # formatting edges
#   edge.color = possible_edges$edge_color,
#   lty = possible_edges$linetype,
#   # formatting nodes
#   groups = node_wordings$Heuristic,
#   nodeNames = node_wordings$Wording_for_Plot,
#   color = c(Cognitions = "#5BBCD6", Emotions = "#F98400", Outcome = "#FAEFD1"),
#   layout = "circle",
#   vTrans = 150,
#   # curveAll = TRUE,
#   label.cex = 1,
#   # formatting legend
#   legend.cex = 0.35, # scalar of the legend
#   legend.mode = "style1", # default is 'style1', different way to show legend
#   # legend = TRUE,
#   layoutScale = c(0.95, 1),
#   layoutOffset = c(-0.155, 0),
#   GLratio = 1.45,
#   # general
#   # normalize = FALSE,
#   # rescale = TRUE,
#   DoNotPlot = FALSE,
#   filetype = "png",
#   filename = "Figures/literature_network"
# )



# Descriptives ---------------------------------------------------------------------
## Demographics ----
### calculate stats
race <- get_demos(dataset,
                  variable = "race_label",
                  name = "Racial or Ethnic Identity")
gen <-
  get_demos(dataset, variable = "gen_label", name = "Gender Identity")
edu <-
  get_demos(dataset, variable = "edu_label", name = "Education")
continuous_demos <- dataset %>%
  select(age, ends_with("conservatism")) %>%
  my_skim() %>%
  select(skim_variable, numeric.mean, numeric.sd, numeric.hist) %>%
  mutate(
    Mean = trimws(format(round(numeric.mean, 2), nsmall = 2)),
    SD = trimws(format(round(numeric.sd, 2), nsmall = 2)),
    "M (SD)" = paste(Mean, " (", SD, ")", sep = ""),
    skim_variable = case_when(
      str_detect(skim_variable, "age") ~ "Age",
      str_detect(skim_variable, "social") ~ "Social Conservativism",
      str_detect(skim_variable, "eco") ~ "Economic Conservativism",
      str_detect(skim_variable, "over") ~ "Overall Conservativism"
    )
  ) %>%
  select("Variable:" = skim_variable, `M (SD)`, Histogram = numeric.hist) %>%
  huxtable()
### format demographics table
bottom_borders <-
  paste(c("Gender", "Racial", "Education"), collapse = "|")
demos <- continuous_demos %>%
  insert_row(rep(NA, ncol(.)), after = nrow(.)) %>%
  add_rows(gen) %>%
  insert_row(rep(NA, ncol(.)), after = nrow(.)) %>%
  add_rows(race) %>%
  insert_row(rep(NA, ncol(.)), after = nrow(.)) %>%
  add_rows(edu) %>%
  set_align(col = 2:3, value = "center") %>%
  set_align(col = 1, value = "left") %>%
  set_bottom_border(row = c(1, nrow(.)), value = 0.1) %>%
  set_bottom_border(row = str_detect(.$`Variable:`, bottom_borders),
                    value = 0.1) %>%
  set_position("left") %>%
  set_font(value = "Times") %>%
  set_col_width(col = ncol(.), value = 0.7)

## Descriptives table (node variables) ----
descs_smoking <- smoking_data %>%
  my_skim() %>%
  as_tibble() %>%
  mutate(
    numeric.mean = format(round(numeric.mean, 2), nsmall = 2),
    numeric.sd = format(round(numeric.sd, 2), nsmall = 2),
    "M (SD)" = paste(numeric.mean, " (", numeric.sd, ")", sep = "")
  ) %>%
  rename(Variable = skim_variable, Histogram = numeric.hist) %>%
  select(Variable, `M (SD)`, Histogram) %>%
  arrange(desc(`M (SD)`)) %>%
  huxtable()
descs_gun <- gun_data %>%
  my_skim() %>%
  as_tibble() %>%
  mutate(
    numeric.mean = format(round(numeric.mean, 2), nsmall = 2),
    numeric.sd = format(round(numeric.sd, 2), nsmall = 2),
    "M (SD)" = paste(numeric.mean, " (", numeric.sd, ")", sep = "")
  ) %>%
  rename(Variable = skim_variable, Histogram = numeric.hist) %>%
  select(Variable, `M (SD)`, Histogram) %>%
  arrange(desc(`M (SD)`)) %>%
  huxtable(add_colnames = FALSE)
descs <- descs_smoking %>%
  insert_row(after = 1, Variable = "Smoking", NA, NA) %>%
  insert_row(after = nrow(.), rep(NA, 3)) %>%
  insert_row(after = nrow(.), Variable = "Guns", NA, NA) %>%
  set_italic(row = c(2, nrow(.)), value = TRUE) %>%
  set_italic(row = 1, col = 2, value = TRUE) %>%
  add_rows(descs_gun) %>%
  set_align(col = 2:3, value = "center") %>%
  set_top_border(row = 1, value = 0.1) %>%
  set_bottom_border(row = c(1, nrow(.)), value = 0.1) %>%
  set_position("left") %>%
  set_col_width(col = ncol(.), value = 0.7)

# Fit Networks, Get Network Statistics ---------------------------------------------
network_smoking <- dataset %>%
  filter(issue == "Smoking") %>%
  select(contains(nodes)) %>%
  estimateNetwork(default = "EBICglasso",
                  corMethod = "spearman")
centrality_smoking <- centrality(
  network_smoking,
  R2 = TRUE,
  all.shortest.paths = TRUE,
  weighted = TRUE,
  signed = TRUE
)
pred_values_smoking <- centrality_smoking$R2
network_gun <- dataset %>%
  filter(issue == "Guns") %>%
  select(contains(nodes)) %>%
  estimateNetwork(default = "EBICglasso",
                  corMethod = "spearman")
centrality_gun <- centrality(
  network_gun,
  R2 = TRUE,
  all.shortest.paths = TRUE,
  weighted = TRUE,
  signed = TRUE
)
pred_values_gun <- centrality_gun$R2

# Bootstrap Model Accuracy ---------------------------------------------------------
# set.seed(352)
# network_smoking_booted <- bootnet(
#   network_smoking,
#   statistics = c("edge", "expectedInfluence", "distance"),
#   computeCentrality = TRUE,
#   signed = TRUE,
#   weighted = TRUE,
#   all.shortest.paths = TRUE,
#   nBoots = 500,
#   nCores = 8,
#   type = "nonparametric"
#   )
# set.seed(352)
# network_gun_booted <- bootnet(
#   network_gun,
#   statistics = c("edge", "expectedInfluence", "distance"),
#   computeCentrality = TRUE,
#   signed = TRUE,
#   weighted = TRUE,
#   all.shortest.paths = TRUE,
#   nBoots = 500,
#   nCores = 8,
#   type = "nonparametric"
#   )
# saveRDS(
#   network_smoking_booted,
#   here("bootstraps", "network_accuracy", "network_smoking_booted_fake")
#   )
# saveRDS(
#   network_gun_booted,
#   here("bootstraps", "network_accuracy", "network_gun_booted_fake")
#   )
# Conduct Network Comparison -------------------------------------------------------
## Conduct NCT ----
# nct <- NCT(
#   network_smoking,
#   network_gun,
#   paired = TRUE,
#   test.edges = TRUE,
#   test.centrality = TRUE,
#   it = 500,
#   abs = FALSE,
#   weighted = TRUE,
#   centrality = "expectedInfluence",
#   p.adjust.methods = "bonferroni"
# )
# # Bootstrap Comparisons (robustness check against NCT) ----
# dataset_nested <- dataset %>%
#   select(id, issue, nodes) %>%
#   group_by(id, .add = TRUE) %>%
#   group_nest()
# booted_cent_comp <- boot(
#   data = dataset_nested,
#   statistic = boot_centrality_func,
#   R = 500,
#   parallel = "multicore",
#   ncpus = parallel::detectCores()
#   )
# booted_edges_comp <- boot(
#   data = dataset_nested,
#   statistic = boot_edges_func,
#   R = 500,
#   parallel = "multicore",
#   ncpus = parallel::detectCores()
#   )
# booted_path_len_comp <- boot(
#   data = dataset_nested,
#   statistic = boot_path_len_func,
#   R = 500,
#   parallel = "multicore",
#   ncpus = parallel::detectCores()
#   )
# booted_global_strength_comp <- boot(
#   data = dataset_nested,
#   statistic = boot_global_strength_func,
#   R = 500,
#   parallel = "multicore",
#   ncpus = parallel::detectCores()
#   )
# booted_max_edge_comp <- boot(
#   data = dataset_nested,
#   statistic = boot_max_edge_func,
#   R = 500,
#   parallel = "multicore",
#   ncpus = parallel::detectCores()
#   )
# ## save nct
# saveRDS(nct, here("bootstraps", "network_comparisons", "nct_fake"))
# # save bootstrapped comparisons
# saveRDS(booted_cent_comp, here("bootstraps", "network_comparisons", "booted_cent_comp_fake"))
# saveRDS(
#   booted_edges_comp,
#   here("bootstraps", "network_comparisons", "booted_edges_comp_fake")
# )
# saveRDS(
#   booted_path_len_comp,
#   here("bootstraps", "network_comparisons", "booted_path_len_comp_fake")
# )
# saveRDS(
#   booted_global_strength_comp,
#   here("bootstraps", "network_comparisons", "booted_global_strength_comp_fake")
# )
# saveRDS(
#   booted_max_edge_comp,
#   here("bootstraps", "network_comparisons", "booted_max_edge_comp_fake")
# )

# Conduct Sensitivity Analysis -----------------------------------------------------
## smoking
# RNGkind("L'Ecuyer-CMRG")
# set.seed(352)
# sens_smoking_minus_1 <- bootstrap_minus_n(
#   smoking_data,
#   n_replicates = round(500 + (500 * (1/ncol(smoking_data))), 0),
#   nodes = colnames(smoking_data),
#   full_net = network_smoking$graph,
#   n_minus = 1,
#   focal_node = "Policy"
#   )
# ## gun
# RNGkind("L'Ecuyer-CMRG")
# set.seed(352)
# sens_gun_minus_1 <- bootstrap_minus_n(
#   gun_data,
#   n_replicates = round(500 + (500 * (1/ncol(gun_data))), 0),
#   nodes = colnames(gun_data),
#   full_net = network_gun$graph,
#   n_minus = 1,
#   focal_node = "Policy"
#   )
# # save sensitivity analyses
# saveRDS(
#   sens_smoking_minus_1,
#   here("bootstraps", "sensitivity_analyses", "sens_smoking_minus_1_fake")
#   )
# saveRDS(
#   sens_gun_minus_1,
#   here("bootstraps", "sensitivity_analyses", "sens_gun_minus_1_fake")
#   )
# # RESULTS ----
# ## Plot Networks
# network_smoking_plot <- plot_network(
#   network_smoking$graph,
#   node_names = node_wordings$Wording_for_Plot,
#   labels = colnames(smoking_data),
#   pred_values = pred_values_smoking,
#   panel = "A (Smoking)",
#   filename = here("Figures", "network_smoking_plot_fake"),
#   filetype = "png"
#   )
# network_gun_plot <- plot_network(
#   network_gun$graph,
#   node_names = node_wordings$Wording_for_Plot,
#   labels = colnames(gun_data),
#   pred_values = pred_values_gun,
#   panel = "B (Gun Ownership)",
#   filename = here("Figures", "network_gun_plot_fake"),
#   filetype = "png"
# )
# img1 <- readPNG(here("Figures", "network_smoking_plot_fake.png"))
# img2 <- readPNG(here("Figures", "network_gun_plot_fake.png"))
# img1 <-  rasterGrob(as.raster(img1), interpolate = FALSE)
# img2 <-  rasterGrob(as.raster(img2), interpolate = FALSE)
# network_grid <- grid.arrange(img1, img2, ncol = 2)
# ggsave(
#   network_grid,
#   filename = here("Figures", "network_grid_fake.png"),
#   height = 2.5,
#   width = 5,
#   scale = 1.5
#   )
## Read In Bootstrapped CIs
network_smoking_booted <- readRDS(
  here(
    "bootstraps",
    "network_accuracy",
    "network_smoking_booted_fake"
  )
)
network_gun_booted <- readRDS(here(
  "bootstraps",
  "network_accuracy",
  "network_gun_booted_fake"
))
## Read In Network Comparison Test
nct <- readRDS(here(
  "bootstraps",
  "network_comparisons",
  "nct_fake"
))
## Read In Bootstrapped Difference CIs
booted_global_strength_comp <- readRDS(
  here(
    "bootstraps",
    "network_comparisons",
    "booted_global_strength_comp_fake"
  )
)
booted_max_edge_comp <- readRDS(
  here(
    "bootstraps",
    "network_comparisons",
    "booted_max_edge_comp_fake"
  )
)
# booted_edges_comp <- readRDS(
#   here(
#     "bootstraps",
#     "network_comparisons",
#     "booted_edges_comp_fake"
#   )
# )
# booted_cent_comp <- readRDS(
#   here(
#     "bootstraps",
#     "network_comparisons",
#     "booted_cent_comp_fake"
#   )
# )
# booted_path_len_comp <- readRDS(
#   here(
#     "bootstraps",
#     "network_comparisons",
#     "booted_path_len_comp_fake"
#   )
# )
## Read In Bootstrap Sensitivity Analyses
sens_smoking_minus_1 <- readRDS(
  here(
    "bootstraps",
    "sensitivity_analyses",
    "sens_smoking_minus_1_fake"
  )
)
sens_gun_minus_1 <- readRDS(
  here(
    "bootstraps",
    "sensitivity_analyses",
    "sens_gun_minus_1_fake"
  )
)
### Global Strength Difference 
global_strength_booted <- booted_global_strength_comp %>%
  tidy(conf.int = TRUE, conf.method = "perc") %>%
  select(starts_with("conf"))
global_strength_diff <- tibble(
  strength_smoking = nct$glstrinv.sep[1],
  strength_gun = nct$glstrinv.sep[2],
  abs_diff = nct$glstrinv.real,
  conf.low = global_strength_booted$conf.low,
  conf.high = global_strength_booted$conf.high,
  p.value = nct$glstrinv.pval
)
global_strength_diff <-
  test_consistency_bs_nct(global_strength_diff,
                          n_tests = nrow(as_tibble(nct$glstrinv.pval)))
### Max Edge Difference (Global Structure Invariance
max_edge_booted_tbl <- booted_max_edge_comp %>%
  tidy(conf.int = TRUE, conf.method = "perc") %>%
  select(starts_with("conf"))
max_edge_diff <- tibble(
  abs_max_edge_diff = nct$nwinv.real,
  conf.low = max_edge_booted_tbl$conf.low,
  conf.high = max_edge_booted_tbl$conf.high,
  p.value = nct$nwinv.pval
)
max_edge_diff <- test_consistency_bs_nct(
  max_edge_diff,
  n_tests = nrow(as_tibble(nct$nwinv.pval))
  ) 

## Edges -------------------------------------------------------------------------
### Prep Data for Plots
#### Edges for Smoking
# edges_smoking <- nct$nw1 %>%
#   as_tibble(rownames = "Var1") %>%
#   pivot_longer(-Var1, names_to = "Var2", values_to = "edge_smoking") %>%
#   filter(!duplicated(paste0(pmax(Var1, Var2), pmin(Var1, Var2))))
# edges_smoking_boot <- network_smoking_booted$sampleTable %>%
#   filter(type == "edge") %>%
#   left_join(
#     get_CIs(
#       network_smoking_booted$bootTable,
#       stat = "edge",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = c("node1", "node2")
#   ) %>%
#   select(starts_with("node"), value, starts_with("conf")) %>%
#   unite(col = "edge", starts_with("node"), sep = " - ") %>%
#   mutate(Issue = "Smoking", Model = "Empirical")
# 
# #### Edges for Gun Ownership
# edges_gun <- nct$nw2 %>%
#   as_tibble(rownames = "Var1") %>%
#   pivot_longer(-Var1, names_to = "Var2", values_to = "edge_gun") %>%
#   filter(!duplicated(paste0(pmax(Var1, Var2), pmin(Var1, Var2))))
# edges_gun_boot <- network_gun_booted$sampleTable %>%
#   filter(type == "edge") %>%
#   left_join(
#     get_CIs(
#       network_gun_booted$bootTable,
#       stat = "edge",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = c("node1", "node2")
#   ) %>%
#   select(starts_with("node"), value, starts_with("conf")) %>%
#   unite(col = "edge", starts_with("node"), sep = " - ") %>%
#   mutate(Issue = "Gun Ownership", Model = "Empirical")
# 
# #### Edge Differences
# actual_edges_diffs <- left_join(edges_smoking, edges_gun) %>%
#   mutate(value = edge_smoking - edge_gun)
boot_conf_edges <- 1 - (0.05 / nrow(as_tibble(nct$einv.pvals)))
# booted_edges_comp_tbl <- booted_edges_comp %>%
#   tidy(conf.int = TRUE,
#        conf.level = boot_conf_edges,
#        conf.method = "perc") %>%
#   select(value = statistic, starts_with("conf"))
# edge_diffs <-
#   left_join(actual_edges_diffs, booted_edges_comp_tbl, by = "value") %>%
#   left_join(nct$einv.pvals, by = c("Var1", "Var2")) %>%
#   filter(!duplicated(paste0(pmax(Var1, Var2), pmin(Var1, Var2)))) %>%
#   rename(p.value = `p-value`,
#          Node1 = Var1,
#          Node2 = Var2) %>%
#   filter(!is.na(p.value)) %>%
#   unite(col = "edge",
#         Node1:Node2,
#         sep = " - ",
#         remove = FALSE)
# edge_diffs <- test_consistency_bs_nct(edge_diffs,
#                                       n_tests = nrow(as_tibble(nct$einv.pvals)))
# edge_diffs <- edge_diffs %>%
#   select(edge, value, conf_lower = conf.low, conf_upper = conf.high) %>%
#   mutate(Issue = "Difference", Model = "Empirical")
# 
# #### Edge Sensitivity
# ## prep sensitivity edge data
# edges_sens_smoking <- sens_smoking_minus_1$edges_table %>%
#   unite(col = "edge", node1:node2, sep = " - ") %>%
#   select(
#     edge,
#     "Full Network" = weight,
#     "Bootstrap Minus 1 Node" = weight_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Smoking", Model = "Sensitivity") %>%
#   select(edge, value, conf_lower, conf_upper, Issue, Model)
# edges_sens_gun <- sens_gun_minus_1$edges_table %>%
#   unite(col = "edge", node1:node2, sep = " - ") %>%
#   select(
#     edge,
#     "Full Network" = weight,
#     "Bootstrap Minus 1 Node" = weight_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Gun Ownership", Model = "Sensitivity") %>%
#   select(edge, value, conf_lower, conf_upper, Issue, Model)
# 
# #### Combine Edges Into Single Tibble
# edges <- bind_rows(edges_smoking_boot, edges_gun_boot, edge_diffs)
# edge_levels <- edge_diffs %>%
#   arrange(desc(value)) %>%
#   pull(edge)
# possible_edges_plot <- possible_edges %>%
#   unite(node1:node2, col = "edge", sep = " - ") %>%
#   mutate(
#     value = case_when(
#       edge_color == "#00A08A" ~ 0.1,
#       edge_color == "#C93312" ~ -0.1,
#       TRUE ~ 0
#     ),
#     Model = "Literature",
#     Issue = NA
#   )
# all_edges <-
#   left_join(select(edges, edge), select(possible_edges_plot, edge)) %>% distinct(edge)
# possible_edges_plot <- left_join(all_edges, possible_edges_plot)
# possible_edges_plot <- mutate(
#   possible_edges_plot,
#   value = case_when(
#     is.na(value) & str_detect(edge, "Policy") ~ -0.2,
#     is.na(value) & !str_detect(edge, "Policy") ~ 0,
#     TRUE ~ value
#   ),
#   Issue = "-",
#   Model = ifelse(is.na(Model), "Literature", Model)
# ) %>%
#   select(edge, value, Issue, Model)
# edges <- full_join(edges,
#                    possible_edges_plot,
#                    by = c("edge", "value", "Issue", "Model"))
# 
# edges <- bind_rows(edges, edges_sens_smoking, edges_sens_gun)
# edges <- edges %>%
#   mutate(edge = factor(edge, levels = edge_levels))
# 
# ## Plot Edges
# edge_literature_plot <- plot_accuracy(
#   filter(edges, Model == "Literature"),
#   x = "edge",
#   y_lab = "Direction of Association",
#   x_lab = "Edge",
#   group = 1
# ) +
#   ggtitle("Literature") +
#   scale_y_continuous(breaks = c(-0.1, 0, 0.1),
#                      labels = c("-", "Absent \nor \nUnknown", "+")) +
#   theme(axis.title.y = element_text(size = 10))
# edge_networks_plot <- plot_accuracy(
#   filter(
#     edges,
#     Issue %in% c("Smoking", "Gun Ownership"),
#     Model == "Empirical"
#   ),
#   x = "edge",
#   y_lab = "Weight",
#   x_lab = NULL,
#   group = Issue,
#   fill = Issue,
#   color = Issue,
#   shape = Issue
# ) +
#   ggtitle("Network Models") +
#   theme(axis.text.y = element_blank())
# edge_diffs_plot <- plot_accuracy(
#   filter(edges, Issue == "Difference"),
#   x = "edge",
#   y_lab = expression(Delta ~ Weight),
#   x_lab = NULL,
#   group = 1
# ) +
#   ggtitle("Difference: \nSmoking - Gun Ownership") +
#   theme(axis.text.y = element_blank()) +
#   scale_y_continuous(breaks = seq(-.4, .4, by = .2))
# edge_sensitivity_plot <- plot_accuracy(
#   filter(edges, Model == "Sensitivity"),
#   x = "edge",
#   x_lab = NULL,
#   y_lab = "Weight",
#   group = Issue,
#   shape = Issue,
#   fill = Issue,
#   color = Issue
# ) +
#   ggtitle("Sensitivity Analysis") +
#   theme(axis.text.y = element_blank())
# #### Combine and Save Edge Plots
# (edge_plots <- edge_literature_plot +
#   edge_networks_plot +
#   edge_diffs_plot +
#   edge_sensitivity_plot +
#   plot_layout(widths = c(2, 4, 4, 4), guides = "collect") +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     axis.title.x = element_text(hjust = 0.5, size = 10),
#     legend.position = "bottom",
#     plot.tag.position = "topleft",
#     plot.title = element_text(size = 10, hjust = 0.5),
#     plot.title.position = "panel"
#   ) &
#   scale_fill_manual(values = c("black", wes_palettes$Royal1[1])) &
#   scale_color_manual(values = c("black", wes_palettes$Royal1[1])) &
#   labs(fill = "Issue:", color = "Issue:", shape = "Issue:"))
# ggsave(
#   filename = "Figures/edge_plots_fake.png",
#   edge_plots,
#   scale = 1,
#   width = 8,
#   height = 6
# )

# ## EXPECTED INFLUENCE ----
# #### EI for Smoking
# exp_inf_smoking_boot <- network_smoking_booted$sampleTable %>%
#   filter(type == "expectedInfluence") %>%
#   left_join(
#     get_CIs(
#       network_smoking_booted$bootTable,
#       stat = "expectedInfluence",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = "node1"
#   ) %>%
#   select(node1, value, starts_with("conf")) %>%
#   mutate(Issue = "Smoking", Model = "Empirical") %>%
#   rename(Node = node1)
# #### EI for Gun Ownership
# exp_inf_gun_boot <- network_gun_booted$sampleTable %>%
#   filter(type == "expectedInfluence") %>%
#   left_join(
#     get_CIs(
#       network_gun_booted$bootTable,
#       stat = "expectedInfluence",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = "node1"
#   ) %>%
#   select(node1, value, starts_with("conf")) %>%
#   mutate(Issue = "Gun Ownership", Model = "Empirical") %>%
#   rename(Node = node1)
# 
# #### EI Differences
# actual_ei_diffs <- nct$diffcen.real %>%
#   as_tibble(rownames = "Node") %>%
#   select(Node, ei_diffs = expectedInfluence)
# ei_pvals <- nct$diffcen.pval %>%
#   as_tibble(rownames = "Node") %>%
#   select(Node, p.value = expectedInfluence)
boot_conf_cent <- 1 - (0.05 / dim(select(dataset, nodes))[2])
# booted_cent_comp_tbl <- booted_cent_comp %>%
#   tidy(conf.int = TRUE,
#        conf.level = boot_conf_cent,
#        conf.method = "perc") %>%
#   select(Node = term, starts_with("conf"))
# ei_diffs <- left_join(actual_ei_diffs, booted_cent_comp_tbl, by = "Node") %>%
#   left_join(ei_pvals, by = "Node")
# ei_diffs <-
#   test_consistency_bs_nct(ei_diffs, n_tests = nrow(as_tibble(nct$diffcen.pval)))
# ei_diffs <- ei_diffs %>%
#   select(Node, value = ei_diffs, conf_lower = conf.low, conf_upper = conf.high) %>%
#   mutate(Issue = "Difference", Model = "Empirical")
# 
# #### EI Sensitivity
# exp_inf_sens_smoking <- sens_smoking_minus_1$exp_inf_table %>%
#   select(
#     node,
#     "Full Network" = expected_influence,
#     "Bootstrap Minus 1 Node" = expected_influence_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Smoking", Model = "Sensitivity") %>%
#   select(Node = node, value, conf_lower, conf_upper, Issue, Model)
# exp_inf_sens_gun <- sens_gun_minus_1$exp_inf_table %>%
#   select(
#     node,
#     "Full Network" = expected_influence,
#     "Bootstrap Minus 1 Node" = expected_influence_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Gun Ownership", Model = "Sensitivity") %>%
#   select(Node = node, value, conf_lower, conf_upper, Issue, Model)
# 
# #### EI Combine to Single Tibble
# exp_infs <-
#   bind_rows(
#     exp_inf_smoking_boot,
#     exp_inf_gun_boot,
#     ei_diffs,
#     exp_inf_sens_smoking,
#     exp_inf_sens_gun
#   )
# exp_infs_levels <- ei_diffs %>% arrange(desc(value)) %>% pull(Node)
# exp_infs <- mutate(exp_infs, Node = factor(Node, levels = exp_infs_levels))
# #### EI Plots ----
# exp_inf_networks_plot <- plot_accuracy(
#   filter(
#     exp_infs,
#     Issue %in% c("Smoking", "Gun Ownership"),
#     Model == "Empirical"
#   ),
#   x = "Node",
#   y_lab = "Expected Influence",
#   x_lab = "Node",
#   group = Issue,
#   fill = Issue,
#   color = Issue,
#   shape = Issue
#   ) +
#   ggtitle("Network Models")
# exp_infs_diffs_plot <- plot_accuracy(
#   filter(exp_infs, Issue == "Difference"),
#   x = "Node",
#   y_lab = expression(Delta ~ "Expected Influence"),
#   x_lab = NULL,
#   group = 1) +
#   ggtitle("Difference: \nSmoking - Gun Ownership") +
#   theme(axis.text.y = element_blank())
# exp_infs_sensitivity_plot <- plot_accuracy(
#   filter(exp_infs, Model == "Sensitivity"),
#   x = "Node",
#   x_lab = NULL,
#   y_lab = "Expected Influence",
#   group = Issue,
#   shape = Issue,
#   fill = Issue,
#   color = Issue
#   ) +
#   ggtitle("Sensitivity Analysis") +
#   theme(axis.text.y = element_blank())
# #### Combine and Save Expected Influence Plots
# exp_infs_plots <- exp_inf_networks_plot +
#   exp_infs_diffs_plot +
#   exp_infs_sensitivity_plot +
#   plot_layout(guides = "collect") +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     axis.title.x = element_text(hjust = 0.5, size = 10),
#     legend.position = "bottom",
#     plot.tag.position = "topleft",
#     plot.title = element_text(size = 10, hjust = 0.5),
#     plot.title.position = "panel"
#   ) &
#   scale_fill_manual(values = c("black", wes_palettes$Royal1[1])) &
#   scale_color_manual(values = c("black", wes_palettes$Royal1[1])) &
#   labs(fill = "Issue:", color = "Issue:", shape = "Issue:")
# ggsave(
#   filename = "Figures/exp_infs_plots_fake.png",
#   exp_infs_plots,
#   scale = 1.4,
#   width = 6,
#   height = 2.5
# )
# 
# ## SHORTEST PATH LENGTHS ----
# ### Prep Shortest Paths Data (Bootstrap Only)
# #### Paths for Smoking
# paths_smoking_boot <- network_smoking_booted$sampleTable %>%
#   filter(type == "distance", node2 == "Policy") %>%
#   left_join(
#     get_CIs(
#       network_smoking_booted$bootTable,
#       stat = "distance",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = c("node1", "node2")
#   ) %>%
#   select(node1, value, starts_with("conf")) %>%
#   mutate(Issue = "Smoking", Model = "Empirical")
# #### Paths for Gun Ownership
# paths_gun_boot <- network_gun_booted$sampleTable %>%
#   filter(type == "distance", node2 == "Policy") %>%
#   left_join(
#     get_CIs(
#       network_gun_booted$bootTable,
#       stat = "distance",
#       conf_level = c(0.025, 0.975)
#     ),
#     by = c("node1", "node2")
#   ) %>%
#   select(node1, value, starts_with("conf")) %>%
#   mutate(Issue = "Gun Ownership", Model = "Empirical")
# 
# #### Path Differences
boot_conf_path <- 1 - (0.05 / (ncol(smoking_data) - 1))
# path_len_booted_tbl <- booted_path_len_comp %>%
#   tidy(conf.int = TRUE,
#        conf.level = boot_conf_path,
#        conf.method = "perc") %>%
#   select(value = statistic, starts_with("conf"))
# paths_diffs <- paths_smoking_boot %>%
#   select(node1, smoking = value) %>%
#   left_join(
#     select(
#       paths_gun_boot,
#       node1,
#       gun = value
#       )
#     ) %>%
#   mutate(value = smoking - gun, Issue = "Difference", Model = "Empirical") %>%
#   left_join(path_len_booted_tbl) %>%
#   select(node1, value, conf_lower = conf.low, conf_upper = conf.high, Issue, Model)
# 
# #### Path Sensitivity
# paths_sens_smoking <- sens_smoking_minus_1$shortest_paths_table %>%
#   select(
#     from,
#     "Full Network" = shortest_path,
#     "Bootstrap Minus 1 Node" = path_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Smoking", Model = "Sensitivity") %>%
#   select(node1 = from, value, conf_lower, conf_upper, Issue, Model)
# paths_sens_gun <- sens_gun_minus_1$shortest_paths_table %>%
#   select(
#     from,
#     "Full Network" = shortest_path,
#     "Bootstrap Minus 1 Node" = path_boot,
#     starts_with("conf")
#   ) %>%
#   pivot_longer(
#     cols = c(`Full Network`, `Bootstrap Minus 1 Node`),
#     names_to = "statistic_origin",
#     values_to = "value"
#   ) %>%
#   filter(statistic_origin == "Full Network") %>%
#   mutate(Issue = "Gun Ownwership", Model = "Sensitivity") %>%
#   select(node1 = from, value, conf_lower, conf_upper, Issue, Model)
# 
# #### Combine Paths Into Single Tibble
# paths <- bind_rows(
#   paths_smoking_boot,
#   paths_gun_boot,
#   paths_diffs,
#   paths_sens_smoking,
#   paths_sens_gun
#   ) %>%
#   rename(From = node1)
# paths_levels <- paths_diffs %>% arrange(desc(value)) %>% pull(node1)
# paths <- mutate(paths, From = factor(From, levels = paths_levels))
# 
# ### Paths Plots
# paths_networks_plot <- plot_accuracy(
#   filter(
#     paths,
#     Issue %in% c("Smoking", "Gun Ownership"),
#     Model == "Empirical"
#   ),
#   x = "From",
#   y_lab = "Shortest Path",
#   x_lab = "From",
#   group = Issue,
#   fill = Issue,
#   color = Issue,
#   shape = Issue
#   ) +
#   ggtitle("Network Models")
# paths_diffs_plot <- plot_accuracy(
#   filter(paths, Issue == "Difference"),
#   x = "From",
#   y_lab = expression(Delta ~ "Shortest Path"),
#   x_lab = NULL,
#   group = 1) +
#   ggtitle("Difference: \nSmoking - Gun Ownership") +
#   theme(axis.text.y = element_blank())
# paths_sensitivity_plot <- plot_accuracy(
#   filter(paths, Model == "Sensitivity"),
#   x = "From",
#   y_lab = "Shortest Path",
#   x_lab = NULL,
#   group = Issue,
#   shape = Issue,
#   fill = Issue,
#   color = Issue
#   ) +
#   ggtitle("Sensitivity Analysis") +
#   theme(axis.text.y = element_blank())
# #### Combine and Save Expected Influence Plots
# paths_plots <- (paths_networks_plot +
#     plot_layout(guides = "collect") &
#     theme(legend.position = "bottom")) +
#     paths_diffs_plot +
#   paths_sensitivity_plot +
#     theme(legend.position = "none") +
#   plot_annotation(tag_levels = "A") &
#   theme(
#     axis.title.x = element_text(hjust = 0.5, size = 10),
#     plot.tag.position = "topleft",
#     plot.title = element_text(size = 10, hjust = 0.5),
#     plot.title.position = "panel"
#   ) &
#   scale_fill_manual(values = c("black", wes_palettes$Royal1[1])) &
#   scale_color_manual(values = c("black", wes_palettes$Royal1[1])) &
#   labs(
#     fill = "Issue:", color = "Issue:", shape = "Issue:", group = "Issue:",
#     line = "Issue:", legend = "Issue:"
#     )
# ggsave(
#   filename = "Figures/paths_plots_fake.png",
#   paths_plots,
#   scale = 1.4,
#   width = 6,
#   height = 2.25
# )
# Appendix ----
# png(
#   filename = "Figures/pairs_plot_smoking_spearman_fake.png",
#   width = 1200,
#   height = 1200,
#   units = "px"
# )
# dataset %>%
#   filter(issue == "Smoking") %>%
#   select(Anger:Policy) %>%
#   psych::pairs.panels(
#     jiggle = TRUE,
#     ci = TRUE,
#     pch = ".",
#     smoother = TRUE,
#     method = "spearman"
#   )
# dev.off()
#
# png(
#   filename = "Figures/pairs_plot_gun_spearman_fake.png",
#   width = 1200,
#   height = 1200,
#   units = "px"
# )
# dataset %>%
#   filter(issue == "Guns") %>%
#   select(Anger:Policy) %>%
#   psych::pairs.panels(
#     jiggle = TRUE,
#     ci = TRUE,
#     pch = ".",
#     smoother = TRUE,
#     method = "spearman"
#   )
# dev.off()

# polycorrs_smoking <- dataset %>%
#   filter(issue == "Smoking") %>%
#   select(Anger:Policy) %>%
#   mutate(across(everything(), as.ordered)) %>%
#   polycor::hetcor(ML = TRUE, parallel = TRUE, ncores = parallel::detectCores())
# saveRDS(
#   polycorrs_smoking,
#   here("bootstraps", "polycorrs_smoking_fake")
#   )
polycorrs_smoking <- readRDS(here(
  "bootstraps", "polycorrs_smoking_fake"
))
polycorrs_smoking$correlations[lower.tri(polycorrs_smoking$correlations)] <- NA
spearmans_smoking <- dataset %>%
  filter(issue == "Smoking") %>%
  select(Anger:Policy) %>%
  cor(method = "spearman")
spearmans_smoking[lower.tri(spearmans_smoking)] <- NA
corrs_diffs_smoking <- polycorrs_smoking$correlations - spearmans_smoking
corrs_diffs_smoking <- as_tibble(corrs_diffs_smoking, rownames = "Variable")
corrs_diffs_smoking <- corrs_diffs_smoking %>% 
  huxtable() %>% 
  theme_article() %>% 
  set_position("left") %>% 
  set_align(row = 1:nrow(.), col = -1, value = ".") %>% 
  set_number_format(value = 2)
  

# polycorrs_gun <- dataset %>%
#   filter(issue == "Guns") %>%
#   select(Anger:Policy) %>%
#   mutate(across(everything(), as.ordered)) %>%
#   polycor::hetcor(ML = TRUE, parallel = TRUE, ncores = parallel::detectCores())
# saveRDS(
#   polycorrs_gun,
#   here("bootstraps", "polycorrs_gun_fake")
#   )
polycorrs_gun <- readRDS(here("bootstraps", "polycorrs_gun_fake"))
polycorrs_gun$correlations[lower.tri(polycorrs_gun$correlations)] <- NA
spearmans_gun <- dataset %>%
  filter(issue == "Guns") %>%
  select(Anger:Policy) %>%
  cor(method = "spearman")
spearmans_gun[lower.tri(spearmans_gun)] <- NA
corrs_diffs_gun <- polycorrs_gun$correlations - spearmans_gun
corrs_diffs_gun <- as_tibble(corrs_diffs_gun, rownames = "Variable")
corrs_diffs_gun <- corrs_diffs_gun %>% 
  huxtable() %>% 
  theme_article() %>% 
  set_position("left") %>% 
  set_align(row = 1:nrow(.), col = -1, value = ".") %>% 
  set_number_format(value = 2)