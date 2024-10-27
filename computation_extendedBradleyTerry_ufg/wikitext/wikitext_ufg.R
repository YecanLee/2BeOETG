# Analysis of decoding strategies by ufg-depth approach

# As performance measures we use consistency, MAUVE, and Diversity, perplexity


# This code is (partially) copied from
# Julian Rodemann, Hannah Blocher (2024)....
# and
# Hannah Blocher, Georg Schollmeyer, Malte Nalenz, Chritoph Jansen (2024) ....




################################################################################
# R Session Setup
################################################################################

### Information about packages ofoos. This package is under development on
# git. Installation can be done by:
# install.packages("devtools")
# devtools::install_github("schollmeyer/oofos")
library(oofos)

### Information about packages ddandrda. This package is under development on
# git. Installation can be done by:
# remove.packages("ddandrda")
# install.packages("devtools")
# devtools::install_github("hannahblo/ddandrda")
library(ddandrda)


library(gurobi)

library(reshape)
library(dplyr)
library(data.table)
library(ggplot2)
library(forcats)
library(gridExtra)
library(stargazer)
library(prefmod)
library(reshape2)
library(utils)
library(stringr)


################################################################################
#
# Please choose between the following setup options
#
################################################################################


# Where to store all produced objects?
# setwd()

# Which data set shall be analysed? Choose between "book"  "wikinews" "wikitext"
data_subset_name <-  "wikitext"


# subset of considered models
model_subset_name <- c("Mistral 3 CS (('0.6', '15'))", "Mistral 3 CS (('0.4', '3'))",
                       "Mistral 3 CS (('0.8', '3'))", "Mistral 3 CS (('0.4', '20'))", "Mistral 3 CS (('0.4', '50'))")


# choose subsample size
size_subsample <- 1314

# set a seed for the subsample
set.seed(2938)

# prefix of stored files
prefix_files <- "wikitext_1314gen_4met_"


################################################################################
# Functions needed later
################################################################################
# Computing the ufg depth based on already computed premises
compute_ufg_exist_premises <- function(poset_interest, ufg_premises,
                                       prep_ufg_premises) {

  emp_prob <- prep_ufg_premises$count_dup / prep_ufg_premises$number_obs
  depth_ufg <- rep(0, length(poset_interest))
  constant_c <- 0

  for (i in 1:length(ufg_premises)) {
    # print(paste0("Iteration ", i,  " of ", dim(ufg_premises)[1]))
    index_premise <- ufg_premises[[i]]
    prod_emp_ufg <- prod(emp_prob[index_premise])
    concl_ufg <- ddandrda::test_porder_in_concl(prep_ufg_premises$list_porder_premises[index_premise], poset_interest) * 1

    depth_ufg <- depth_ufg + concl_ufg * prod_emp_ufg
    constant_c <- constant_c + prod_emp_ufg
  }

  depth_value <- depth_ufg / constant_c

  return(depth_value)

}

# Preparation of computing the ufg premises
prepare_ufg_premises <- function(list_mat_porders_ml,
                                 number_items) {

  fc_ml_porder <- ddandrda::compute_conceptual_scaling(input_porder = list_mat_porders_ml)
  porder_all <- ddandrda::compute_all_partial_orders(number_items, list = FALSE, complemented = TRUE)

  data_context <- get_weighted_representation(fc_ml_porder) # duplication
  n_row_context <- nrow(data_context$x_weighted)
  count_dup <- data_context$counts
  number_obs <- sum(data_context$counts)
  list_porder_premises <- ddandrda::convert_context_to_list(data_context$x_weighted[ ,(1:(number_items * number_items))],  complemented = FALSE)

  whole_context <- rbind(data_context$x_weighted, porder_all) # context of all posets
  index <- which(!duplicated(whole_context))
  whole_context <- whole_context[index,]
  return(list(count_dup = count_dup,
              number_obs = number_obs,
              whole_context = whole_context,
              list_porder_premises = list_porder_premises,
              n_row_context = n_row_context))
}



# Function which converts to partial orders
convert_to_matrix <- function(single_data_eval) {
  list_decoder <- single_data_eval[["decoder"]]
  number_decoder <- length(list_decoder)
  graph_mat <- matrix(rep(0, number_decoder * number_decoder),
                      nrow = number_decoder)
  rownames(graph_mat) <- colnames(graph_mat) <- list_decoder
  diag(graph_mat) <- 1

  store_not_order <- list()

  # single_data_eval <- subset(single_data_eval, select = -c(decoder))
  for (opt_i in seq(1, number_decoder)) {
    learner_base <- single_data_eval[opt_i, ]
    for (opt_j in seq(1, number_decoder)[-opt_i]) {
      learner_comp <- single_data_eval[opt_j, ]
      difference <- as.numeric(learner_base) - as.numeric(learner_comp)
      difference <- difference[!is.na(difference)]
      if (length(difference) == 0) {
        store_not_order <- append(store_not_order, list(list_decoder[opt_i],
                                                        list_decoder[opt_j]))

      } else if (all(difference == 0)) {
        # print(paste0("decoder ", list_decoder[opt_i], " and decoder ",
        #              list_decoder[opt_j], " have both in all components same values."))
        store_not_order <- append(store_not_order, list(list_decoder[opt_i],
                                                        list_decoder[opt_j]))
      } else if (!any(difference < 0)) {
        graph_mat[opt_i, opt_j] <- 1
      }
    }
  }
  return(list(graph_mat, store_not_order))
}


################################################################################
#
#  Part 0: Construction of the Partial Orders
#
################################################################################


# Read the data and convert them to the correct data type
data <- read.csv(file = "results_with_pareto_efficiency.csv",sep = ",", header = TRUE)
data <- data[data[, 4] == data_subset_name, ]

data$model.specification <- paste(data$Model, data$Method)

data <- data[ ,c("id",
                 "model.specification",
                 "Generation.perplexity",
                 "Generation.diversity",
                 "Generation.coherence" )]

# unique(data$model.specification)
data <- data[which(data$model.specification %in% model_subset_name), ]

colnames(data)[c(1, 2)] <- c("funcId", "decoder")


# set all measures such that the higher the better
data$Generation.perplexity <- max(data$Generation.perplexity) - data$Generation.perplexity



# convert the data frame into a list of partial orders (posets)
# we say that a decoding strategy i is better than decoder j if and only if at least one
# performance evaluation states that decoder i is better and every other performance
# measure states that decoder i is not worse than decoder j.
funcId <- sort(unique(data[, 1]))

if (size_subsample < length(funcId)) {
  funcId <- sample(funcId, size_subsample)
}


# we are only using a subset --> sample this one

list_graph <- list()

for (id in funcId) { # warnings here are normal :)
  print(paste0("Now at functionId ", id))
  single_data_eval <- filter(data, funcId == id)
  single_data_eval <- subset(single_data_eval, select = -c(funcId))
  # to apply the convert_to_matrix function it is important that there exists a
  # column with name decoder which lists the decoder of interest
  list_graph <- append(list_graph, list(convert_to_matrix(single_data_eval)))
}


saveRDS(list_graph, paste0(prefix_files, "ufg_list_graph.RDS"))


same_metric_values <- lapply(list_graph, FUN = function(x){x[[2]]})
list_graph <- lapply(list_graph, FUN = function(x){x[[1]]})

# check if everything is correct and we observe posets
no_poset <- list()
i <- 1
for (graph in list_graph) {
  if (!ddandrda::test_if_porder(graph)) {
    no_poset <- append(no_poset, i)
  }
  i <- i + 1
}



if (length(no_poset) != 0) {
  saveRDS(no_poset, paste0(prefix_files, "ufg_no_poset.RDS"))
  list_graph <- list_graph[-unlist(no_poset)]
}

################################################################################
#
# PART 3: COMPUTATION OF UFG DEPHT
#
################################################################################
# Hannah Blocher, Georg Schollmeyer, Christoph Jansen, and Malte Nalenz. Depth functions for
# partial orders with a descriptive analysis of machine learning algorithms. In Enrique Miranda,
# Ignacio Montes, Erik Quaeghebeur, and Barbara Vantaggi (eds.), Proceedings of the Thirteenth
# International Symposium on Imprecise Probability: Theories and Applications, volume 215 of
# Proceedings of Machine Learning Research, pp. 59–71. PMLR, 11–14 Jul 2023.
# and the code provided by
# https://github.com/hannahblo/23_Performance_Analysis_ML_Algorithms (accessed: 07.11.2023)

item_number <- dim(list_graph[[1]])[1]
names_columns <- colnames(list_graph[[1]])

fc_ml_porder <- ddandrda::compute_conceptual_scaling(input_porder = list_graph)
ml_porder_model <- oofos::compute_extent_vc_dimension(fc_ml_porder)
vc_fc_ml_porder <- gurobi::gurobi(ml_porder_model)
vc_fc_ml_porder$objval # 16


# Computation of S, see article (1)
item_number <- dim(list_graph[[1]])[1]
prep_ufg_premises <- prepare_ufg_premises(list_graph, number_items = item_number)
start_time <- Sys.time()
ufg_premises <- oofos::enumerate_ufg_premises(prep_ufg_premises$whole_context, prep_ufg_premises$n_row_context)
total_time <- Sys.time() - start_time

saveRDS(ufg_premises, paste0(prefix_files, "ufg_premises.rds"))
# total_time <- readRDS("part1_total_time_ufg_premises.rds")
# ufg_premises <- readRDS("part1_ufg_premises.rds")
# length(ufg_premises)


# Computation of depth of observed posets
depth_value <- compute_ufg_exist_premises(poset_interest = prep_ufg_premises$list_porder_premises,
                                          ufg_premises,
                                          prep_ufg_premises)










### Plotting
max(depth_value)
sort(depth_value)
length(unique(depth_value)) == length(depth_value)

order(depth_value)
prep_ufg_premises$count_dup



# observed deepest depth values
max_depth_index <- sort(depth_value, index.return = TRUE, decreasing = TRUE)$ix
pdf(paste0(prefix_files, "ufg_plots_from_highest_depth.pdf"), onefile = TRUE)
for (i in max_depth_index) {
  current_poset <- matrix(as.logical(prep_ufg_premises$list_porder_premises[[i]]), ncol = item_number)
  colnames(current_poset) <- rownames(current_poset) <- names_columns
  hasse(t(current_poset), parameters = list(arrow = "backward", shape = "roundrect"))
}
dev.off()

#
#
# # Intersections (high to low) of the observed depth values
# max_depth_index <- sort(depth_premises$depth_ufg, index.return = TRUE, decreasing = TRUE)$ix
# pdf("plots_observed_intersect_from_highes.pdf", onefile = TRUE)
# for (i in 1:max(max_depth_index)) {
#   intersect <- matrix(rep(TRUE, item_number * item_number), ncol = item_number)
#   for (j in seq(1, i)) {
#     intersect <- intersect & matrix(as.logical(list_graph[[max_depth_index[j]]]), ncol = item_number)
#   }
#   colnames(intersect) <- rownames(intersect) <- names_columns
#   hasse(t(intersect), parameters = list(arrow = "backward", shape = "roundrect"))
# }
# dev.off()



