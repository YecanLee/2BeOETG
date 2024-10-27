# Computation of the extended Bradley-Terry model based on WikiText-103

# This code is based on Hannah Blocher, Georg Schollmeyer, Malte Nalenz and
# Christoph Jansen (2024): Comparing Machine Learning Algorithms by Union-Free
# Generic Depth. International Journal of Approximate Reasoning, 169: 1-23.

# This file stores not only the result, but also interim results



################################################################################
# R Session Setup
################################################################################
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
# (Note that the file "results_with_pareto_efficiency.csv" needs to be stored in the same folder)
# setwd()

# Which data set shall be analysed? Choose between "book"  "wikinews" "wikitext"
data_subset_name <-  "wikitext"


# prefix of stored files
prefix_files <- "wikitext"


################################################################################
# Functions needed later
################################################################################


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



# Function which converts a list of posets as matrices (see convert_to_matrix
# function) to format needed for prefmod package (analogous to prefmod::design(prefmod::cemspc))
construct_design_bt <- function(list_graph) {

  heatmap <-  Reduce("+", list_graph)
  cl_names <- colnames(list_graph[[1]])
  df_design_bt <- data.frame(matrix(ncol = 2 + 3 + dim(heatmap)[1], nrow = 0))
  colnames(df_design_bt) <- c("cum_sum", "mu", "pref_a", "undecided", "pref_b", colnames(list_graph[[1]]))
  sum_graphs <- length(list_graph)


  mu <- 1
  for (i in seq_len(length(cl_names) - 1)) {

    for (j in seq(i + 1, length(cl_names))) {
      print(paste0("Now at ", i, " and ", j))
      input <- rep(0, length(cl_names))

      input[c(i,j)] <- c(-1, 1)
      df_design_bt[nrow(df_design_bt) + 1, ] <- c(heatmap[i,j], mu, 1, 0, 0, input)

      input[c(i,j)] <- c(0, 0)
      df_design_bt[nrow(df_design_bt) + 1, ] <- c(sum_graphs - heatmap[i,j] - heatmap[j,i], mu, 0, 1, 0, input)

      input[c(i,j)] <- c(1, -1)
      df_design_bt[nrow(df_design_bt) + 1, ] <- c(heatmap[j,i], mu, 0, 0, 1, input)

      mu <- mu + 1
    }
  }


  return(df_design_bt)
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

colnames(data)[c(1, 2)] <- c("funcId", "decoder")

# set all measures such that the higher the better
data$Generation.perplexity <- max(data$Generation.perplexity) - data$Generation.perplexity

data_test <- data[which(data$decoder %in% c("Mistral 3 CS (('0.2', '1'))",  "Mistral 3 CS (('0.8', '1'))")), ]

# convert the data frame into a list of partial orders (posets)
# we say that a decoding strategy i is better than decoder j if and only if at least one
# performance evaluation states that decoder i is better and every other performance
# measure states that decoder i is not worse than decoder j.
funcId <- sort(unique(data[, 1]))

list_graph <- list()

for (id in funcId) { # warnings here are normal :)
  print(paste0("Now at functionId ", id))
  single_data_eval <- filter(data, funcId == id)
  single_data_eval <- subset(single_data_eval, select = -c(funcId))
  # to apply the convert_to_matrix function it is important that there exists a
  # column with name decoder which lists the decoder of interest
  list_graph <- append(list_graph, list(convert_to_matrix(single_data_eval)))
}


saveRDS(list_graph, paste0(prefix_files, "bt_list_graph.RDS"))

# list_graph <- readRDS("wikitextbt_list_graph.RDS")

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
  saveRDS(no_poset, paste0(prefix_files, "bt_no_poset.RDS"))
  list_graph <- list_graph[-unlist(no_poset)]
}



################################################################################
#
# PART 1: FIRST IMPRESSION
#
################################################################################
number_decoder <- dim(list_graph[[1]])[1]
decoder_interest <- colnames(list_graph[[1]])

# Which edge exists
length(list_graph)
length(unique(list_graph))
any(duplicated(list_graph)) # no duplications exist


# Domince Stucture
edges <- Reduce("+", list_graph)
colnames(edges) <- rownames(edges) <- decoder_interest
df_edge_exist <- melt(edges)


# Get the ones where the most dominances exist and the once that are most incomparable
df_edge_exist <- df_edge_exist[-which(df_edge_exist$Var1 == df_edge_exist$Var2), ]

max_index <- sort(df_edge_exist$value, index.return = TRUE, decreasing = TRUE)
df_edge_exist[max_index$ix[which(max_index$x == max_index$x[1])], ]
# xtable((df_edge_exist[max_index$ix[which(max_index$x == max_index$x[1])], ]))
dim(df_edge_exist[max_index$ix[which(max_index$x > 1314*0.9)], ])



min_index <- sort(df_edge_exist$value, index.return = TRUE, decreasing = FALSE)
df_edge_exist[min_index$ix[which(min_index$x == min_index$x[1])], ]
dim(df_edge_exist[min_index$ix[which(min_index$x == min_index$x[1])], ] )
dim(df_edge_exist[max_index$ix[which(max_index$x < 1314*0.1)], ])

################################################################################
#
# PART 2: EXTENDED BRADLEY TERRY MODEL
#
################################################################################
# Computation of the  Bradley-Terry Model with no convariables, but with ties
# for further informations see:
# - Hatzinger, Dittrich (2012): prefmod: An R Package for Modeling Preferences
#    Based on Paired Comparisons, Rankings, or Ratings
# - Hatzinger, Maier (2023): Package ‘prefmod’
# - Sinclaig (1982): GLIM for Preferences
# - Davidson (1970): On Extending the Bradley-Terry Model to Accommodate Ties in
#    Paired Comparison Experiments

list_graph <- lapply(list_graph, FUN = function(x)(t(x)))

design_mat <- construct_design_bt(list_graph)
saveRDS(design_mat, paste0(prefix_files, "bt_design_mat.RDS"))
name_classifiers <- colnames(design_mat[-c(1, 2, 3, 4, 5)])
name_classifiers <- gsub(" ", "", name_classifiers)
name_classifiers <- gsub("-", "", name_classifiers)
name_classifiers <- gsub(",", "_", name_classifiers)
name_classifiers <- gsub("'", "", name_classifiers)
name_classifiers <- gsub("|", "", name_classifiers)
name_classifiers <- gsub(")", "", name_classifiers)
name_classifiers <- stringr::str_remove_all(name_classifiers,"[(]")



colnames(design_mat)[seq(6, length(colnames(design_mat)))]  <- name_classifiers
name_mu <- list()
for (class_1 in seq(1, length(name_classifiers) - 1)) {
  for (class_2 in seq(class_1 + 1, length(name_classifiers))) {
    name_mu <- append(name_mu, rep(paste0(name_classifiers[class_1], "_", name_classifiers[class_2]), 3))
  }
}

design_mat$mu <- unlist(name_mu)
design_mat$mu <- as.factor(design_mat$mu)


formula_bt <-  as.formula(paste0("cum_sum ~ undecided + ", paste0(name_classifiers, collapse = "+")))
result_glm <- gnm(formula_bt,
                      elim = mu,
                      family = 'poisson', data = design_mat) # loglink is default
saveRDS(result_glm, paste0(prefix_files, "bt_result_glm.RDS"))

# summary(result_glm)
#
# lambdas
lambda <- result_glm$coefficients
lambda <- lambda[seq(2, length(lambda))]
lambda[length(lambda)] <- 0

# worth
division <- sum(unlist(lapply(lambda, function(x){exp(2*x)})))
worth <- lapply(lambda, function(x){exp(2*x)/division})
sort_worth <- sort(unlist(worth), decreasing = TRUE)
sort_worth

data.frame(x = names(sort_worth), y = as.numeric(sort_worth))
print(xtable(data.frame(x = names(sort_worth), y = as.numeric(sort_worth)),
             digits = 50),
      include.rownames = FALSE)
