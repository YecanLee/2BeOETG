# Load necessary libraries
library(dplyr)
library(tidyr)
library(gt)
library(openxlsx)
library(plotly)
library(ggrepel)
library(ggthemes)
library(patchwork)
library(scales)
library(ggplot2)

# Define generic file paths
data_file <- "path/to/results_with_pareto_efficiency.csv"
candidate_stats_file <- "path/to/ranking_qtext.csv"
dominance_summary_file <- "path/to/dominance_final_analysis.csv"

# Function to normalize and winsorize a metric
normalize_metric <- function(metric, lower_pct = 0.05, upper_pct = 0.95, rescale_to = c(0, 100)) {
  lower <- quantile(metric, lower_pct, na.rm = TRUE)
  upper <- quantile(metric, upper_pct, na.rm = TRUE)
  winsorized <- pmin(pmax(metric, lower), upper)
  normalized <- scales::rescale(winsorized, to = rescale_to)
  return(normalized)
}

# Load data
data <- read.csv(data_file)

# Normalize metrics
data <- data %>%
  mutate(
    Normalized.Coherence = normalize_metric(Generation.coherence),
    Normalized.Diversity = normalize_metric(Generation.diversity),
    Inverse.Perplexity = 1 / Generation.perplexity,
    Normalized.Perplexity = normalize_metric(Inverse.Perplexity)
  )

# Adjust sigmoid-based penalty function
apply_sigmoid_penalty <- function(x, threshold = 95, steepness = 5) {
  penalty <- 1 / (1 + exp(-steepness * (x - threshold)))
  penalized_value <- x * (1 - penalty)
  return(max(penalized_value, 0.01))  # Ensure a minimum value
}

# Apply penalty to normalized metrics
data <- data %>%
  mutate(
    Penalized.Coherence = sapply(Normalized.Coherence, apply_sigmoid_penalty),
    Penalized.Diversity = sapply(Normalized.Diversity, apply_sigmoid_penalty),
    Penalized.Perplexity = sapply(Normalized.Perplexity, apply_sigmoid_penalty)
  )

# Function to calculate harmonic mean
calculate_QText <- function(coherence, diversity, perplexity) {
  scores <- na.omit(c(coherence, diversity, perplexity))
  if (length(scores) == 0) return(NA)
  harmonic_mean <- length(scores) * prod(scores) / sum(scores)
  return(harmonic_mean)
}

# Calculate QText
data <- data %>%
  rowwise() %>%
  mutate(
    Unbounded.QText = calculate_QText(Penalized.Coherence, Penalized.Diversity, Penalized.Perplexity),
    QText = scales::rescale(Unbounded.QText, to = c(0, 100))
  ) %>%
  ungroup()

# Create combined identifiers
data <- data %>%
  mutate(
    Dec_Method = paste0(Model, " ", Method),
    ID_Dataset = paste0(id, " ", Dataset)
  )

# Generate dominance summary
dominance_summary <- data %>%
  group_by(ID_Dataset) %>%
  summarize(
    Most_Dominant_Method = Dec_Method[which.max(QText)],
    Max_QText = max(QText, na.rm = TRUE),
    Least_Dominant_Method = Dec_Method[which.min(QText)],
    Min_QText = min(QText, na.rm = TRUE)
  )

# Calculate summary statistics for candidates
candidate_stats <- data %>%
  group_by(Dec_Method) %>%
  summarise(
    mean_QText = mean(QText, na.rm = TRUE),
    median_QText = median(QText, na.rm = TRUE),
    sd_QText = sd(QText, na.rm = TRUE),
    mean_coh = mean(Penalized.Coherence, na.rm = TRUE),
    mean_div = mean(Penalized.Diversity, na.rm = TRUE),
    mean_perp = mean(Penalized.Perplexity, na.rm = TRUE)
  )

# Save candidate statistics to CSV
write.csv(candidate_stats, candidate_stats_file, row.names = FALSE)

# Save dominance summary to CSV
write.csv(dominance_summary, dominance_summary_file, row.names = FALSE)

# View dominance summary
print(dominance_summary)

# Identify top and worst candidates
best_candidates <- candidate_stats %>%
  arrange(desc(mean_QText)) %>%
  slice_head(n = 20)

worst_candidates <- candidate_stats %>%
  arrange(mean_QText) %>%
  slice_head(n = 20)

# Display best and worst candidates
print(best_candidates)
print(worst_candidates)

# Filter data for best and worst candidates
best_candidate_data <- data %>%
  filter(Dec_Method %in% best_candidates$Dec_Method)

worst_candidate_data <- data %>%
  filter(Dec_Method %in% worst_candidates$Dec_Method)

# Boxplot for best candidates
best_plot <- ggplot(best_candidate_data, aes(x = factor(Dec_Method), y = QText)) +
  geom_boxplot(fill = "lightblue") +
  labs(
    title = "Q*Text Distribution for Best Candidates",
    x = "Candidate",
    y = "Q*Text"
  ) +
  theme_economist()

# Boxplot for worst candidates
worst_plot <- ggplot(worst_candidate_data, aes(x = factor(Dec_Method), y = QText)) +
  geom_boxplot(fill = "lightcoral") +
  labs(
    title = "Q*Text Distribution for Worst Candidates",
    x = "Candidate",
    y = "Q*Text"
  ) +
  theme_economist()

# Combine best and worst plots
best_plot / worst_plot

# Violin plots for normalized metrics and QText by Strategy
coh_plot <- ggplot(data, aes(x = Strategy, y = Normalized.Coherence)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Coherence Distribution (Violin Plot) for Strategies",
    x = "Strategy",
    y = "Coherence"
  ) +
  theme_economist()

div_plot <- ggplot(data, aes(x = Strategy, y = Normalized.Diversity)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Diversity Distribution (Violin Plot) for Strategies",
    x = "Strategy",
    y = "Diversity"
  ) +
  theme_economist()

perp_plot <- ggplot(data, aes(x = Strategy, y = Normalized.Perplexity)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Perplexity Distribution (Violin Plot) for Strategies",
    x = "Strategy",
    y = "Perplexity"
  ) +
  theme_economist()

qtext_plot <- ggplot(data, aes(x = Strategy, y = QText, fill = Strategy)) +
  geom_violin() +
  labs(
    title = "Q*Text Distribution (Violin Plot) for Strategies",
    x = "Strategy",
    y = "Q*Text"
  ) +
  theme_economist()

# Combine violin plots
(coh_plot / div_plot) / (perp_plot / qtext_plot)

# Density plot for best candidates
ggplot(best_candidate_data, aes(x = QText, color = factor(Dec_Method))) +
  geom_density(size = 1) +
  labs(
    title = "Q*Text Density for Best Candidates",
    x = "Q*Text",
    color = "Candidate"
  ) +
  theme_economist()

# Density plot for worst candidates
ggplot(worst_candidate_data, aes(x = QText, color = factor(Dec_Method))) +
  geom_density(size = 1) +
  labs(
    title = "Q*Text Density for Worst Candidates",
    x = "Q*Text",
    color = "Candidate"
  ) +
  theme_economist()

# Violin plot for best candidates
ggplot(best_candidate_data, aes(x = factor(Dec_Method), y = QText)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Q*Text Distribution (Violin Plot) for Best Candidates",
    x = "Candidate",
    y = "Q*Text"
  ) +
  theme_economist()

# Violin plot for worst candidates
ggplot(worst_candidate_data, aes(x = factor(Dec_Method), y = QText)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Q*Text Distribution (Violin Plot) for Worst Candidates",
    x = "Candidate",
    y = "Q*Text"
  ) +
  theme_economist()

# Overall Q*Text distribution per Strategy
ggplot(data, aes(x = factor(Strategy), y = QText)) +
  geom_violin(fill = "lightgreen") +
  labs(
    title = "Q*Text Distribution (Violin Plot) per Strategy",
    x = "Strategy",
    y = "Q*Text"
  ) +
  theme_economist()

# View summary statistics
summary(candidate_stats)
summary(data)

# End of Script
