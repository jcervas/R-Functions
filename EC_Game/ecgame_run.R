# ecgame_run.R — Cleaned and Modularized Execution Script

# ========== 1. Setup ==========
rm(list = ls(all = TRUE))                            # Clear environment
options(scipen = 999)                                 # Avoid scientific notation
quartzFonts(
  palatino = c("Palatino", "Palatino-Italic", "Palatino-Bold", "Palatino-BoldItalic")
)
set.seed(66)                                          # Reproducibility

source('/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/GitHub/R-Functions/EC_Game/colonial_blotto.R')
figure_directory <- '/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Academic/Working Papers/EC Game/figures/'

# Electoral College weights and quota
ec_weights <- c(3, 5, 8, 13, 21, 34, 55)
quota <- 70
state_names <- LETTERS[1:7]
state_labels <- paste("State", state_names)
names(ec_weights) <- state_names


# ========== 2. Coalition Analysis ==========

ec_Banzhaf <- banzhaf(names(ec_weights), ec_weights, quota)     # Compute Banzhaf index
mwcs <- find_mwcs(vector_weights = ec_weights, quota = quota)   # Identify minimal winning coalitions (MWCs)


# ========== 3. User Data ==========

user_data <- jsonlite::read_json('/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Academic/Working Papers/EC Game/data/results.json')
participant_matrix_raw <- as.matrix(do.call(rbind, lapply(user_data, unlist)))
participant_matrix <- deduplicate_combinations(participant_matrix_raw)

participant_matrix_adj <- t(apply(participant_matrix, 1, function (x) apportion_values(x, target_sum = 100)))

participant_results <- evaluate_strategies(participant_matrix_adj, ec_weights, 70)

# Rename and clean up column names for publication
participant_summary_table <- participant_results$position_summary
colnames(participant_summary_table) <- c(
  "State", "Zeroes", "Zero (%)", "Mean", "Median", 
  "SD", "Min", "Max"
)

# Replace State positions with labels A–G
participant_summary_table$State <- LETTERS[1:7]

# Print table in markdown
knitr::kable(participant_summary_table, format = "simple", align = "c",
      caption = "Summary of Allocation Statistics by State (N = 214)")

# Hypothesis 1: Predictive Power of Electoral Weight vs. Voting Power

user_means <- apply(participant_matrix_adj, 2, mean)
user_medians <- apply(participant_matrix_adj, 2, median)

user_table <- t(rbind(
  Participant_Average = round(user_means,1),
  Participant_Median = user_medians
  ))
rownames(user_table) <- state_labels

# ==== Table x: Participant Average and Median Allocations, by State ===== #
knitr::kable(user_table, format = "simple",
             caption = "Participant Average and Median Allocations, by State")

# === Figure x:  Participant allocations by state ====== #

# Stripchart of participant allocations by state
pdf(paste0(figure_directory, "mean-alloc-strip.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)
stripchart(as.data.frame(participant_matrix_adj),
           vertical = TRUE, method = "jitter",
           pch = 16, col = "gray", xaxt = "n",
           main = "Participant Allocations by State",
           ylab = "Allocation", xlab = "State",
           ylim = c(0, 100), cex = 0.5)

# Add x-axis labels
axis(1, at = 1:7, labels = state_names)

# Add horizontal reference lines
abline(h = c(25, 50, 75), col = "lightgray", lty = 2)

# Add red points for medians
medians <- apply(participant_matrix_adj, 2, median)
points(1:7, medians, col = "red", pch = 19)

# Optional: add legend
legend("topleft", legend = "Median", col = "red", pch = 19, bty = "n")

dev.off()
# Note: Participant allocations by state. Gray dots show individual allocations; red dots indicate the median per state. Horizontal reference lines at 25, 50, and 75 units aid visual comparison.


# Compute normalized summary statistics for user strategies
proportional_vector <- normalize(ec_weights)
banzhaf_vector <- normalize(ec_Banzhaf)
user_means_norm <- normalize(user_means)
user_medians_norm <- normalize(user_medians)

strategies_matrix <- t(rbind(
  proportional_vector,
  banzhaf_vector,
  user_means_norm,
  user_medians_norm
))

colnames(strategies_matrix) <- c("Vote-Share Weights", "Banzhaf", "Avg Allocation", "Median Allocation")
rownames(strategies_matrix) <- LETTERS[1:7]

# Bar plot to compare strategy centroids <<Figure : Vote Weights vs. Power Indices and Allocation Outcomes>>
pdf(paste0(figure_directory, "mean-alloc.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)

barplot(
  t(strategies_matrix) * 100,
  beside = TRUE,
  col = gray.colors(4, start = 0.1, end = 0.9),
  ylim = c(0, 50),
  ylab = "",
  xlab = "",
  yaxt = "n",
  names.arg = rownames(strategies_matrix),
  legend.text = colnames(strategies_matrix),
  args.legend = list(x = "topleft", bty = "n", inset = 0.01, x.intersp = 0.5, y.intersp = 0.8),
  las = 1
)

axis(2, at = seq(0, 50, by = 10), labels = paste0(seq(0, 50, by = 10), "%"), las = 1)

dev.off()


# Compare average and median user allocations to EC weights and Banzhaf power

model_means_ec <- lm(user_means_norm ~ proportional_vector)
model_medians_ec <- lm(user_medians_norm ~ proportional_vector)
model_means_bz <- lm(user_means_norm ~ banzhaf_vector)
model_medians_bz <- lm(user_medians_norm ~ banzhaf_vector)

summary(model_means_ec)
summary(model_medians_ec)
summary(model_means_bz)
summary(model_medians_bz)


# Define function to extract stats from each model
extract_model_info <- function(model, predictor_name, outcome_name, model_label) {
  summary_model <- summary(model)
  adj_r2 <- round(summary_model$adj.r.squared, 2)
  aic_val <- round(AIC(model), 2)
  t_val <- round(summary_model$coefficients[2, "t value"], 2)
  
  data.frame(
    Model = model_label,
    Predictor = predictor_name,
    Outcome = outcome_name,
    `Adj. R²` = adj_r2,
    AIC = aic_val,
    `t-value` = t_val,
    check.names = FALSE
  )
}

# Combine the models into a markdown-ready table
comparison_table <- rbind(
  extract_model_info(model_means_ec, "EC Weights", "Mean Allocation", "Model 1"),
  extract_model_info(model_means_bz, "Banzhaf Power", "Mean Allocation", "Model 2"),
  extract_model_info(model_medians_ec, "EC Weights", "Median Allocation", "Model 3"),
  extract_model_info(model_medians_bz, "Banzhaf Power", "Median Allocation", "Model 4")
)

# Use knitr to print as markdown table
knitr::kable(comparison_table, format = "simple",
             caption = "Comparison of Regression Models: EC Weights vs. Banzhaf Power")


# Compare MSEs to evaluate which benchmark (EC or Banzhaf) user allocations are closer to
user_mse_ec <- apply(participant_matrix_adj, 1, function(x) mean((normalize(x) - normalize(proportional_vector))^2))
user_mse_banzhaf <- apply(participant_matrix_adj, 1, function(x) mean((normalize(x) - normalize(banzhaf_vector))^2))
# Run the t-test
tt <- t.test(user_mse_ec, user_mse_banzhaf, paired = TRUE)

# Extract values
t_val <- round(tt$statistic, 2)
df <- tt$parameter
p_val <- tt$p.value
ci <- round(tt$conf.int, 3)

# Format p-value
p_str <- if (p_val < 0.001) "< 0.001" else paste0("= ", signif(p_val, 3))

# Print result using cat()
cat("To further evaluate predictive accuracy, we computed the mean squared error (MSE) for each participant’s strategy relative to both benchmarks. A paired t-test showed that participant strategies were significantly closer to the Banzhaf benchmark,")
cat(sprintf("t(%d) = %.2f, p %s, 95%% CI = [%.3f, %.3f].",
            df, t_val, p_str, ci[1], ci[2]))


# Chi-square test for frequency of closer alignment
closer_to_banzhaf <- sum(user_mse_banzhaf < user_mse_ec)
closer_to_ec <- sum(user_mse_ec < user_mse_banzhaf)

# Run the chi-squared test
freq_table <- c(Banzhaf = closer_to_banzhaf, EC = closer_to_ec)
cs <- chisq.test(freq_table)

# Extract values
chi_sq <- round(cs$statistic, 2)
df <- cs$parameter
p_val <- cs$p.value

# Format p-value
p_str <- if (p_val < 0.001) "< 0.001" else paste0("= ", signif(p_val, 3))

# Total N
N <- sum(freq_table)

# Print result
cat("As a complementary categorical test, we classified each participant based on which benchmark their strategy more closely resembled. A chi-squared test showed that significantly more participants aligned with the Banzhaf benchmark than would be expected by chance:")
cat(sprintf("χ²(%d, N = %d) = %.2f, p %s.", df, N, chi_sq, p_str))


# ===========================================================
# Hypothesis 2a: Are user strategies clustered around MWCs?

strict_assignments <- assign_strict_mwc(participant_matrix_adj, mwcs)
mwc_labels <- sapply(mwcs, function(coal) paste(state_names[coal], collapse = ","))

# Force all 10 MWC levels plus NA to appear
counts <- table(factor(strict_assignments, levels = c(1:10, NA)), useNA = "ifany")

names(counts) <- c(mwc_labels, "Other") # Assign readable names

clustered <- sum(counts[!is.na(names(counts))])
total <- sum(counts)
percent_clustered <- round(clustered / total * 100, 1)
percent_non_clustered <- round(counts[is.na(names(counts))] / total * 100, 1)

cat("We find that", percent_clustered, "% of all simulated strategies allocate resources exclusively to the members of a single minimal winning coalition (MWC), with no allocation to any other states. This provides moderate support for Hypothesis 2. While a minority of strategies exhibit strict clustering around MWCs, the majority (", percent_non_clustered, "%) distribute resources more broadly—suggesting that while MWCs influence behavior, strategic diversity remains high.")

df <- as.data.frame(counts)
names(df) <- c("MWC", "Participants")

# ==== Table X: Participant counts per MWC match ======== #
knitr::kable(df, format = "simple", caption = "Participant counts per MWC match")

# ===========================================================
# Hypothesis 2b: Do users favor small coalitions (few states)?
active_states <- rowSums(participant_matrix_adj > 0)
active_states_table <- table(active_states)

# Prepare data
active_states <- rowSums(participant_matrix_adj > 0)
active_states_table <- table(active_states)
freq <- as.vector(active_states_table)

# Percentages
percent <- round(100 * freq / sum(freq), 1)

# Create table
df <- rbind(Number_of_Participants = freq, Percentage = percent)
colnames(df) <- 1:7


# ==== Table X: Number and Percentage of Strategies Allocating to Each Number of States (A–G) ===== #
knitr::kable(df, format= "simple", caption = "Number and Percentage of Strategies Allocating to Each Number of States (A–G)")


# Variation
var_active <- var(active_states)

# Shannon entropy to measure strategy diversity
p <- table(active_states) / length(active_states)
entropy <- -sum(p * log2(p))/log2(length(p))

cat("• Variance of active state count: ", round(var_active,2), "\n• Shannon entropy: ", round(entropy, 2), " (normalized)")

# Hypothesis 3a: Preference for State F over E
F_mean <- mean(participant_matrix_adj[,6])  # State F
E_mean <- mean(participant_matrix_adj[,5])  # State E
wtest <- wilcox.test(participant_matrix_adj[,6], participant_matrix_adj[,5], paired = TRUE)

ttest <- t.test(participant_matrix_adj[,6], participant_matrix_adj[,5])

cat("• Mean allocation to State F: ", round(F_mean, 2), "\n• Mean allocation to State E: ", round(E_mean, 2))
cat(sprintf("V = %.1f, p = %.5f\n", wtest$statistic, wtest$p.value))
cat(sprintf("p-value = %.3f\n", ttest$p.value))


# Hypothesis 3b: Preference for State D over E

# Mean allocations for all states
D_mean <- mean(participant_matrix_adj[,4])  # State D

# Compare State D (column 4) and State E (column 5)
cat("Mean allocation to State D:", D_mean, "\n")
cat("Mean allocation to State E:", E_mean, "\n")


pdf(paste0(figure_directory, "participant-alloc-distributions.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)
stacked_hist_plot(participant_matrix_adj, main = "State Allocation Distributions", show_density = FALSE)
dev.off()

wtest <- wilcox.test(participant_matrix_adj[,5], participant_matrix_adj[,4], paired = TRUE)

ttest <- t.test(participant_matrix_adj[,5], participant_matrix_adj[,4])

cat("• Mean allocation to State D: ", round(D_mean, 2), "\n• Mean allocation to State E: ", round(E_mean, 2))
cat(sprintf("V = %.1f, p = %.5f\n", wtest$statistic, wtest$p.value))
cat(sprintf("p-value = %.3f\n", ttest$p.value))


# === Coalition Logic Analysis: State D vs State E ===

# Logical flags for who allocated to D or E
gave_D <- participant_matrix_adj[, 4] > 0  # State D = column 4
gave_E <- participant_matrix_adj[, 5] > 0  # State E = column 5

# Subset allocations to small states: A (1), B (2), C (3)
alloc_A_to_C <- participant_matrix_adj[, 1:3]

# --- Mean allocations to A–C by D-allocation status ---
mean_A_to_C_D <- colMeans(alloc_A_to_C[gave_D, ])
mean_A_to_C_noD <- colMeans(alloc_A_to_C[!gave_D, ])

# --- Mean allocations to A–C by E-allocation status ---
mean_A_to_C_E <- colMeans(alloc_A_to_C[gave_E, ])
mean_A_to_C_noE <- colMeans(alloc_A_to_C[!gave_E, ])

# --- Display summary ---
cat("Mean allocations to small states (A–C) by allocation group:\n\n")
coalition_support_df <- data.frame(
  State = c("A", "B", "C"),
  Gave_D = round(mean_A_to_C_D, 2),
  No_D = round(mean_A_to_C_noD, 2),
  Gave_E = round(mean_A_to_C_E, 2),
  No_E = round(mean_A_to_C_noE, 2)
)
print(coalition_support_df)

# --- Optional: t-tests for each state ---
cat("\nT-tests for small states by D allocation:\n")
for (i in 1:3) {
  test_result <- t.test(alloc_A_to_C[gave_D, i], alloc_A_to_C[!gave_D, i])
  cat(sprintf("State %s: p = %.4f\n", LETTERS[i], test_result$p.value))
}

cat("\nT-tests for small states by E allocation:\n")
for (i in 1:3) {
  test_result <- t.test(alloc_A_to_C[gave_E, i], alloc_A_to_C[!gave_E, i])
  cat(sprintf("State %s: p = %.4f\n", LETTERS[i], test_result$p.value))
}


# Hypothesis 4: Rationality check: does each strategy meet the quota?
eval_sample_strat <- evaluate_strategies(participant_matrix_adj, ec_weights, quota = quota)

# Endowment violations
below_endow <- sum(1 * rowSums(participant_matrix) < 100)
above_endow <- sum(1 * rowSums(participant_matrix) > 100)

cat(length(eval_sample_strat $ec_coverage)-eval_sample_strat$rationality_rate/100 * length(eval_sample_strat$ec_coverage), "submitted strategies that relied on tie-breaking outcomes to reach the 70-vote majority threshold")
cat("An additional", below_endow + above_endow, "participants submitted resource allocations that did not sum to the required 100 units:",  below_endow,"were under-allocated, and", above_endow, "exceeded the budget.")
cat("# relying on tie-breaking cointoss:", nrow(participant_matrix_raw)-nrow(participant_matrix_adj))



# ========== Strategic Effectiveness of Allocation Rules

# ----- Define Strategies -----

# Electoral weights and Banzhaf (assumed already defined: ec_weights, ec_Banzhaf)
strategy_ec         <- apportion_values(ec_weights, 100)
strategy_ec2        <- apportion_values(ec_weights^2, 100)
strategy_banzhaf    <- apportion_values(ec_Banzhaf, 100)
strategy_banzhaf2   <- apportion_values(ec_Banzhaf^2, 100)
strategy_proportional <- apportion_values(c(rep(1/7,5),rep(1/6.999,2)), 100)
# Combine strategies
all_strategies <- rbind(
  strategy_ec,
  strategy_ec2,
  strategy_banzhaf,
  strategy_banzhaf2,
  strategy_proportional
)
rownames(all_strategies) <- c("ec", "ec2", "banzhaf", "banzhaf2", "proportional")

# ----- Run Evaluation -----

# Compare each strategy against participant pool
all_strategies_results <- lapply(1:nrow(all_strategies), function(i) {
  blotto_compare(participant_matrix, game_weights = ec_weights, tie_method = "p2wins", single_strategy = all_strategies[i, ])
})
names(all_strategies_results) <- rownames(all_strategies)

# Extract win counts
win_counts <- sapply(all_strategies_results, function(x) x$Win_Count)
win_rates <- sapply(all_strategies_results, function(x) x$Win_Percentage)

# ----- Summarize -----

summary_df <- do.call(rbind, lapply(all_strategies_results, function(x) as.data.frame(x)))
summary_df <- cbind(Strategy = rownames(all_strategies), summary_df)
print(summary_df)

# ----- Hypothesis Tests (One-Sided) -----

cat("\nEC² vs EC:\n")
print(prop.test(x = c(win_counts["ec2"], win_counts["ec"]), n = c(214, 214), alternative = "greater"))

cat("\nBanzhaf² vs Banzhaf:\n")
print(prop.test(x = c(win_counts["banzhaf2"], win_counts["banzhaf"]), n = c(214, 214), alternative = "greater"))


# Table
strategy_labels <- c("EC", "EC²", "Banzhaf", "Banzhaf²", "Proportional")

df <- data.frame(Strategy = strategy_labels, `Win %` = paste0(round(win_rates,1),'%'), check.names = FALSE)

knitr::kable(df, format = "simple", caption = "Win rates for deterministic strategies under p2wins tie-breaking", row.names = FALSE)





# ========== Strategy Typologies and Allocation Patterns ==========
k = 5
# Perform clustering
kmeans_participant <- kmeans(participant_matrix_adj, centers = k, nstart = 10)

centroids_k5 <- kmeans_participant$centers
colnames(centroids_k5) <- paste("State", LETTERS[1:7])
rownames(centroids_k5) <- paste("Cluster", 1:5)

centroid_table <- as.data.frame(round(centroids_k5, 2))
centroid_table$n <- as.vector(table(kmeans_participant$cluster))

knitr::kable(centroid_table, format = "simple", caption = "Allocation Profiles by Cluster (k = 5) with Avg Win %")


# ========== Testing Clusters against Participants ==========

# Extract strategies only (drop 'n' column)
cluster_strategies <- as.matrix(centroid_table[, 1:7])

# Run comparisons
cluster_results <- lapply(1:5, function(i) {
  blotto_compare(matrix_data = participant_matrix_adj,
                 game_weights = ec_weights,
                 single_strategy = apportion_values(cluster_strategies[i, ],100))
})

# Extract results into a data frame
cluster_summary <- data.frame(
  Cluster = paste0("Cluster ", 1:5),
  Win_Count = sapply(cluster_results, function(x) x$Win_Count),
  Opponents_Faced = sapply(cluster_results, function(x) x$Opponents_Faced),
  Win_Percentage = sapply(cluster_results, function(x) x$Win_Percentage)
)

# === Table : Performance of Cluster Centroids Against Participant Strategies in Blotto Game ==== #
knitr::kable(cluster_summary, format = "simple", caption = "Performance of Cluster Centroids Against Participant Strategies in Blotto Game")


# # ========== 1. Round-Robin Comparison of Participant Strategies ==========

compare_coinflip_df <- as.data.frame(blotto_compare(participant_matrix_adj, ec_weights, tie_method = "cointoss"))
compare_p2wins_df   <- as.data.frame(blotto_compare(participant_matrix_adj, ec_weights, tie_method = "p2wins"))

# # ========== 2. Top 10 Strategies by Rule ==========
colnames(compare_coinflip_df)[1:7] <- colnames(compare_p2wins_df)[1:7] <- LETTERS[1:7]

top_coinflip <- head(compare_coinflip_df[order(compare_coinflip_df$Rank), ], 10)
top_p2wins   <- head(compare_p2wins_df[order(compare_p2wins_df$Rank), ], 10)

cols <- c(LETTERS[1:7], "Win_Count", "Win_Percentage")
top_both <- rbind(
  cbind(Tie_Break = "Coin Flip", top_coinflip[, cols]),
  cbind(Tie_Break = "Player 2 Wins", top_p2wins[, cols])
)

knitr::kable(top_both, format = "simple", row.names = FALSE,
             caption = "Top 10 Strategies by Tie-Breaking Rule")

# ========== 3. Strategy Rank Stability ==========
rank_cf <- rank(-compare_coinflip_df$Win_Count, ties.method = "first")
rank_p2 <- rank(-compare_p2wins_df$Win_Count, ties.method = "first")
names(rank_cf) <- rownames(compare_coinflip_df)
names(rank_p2) <- rownames(compare_p2wins_df)

cf_top <- rank_cf <= floor(length(rank_cf) / 2)
p2_top <- rank_p2 <= floor(length(rank_p2) / 2)

moved_up_ids   <- names(rank_cf)[!cf_top & p2_top]
moved_down_ids <- names(rank_cf)[cf_top & !p2_top]

common_ids <- intersect(names(rank_cf), names(rank_p2))
x <- rank_cf[common_ids]; y <- rank_p2[common_ids]

point_color <- rep("gray70", length(x))
names(point_color) <- names(x)
point_color[moved_up_ids]   <- "blue"
point_color[moved_down_ids] <- "red"

plot(x, y, pch = 21, bg = point_color, col = "black",
     xlab = "Rank under Coin Flip", ylab = "Rank under Player 2 Wins",
     main = "Rank Stability with Movers Highlighted")
abline(a = 0, b = 1, col = "darkgray", lty = 2); grid()
legend("bottomright", legend = c("Stable", "Moved Up", "Moved Down"),
       pt.bg = c("gray70", "blue", "red"), pch = 21, title = "Strategy Type")

compare_coinflip_df <- compare_coinflip_df[common_ids, ]
compare_p2wins_df   <- compare_p2wins_df[common_ids, ]

rank_cf <- rank(-compare_coinflip_df$Win_Count, ties.method = "first")
rank_p2 <- rank(-compare_p2wins_df$Win_Count, ties.method = "first")
names(rank_cf) <- rownames(compare_coinflip_df)
names(rank_p2) <- rownames(compare_p2wins_df)

avg_rank_up_cf   <- mean(rank_cf[moved_up_ids], na.rm = TRUE)
avg_rank_up_p2   <- mean(rank_p2[moved_up_ids], na.rm = TRUE)
avg_rank_down_cf <- mean(rank_cf[moved_down_ids], na.rm = TRUE)
avg_rank_down_p2 <- mean(rank_p2[moved_down_ids], na.rm = TRUE)

cat("\u2192 Movers (Up): Avg Rank under coin flip:", round(avg_rank_up_cf, 1),
    "| under Player 2 wins:", round(avg_rank_up_p2, 1), "\n")
cat("\u2192 Movers (Down): Avg Rank under coin flip:", round(avg_rank_down_cf, 1),
    "| under Player 2 wins:", round(avg_rank_down_p2, 1), "\n")

# ========== 4. Transition Table ==========
cf_top <- rank_cf <= floor(nrow(compare_coinflip_df) / 2)
p2_top <- rank_p2 <= floor(nrow(compare_p2wins_df) / 2)

transition_table <- table(cf_top, p2_top)
cat("\u2192 Strategies moving from bottom to top half:", sum(!cf_top & p2_top), "\n")
cat("\u2192 Strategies moving from top to bottom half:", sum(cf_top & !p2_top), "\n")
print(transition_table)

# ========== 5. Allocation Profiles ==========
stable_high_ids <- rownames(compare_coinflip_df)[cf_top & p2_top]
stable_low_ids  <- rownames(compare_coinflip_df)[!cf_top & !p2_top]
mover_ids       <- rownames(compare_coinflip_df)[xor(cf_top, p2_top)]

mean_high  <- colMeans(participant_matrix_adj[stable_high_ids, , drop = FALSE])
mean_low   <- colMeans(participant_matrix_adj[stable_low_ids, , drop = FALSE])
mean_mover <- colMeans(participant_matrix_adj[mover_ids, , drop = FALSE])

print(round(mean_high, 2))
print(round(mean_mover, 2))
print(round(mean_low, 2))

# ========== 6. MWC Coverage by Winning Strategies ==========
winning_ids   <- rownames(compare_p2wins_df)[compare_p2wins_df$Win_Percentage > 50]
winning_allocs <- participant_matrix_adj[winning_ids, , drop = FALSE]

count_mwcs_covered <- function(strategy_row, mwcs) {
  sum(sapply(mwcs, function(coal) all(strategy_row[coal] > 0)))
}

mwc_counts <- apply(winning_allocs, 1, count_mwcs_covered, mwcs = mwcs)

cat("\u2192 Mean number of MWCs covered by net-winning strategies:", round(mean(mwc_counts), 2), "\n")
cat("\u2192 Distribution:\n")
print(table(mwc_counts))


# # ========== 3.  The Strategic Superiority of State G ==========

# label_f_g_heavy <- function(mat, f_col = 6, g_col = 7, threshold = 50) {
#   apply(mat, 1, function(row) {
#     f <- row[f_col]
#     g <- row[g_col]
#     if (f >= threshold && g < threshold) {
#       return("F-Heavy")
#     } else if (g >= threshold && f < threshold) {
#       return("G-Heavy")
#     } else {
#       return("Other")
#     }
#   })
# }
# label_fg_or <- function(label_vector) {
#   ifelse(label_vector %in% c("F-Heavy", "G-Heavy"), "F-or-G-Heavy", "Other")
# }

# results <- blotto_compare(participant_matrix_adj, game_weights = ec_weights, tie_method = "p2wins")
# results$FG_Label <- label_fg_bias(as.matrix(results[, 1:7]))


# results$FG_Label <- label_f_g_heavy()
# results$FG_Or_Label <- label_fg_or(results$FG_Label)

# boxplot(Win_Percentage ~ FG_Label, data = results,
#         main = "Win % by Strategy Type (F vs G)",
#         ylab = "Win Percentage", xlab = "Strategy Type")


# # Compute core stats
# means <- tapply(results$Win_Percentage, results$FG_Label, mean)
# sds   <- tapply(results$Win_Percentage, results$FG_Label, sd)
# ns    <- tapply(results$Win_Percentage, results$FG_Label, length)
# ses   <- sds / sqrt(ns)
# ci_lowers <- means - 1.96 * ses
# ci_uppers <- means + 1.96 * ses
# cis <- sprintf("[%.1f%%, %.1f%%]", ci_lowers, ci_uppers)

# summary_df <- data.frame(
#   Strategy_Type = as.character(names(means)),
#   Mean_Win_Percent = round(means, 1),
#   CI_95 = cis,
#   N = as.integer(ns)
# )

# summary_df <- summary_df[order(summary_df$Strategy_Type), ]

# knitr::kable(summary_df,
#               format = "simple",
#              align = "lccc",
#              caption = "Win Percentages by Strategy Type",
#              row.names = FALSE)








# # ========== 7. Coin Flip Performance for Structurally Non-Viable Strategies ==========
# p2_loss_ids <- rownames(compare_p2wins_df)[compare_p2wins_df$Win_Percentage == 0]
# coinflip_results <- compare_coinflip_df[p2_loss_ids, ]

# mean_win <- mean(coinflip_results$Win_Percentage)
# max_win  <- max(coinflip_results$Win_Percentage)
# min_win  <- min(coinflip_results$Win_Percentage)

# cat(sprintf("\u2713 Mean win rate under coin flip (among structurally non-viable strategies): %.2f%%\n", mean_win))
# cat(sprintf("\u2713 Range: %.2f%% to %.2f%%\n", min_win, max_win))

# # ========== 8. Margin Analysis for Top Strategy ==========
# top_id <- rownames(compare_p2wins_df)[which.min(rank_p2)]
# top_alloc <- participant_matrix_adj[top_id, , drop = FALSE]

# compute_margin_over_threshold <- function(p1, p2, weights, threshold = 70) {
#   win_matrix <- p2 < p1
#   tie_matrix <- p2 == p1
#   win_points <- sum(weights[win_matrix])
#   margin_over <- win_points - threshold
#   return(margin_over)
# }

# margins_over_70 <- sapply(rownames(participant_matrix_adj), function(id) {
#   if (id == top_id) return(NA)
#   compute_margin_over_threshold(top_alloc, participant_matrix_adj[id, ], ec_weights)
# }, USE.NAMES = FALSE)

# mean_margin_over <- mean(margins_over_70[margins_over_70 > 0], na.rm = TRUE)

# cat(sprintf("\u2713 Avg margin *above threshold* for wins: %.2f electoral votes\n", mean_margin_over))




# ========== 13. ENCM (Effective Number of Coalition Members) Analysis ==========

encm_values <- apply(participant_matrix_adj, 1, calc_encm)

# Summary stats for all strategies
summary(encm_values)

# Compare by win group
win_group <- ifelse(compare_p2wins_df$Win_Percentage > 50, ">50%", "\u226450%")
encm_grouped <- data.frame(ENCM = encm_values, Win_Group = win_group)

group_levels <- unique(win_group)
summary_matrix <- sapply(group_levels, function(g) {
  vals <- encm_grouped$ENCM[encm_grouped$Win_Group == g]
  c(Mean = mean(vals), Median = median(vals), SD = sd(vals),
    Min = min(vals), Max = max(vals))
})

summary_table <- round(t(summary_matrix), 2)
knitr::kable(summary_table, format = "simple",
             caption = "Effective Number of Coalition Members by Win Group")

# ========== 14. Logistic Regression: Win ~ ENCM ==========

win_flag <- as.integer(compare_p2wins_df$Win_Percentage > 50)
encm_logit <- glm(win_flag ~ encm_values, family = binomial)
summary(encm_logit)

# ========== 15. Match-Level Logistic Regression ==========

strategy_ids <- rownames(participant_matrix_adj)
encm_lookup <- setNames(encm_values, strategy_ids)

results <- data.frame(Winner = character(), Loser = character(), ENCM = numeric(), Outcome = integer(), stringsAsFactors = FALSE)

for (i in seq_along(strategy_ids)) {
  for (j in seq_along(strategy_ids)) {
    if (i == j) next
    p1 <- participant_matrix_adj[i, ]
    p2 <- participant_matrix_adj[j, ]
    win_points <- sum(ec_weights[p2 < p1])
    outcome <- as.integer(win_points > 70)
    results <- rbind(results, data.frame(
      Winner = strategy_ids[i],
      Loser = strategy_ids[j],
      ENCM = encm_lookup[strategy_ids[i]],
      Outcome = outcome
    ))
  }
}

logit_dyadic <- glm(Outcome ~ ENCM, family = binomial, data = results)
summary(logit_dyadic)

# ========== 16. ENCM Prediction Plot ==========

encm_seq <- seq(min(results$ENCM), max(results$ENCM), length.out = 100)
pred_frame <- data.frame(ENCM = encm_seq)
pred <- predict(logit_dyadic, newdata = pred_frame, type = "link", se.fit = TRUE)
fit <- pred$fit
se <- pred$se.fit

prob <- plogis(fit)
upper <- plogis(fit + 1.96 * se)
lower <- plogis(fit - 1.96 * se)

plot(encm_seq, prob, type = "l", lwd = 2, col = "blue",
     ylim = c(0, 1),
     xlab = "Effective Number of Coalition Members (ENCM)",
     ylab = "Predicted Win Probability",
     main = "ENCM and Probability of Winning a Matchup")
polygon(c(encm_seq, rev(encm_seq)), c(lower, rev(upper)),
        col = adjustcolor("blue", alpha.f = 0.2), border = NA)
set.seed(42)
jitter_y <- jitter(results$Outcome, factor = 0.1)
points(results$ENCM, jitter_y, pch = 20, col = adjustcolor("black", alpha.f = 0.05))
grid()

# Logistic regression summary output
stargazer::stargazer(logit_dyadic, type = "text",
  title = "Logistic Regression: Match-Level Win Outcome ~ ENCM",
  digits = 3,
  header = FALSE,
  dep.var.labels = "Win Outcome",
  covariate.labels = c("ENCM"),
  star.cutoffs = c(0.05, 0.01, 0.001))





# ========== 3. Strategy Generator ==========

methods_ec <- c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric")

generate_sample_set <- function(vector_weights, methods, n = 10000) {
  setNames(lapply(methods, function(m) {
    t(replicate(n, new_combinations(vector_weights = vector_weights, target_sum = 100, method = m)))
  }), methods)
}

all_random_samples_ec <- generate_sample_set(ec_weights, methods_ec)
all_random_samples_banzhaf <- generate_sample_set(ec_Banzhaf, methods_ec)
all_random_samples_equal <- generate_sample_set(rep(1/7, 7), methods_ec)

# ========== 4. Combine All Samples ==========

all_random_samples_raw <- rbind(
  do.call(rbind, all_random_samples_ec),
  do.call(rbind, all_random_samples_banzhaf),
  do.call(rbind, all_random_samples_equal)
)

cat("✓ Total Number of Random Samples Generated:", nrow(all_random_samples_raw), "\n")
all_random_samples <- unique(all_random_samples_raw)
cat("Original:", nrow(all_random_samples_raw), "→ Unique:", nrow(all_random_samples), "\n")

# ========== 5. MWC Sampling ==========

mwc_samples <- sample_mwc_distributions(
  mwcs, vector_weights = ec_weights, target_sum = 100, total_samples_goal = 1e6
)
all_mwc_samples <- do.call(rbind, mwc_samples)
cat("✓ Total Number of MWC Samples Generated:", nrow(all_mwc_samples), "\n")


# ========== 6. Sample Validation ==========

validate_sampling_methods(ec_weights, methods_ec)

# ========== 7. Visualization ==========

par(mfrow = c(1, 3))
stacked_hist_plot(all_random_samples, main = "All Random Simulations", show_density = TRUE)
stacked_hist_plot(all_mwc_samples, main = "MWC Random Distributions", show_density = TRUE)
stacked_hist_plot(rbind(all_mwc_samples, all_random_samples), main = "Full Sample", show_density = TRUE)

# ========== 8. Strategy Evaluation and Round Robin ==========

all_mwc_results <- blotto_compare(all_mwc_samples, game_weights = ec_weights, tie_method = "p2wins")

cat("\n🏆 Top 10 Strategies:\n")
print(head(all_mwc_results, 10))

cat("\n🔻 Bottom 10 Strategies:\n")
print(tail(all_mwc_results, 10))

cat("\n📊 Summary Stats:\n")
cat("→ Mean Win %:", round(mean(all_mwc_results$Win_Percentage), 2), "\n")
cat("→ Max Win %:", max(all_mwc_results$Win_Percentage), "\n")
cat("→ Min Win %:", min(all_mwc_results$Win_Percentage), "\n")
cat("→ # of Unique Ranks:", length(unique(all_mwc_results$Rank)), "\n")

sample_strategies_raw <- all_random_samples[sample(nrow(all_random_samples), 100000), , drop = FALSE]
sample_strategies <- drop_irrational_allocations(sample_strategies_raw, ec_weights, quota = quota)
eval_sample_strat <- evaluate_strategies(sample_strategies, ec_weights, quota = quota)

start_time <- Sys.time()
all_sample_results <- blotto_compare(sample_strategies, game_weights = ec_weights, tie_method = "p2wins")
end_time <- Sys.time()
cat(sprintf("\nTotal execution time: %.2f minutes\n", as.numeric(difftime(end_time, start_time, units = "mins"))))

top_mwc_strategies <- as.matrix(all_mwc_results[1:1000, 1:7])
top_1000_results <- blotto_compare(top_mwc_strategies, game_weights = ec_weights, tie_method = "p2wins")


# ========== 9. Strategy Comparisons ==========

# ========== 10. Compare Key Strategies ==========
