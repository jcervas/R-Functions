# ===========================================================
# EC Game Analysis Script
# Author: Jonathan Cervas
# Purpose: Analyze participant strategies in a Blotto-style Electoral College game
# ===========================================================

# ========== 1. Setup and Initialization ==========

rm(list = ls(all = TRUE)) # Clear environment to avoid variable conflicts
options(scipen = 999) # Avoid scientific notation in outputs

# Set Palatino as the default font for plots (Mac-specific)
quartzFonts(
    palatino = c("Palatino", "Palatino-Italic", "Palatino-Bold", "Palatino-BoldItalic")
)

set.seed(66) # For reproducibility

# --- Load required functions and set output directory ---
source("/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/GitHub/R-Functions/EC_Game/colonial_blotto.R")
figure_directory <- "/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Academic/Working Papers/EC Game/figures/"

# --- Define Electoral College weights and quota ---
members <- c("A", "B", "C", "D", "E", "F", "G") # State labels
blotto_weights <- c(3, 5, 8, 13, 21, 34, 55) # Electoral votes per state
quota <- 70 # Votes needed to win
state_names <- LETTERS[1:7]
state_labels <- paste("State", state_names)
names(blotto_weights) <- state_names

# ========== 2. Coalition Analysis ==========

## Find all winning coalitions (combinations of states exceeding the quota)
winning_combos <- all_winning_combinations(members, blotto_weights)
winning_combos <- winning_combos[order(winning_combos$Total_Weight), ] # Sort by total weight
table(winning_combos$Total_Weight) # Frequency of each winning total

# Display all winning coalitions in a table
knitr::kable(
    winning_combos,
    format = "simple",
    align = "c",
    caption = "All Possible Winning Combinations",
    row.names = FALSE
)

unique_weights <- unique(winning_combos$Total_Weight)
unique_weights[1] # Show the smallest winning total

# Compute Banzhaf index for each state (measures voting power)
ec_banzhaf <- banzhaf(
    names(blotto_weights),
    blotto_weights,
    quota
)

# Apportion values for Banzhaf and proportional weights (for later comparison)
banzhaf_apportion <- apportion_values(ec_banzhaf, target_sum = 100)
proportional_apportion <- apportion_values(blotto_weights, target_sum = 100)

# Identify minimal winning coalitions (MWCs)
mwcs <- find_mwcs(
    vector_weights = blotto_weights,
    quota = quota
)


# ========== 3. 2024 Election ===========
data2024 <- read.csv('/Users/cervas/Downloads/dataset.csv')
trump24 <- data2024[data2024$candidate %in% c("Trump","Vance"),]
harris24 <- data2024[data2024$candidate %in% c("Biden", "Harris","Walz"),]

round(prop.table(table(trump24$state)) * 100, 2)
round(prop.table(table(harris24$state)) * 100, 2)

election_2024 <- data.frame(
    state_labels = c("Arizona", "Georgia", "Michigan", "Nevada", "North Carolina", "Pennsylvania", "Wisconsin"),
    ec = c(11, 16, 15, 6, 16, 19, 10)
    )

quota_GOP <- 51
quota_DEM <- 44

banzhaf_GOP <- banzhaf(
    election_2024$state_labels,
    election_2024$ec,
    quota_GOP
)

banzhaf_DEM <- banzhaf(
    election_2024$state_labels,
    election_2024$ec,
    quota_DEM
)

round(100 * rbind(
    EC = election_2024$ec/sum(election_2024$ec),
    `Trump Campaign Banzhaf` = banzhaf_GOP,
    `Biden/Harris Campaign Banzhaf` = banzhaf_DEM
    ), d=2)
    
# ========== 3. User Data Processing ==========

# Load participant data from JSON file
user_data <- jsonlite::read_json(
    "/Users/cervas/Library/CloudStorage/GoogleDrive-jcervas@andrew.cmu.edu/My Drive/Academic/Working Papers/EC Game/data/results.json"
)
participant_matrix_raw <- as.matrix(do.call(rbind, lapply(user_data, unlist)))
participant_matrix <- deduplicate_combinations(participant_matrix_raw) # Remove duplicate strategies

# Normalize each participant's allocation to sum to 100
participant_matrix_adj <- t(apply(
    participant_matrix, 1, function(x) apportion_values(x, target_sum = 100)
))

# Evaluate participant strategies (coverage, rationality, etc.)
participant_results <- evaluate_strategies(
    strategy_matrix = participant_matrix_adj,
    vector_weights = blotto_weights,
    quota = 70
)

# Prepare summary table for publication
participant_summary_table <- participant_results$position_summary
colnames(participant_summary_table) <- c(
    "State", "Zeroes", "Zero (%)", "Mean", "Median",
    "SD", "Min", "Max"
)
participant_summary_table$State <- LETTERS[1:7] # Use A–G labels

# Output: Summary of Allocation Statistics by State
knitr::kable(
    participant_summary_table,
    format = "simple",
    align = "c",
    caption = "Summary of Allocation Statistics by State (N = 214)"
)

# ========== 4. Hypothesis 1: Predictive Power of Electoral Weight vs. Voting Power ==========

# Compute mean and median allocations by state
user_means <- apply(participant_matrix_adj, 2, mean)
user_medians <- apply(participant_matrix_adj, 2, median)

user_table <- t(rbind(
    Participant_Average = round(user_means, 1),
    Participant_Median = user_medians
))
rownames(user_table) <- state_labels

# Output: Table of participant average and median allocations
knitr::kable(
    user_table,
    format = "simple",
    caption = "Participant Average and Median Allocations, by State"
)

# Plot: Stripchart of participant allocations by state
pdf(paste0(figure_directory, "mean-alloc-strip.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)
stripchart(as.data.frame(participant_matrix_adj),
    vertical = TRUE, method = "jitter",
    pch = 16, col = "gray", xaxt = "n",
    main = "Participant Allocations by State",
    ylab = "Allocation", xlab = "State",
    ylim = c(0, 100), cex = 0.5
)
axis(1, at = 1:7, labels = state_names)
abline(h = c(25, 50, 75), col = "lightgray", lty = 2)
medians <- apply(participant_matrix_adj, 2, median)
points(1:7, medians, col = "red", pch = 19)
legend("topleft", legend = "Median", col = "red", pch = 19, bty = "n")
dev.off()
# Note: Gray dots = individual allocations; red dots = median per state.

# ========== 5. Compare User Allocations to Theoretical Benchmarks ==========

# Normalize all strategies for comparison
proportional_norm <- normalize(blotto_weights)
banzhaf_norm <- normalize(ec_banzhaf)
user_means_norm <- normalize(user_means)
user_medians_norm <- normalize(user_medians)

strategies_matrix <- t(rbind(
    proportional_norm,
    banzhaf_norm,
    user_means_norm,
    user_medians_norm
))
colnames(strategies_matrix) <- c("Vote-Share Weights", "Banzhaf", "Avg Allocation", "Median Allocation")
rownames(strategies_matrix) <- LETTERS[1:7]

# Linear models: Do user allocations align more with EC weights or Banzhaf power?
model_means_ec <- lm(user_means_norm ~ proportional_norm)
model_medians_ec <- lm(user_medians_norm ~ proportional_norm)
model_means_bz <- lm(user_means_norm ~ banzhaf_norm)
model_medians_bz <- lm(user_medians_norm ~ banzhaf_norm)

summary(model_means_ec)
summary(model_medians_ec)
summary(model_means_bz)
summary(model_medians_bz)

# Helper: Extract model stats for reporting
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

# Output: Table comparing regression models
comparison_table <- rbind(
    extract_model_info(
        model_means_ec, 
        "EC Weights", 
        "Mean Allocation", 
        "Model 1"),
    extract_model_info(
        model_means_bz, 
        "Banzhaf Power", 
        "Mean Allocation", 
        "Model 2"),
    extract_model_info(
        model_medians_ec, 
        "EC Weights", 
        "Median Allocation", 
        "Model 3"),
    extract_model_info(
        model_medians_bz, 
        "Banzhaf Power", 
        "Median Allocation", 
        "Model 4")
)
knitr::kable(comparison_table,
    format = "simple",
    caption = "Comparison of Regression Models: EC Weights vs. Banzhaf Power"
)

# ========== 6. Define and Evaluate Key Strategies ==========

# Construct deterministic strategies for comparison
strategy_ec <- apportion_values(blotto_weights, 100)
strategy_ec2 <- apportion_values(blotto_weights^2, 100)
strategy_banzhaf <- apportion_values(ec_banzhaf, 100)
strategy_banzhaf2 <- apportion_values(ec_banzhaf^2, 100)
strategy_uniform <- apportion_values(c(rep(1 / 7, 5), rep(1 / 6.999, 2)), 100)
all_strategies <- rbind(
    strategy_ec,
    strategy_ec2,
    strategy_banzhaf,
    strategy_banzhaf2,
    strategy_uniform
)
rownames(all_strategies) <- c("ec", "ec2", "banzhaf", "banzhaf2", "proportional")

# Compare each strategy against participant pool
all_strategies_results <- lapply(seq_len(nrow(all_strategies)), function(i) {
    blotto_compare(
        participant_matrix_adj,
        weights = blotto_weights,
        tie_method = "winhalf",
        single_strategy = all_strategies[i, ]
    )
})
names(all_strategies_results) <- rownames(all_strategies)

# Extract win counts and rates
win_counts <- sapply(all_strategies_results, function(x) x$Win_Count)
opponents_faced <- sapply(all_strategies_results, function(x) x$Opponents_Faced)
win_rates <- sapply(all_strategies_results, function(x) x$Win_Percentage)

# Output: Table of win rates for deterministic strategies
strategy_labels <- c("EC", "EC²", "Banzhaf", "Banzhaf²", "Proportional")
df <- data.frame(
    ` ` = strategy_labels,
    `Win Count` = win_counts,
    `Opponents Faced` = opponents_faced,
    `Win %` = paste0(round(win_rates, 1), "%"),
    check.names = FALSE
)
knitr::kable(
    df,
    format = "simple",
    caption = "Win rates for deterministic strategies under p2wins tie-breaking",
    row.names = FALSE
)

# Hypothesis tests: Is EC² better than EC? Is Banzhaf² better than Banzhaf?
cat("\nEC² vs EC:\n")
print(
    prop.test(
        x = c(win_counts["ec2"], win_counts["ec"]),
        n = c(214, 214),
        alternative = "greater"
    )
)
cat("\nBanzhaf² vs Banzhaf:\n")
print(
    prop.test(
        x = c(win_counts["banzhaf2"], win_counts["banzhaf"]),
        n = c(214, 214),
        alternative = "greater"
    )
)

# Plot: Barplot comparing normalized strategies
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
    args.legend = list(
        x = "topleft", 
        bty = "n", 
        inset = 0.01, 
        x.intersp = 0.5, 
        y.intersp = 0.8),
    las = 1
)
axis(2, 
at = seq(0, 50, by = 10), 
labels = paste0(seq(0, 50, by = 10), "%"), 
las = 1)
dev.off()

# ========== 7. Distance and Error Analysis: User vs. Benchmarks ==========

# Compute Euclidean distance to Banzhaf and Proportional benchmarks
power_index <- apportion_values(ec_banzhaf, 100)
seat_index <- apportion_values(blotto_weights, 100)
dist_to_Banzhaf <- apply(participant_matrix_adj, 1, function(x) sqrt(sum((x - power_index)^2)))
dist_to_Seats <- apply(participant_matrix_adj, 1, function(x) sqrt(sum((x - seat_index)^2)))
closer_to <- ifelse(dist_to_Banzhaf < dist_to_Seats, "Banzhaf", "Seats")
table(closer_to) # How many are closer to each?

# Paired t-test: Are users closer to Banzhaf or EC weights?
tt.Euclidean <- t.test(dist_to_Seats, dist_to_Banzhaf, paired = TRUE)

# Mean squared error (MSE) comparison
user_mse_ec <- apply(participant_matrix_adj, 1, function(x) mean((normalize(x) - normalize(proportional_norm))^2))
user_mse_banzhaf <- apply(participant_matrix_adj, 1, function(x) mean((normalize(x) - normalize(banzhaf_norm))^2))
tt.mse <- t.test(user_mse_ec, user_mse_banzhaf, paired = TRUE)
t_val <- round(tt.mse$statistic, 2)
df <- tt.mse$parameter
p_val <- tt.mse$p.value
ci <- round(tt.mse$conf.int, 3)
p_str <- if (p_val < 0.001) "< 0.001" else paste0("= ", signif(p_val, 3))
cat("To further evaluate predictive accuracy, we computed the mean squared error (MSE) for each participant’s strategy relative to both benchmarks. A paired t-test showed that participant strategies were significantly closer to the Banzhaf benchmark,")
cat(sprintf(
    "t(%d) = %.2f, p %s, 95%% CI = [%.3f, %.3f].",
    df, t_val, p_str, ci[1], ci[2]
))

# Chi-square test: Frequency of closer alignment
closer_to_banzhaf <- sum(user_mse_banzhaf < user_mse_ec)
closer_to_ec <- sum(user_mse_ec < user_mse_banzhaf)
freq_table <- c(Banzhaf = closer_to_banzhaf, EC = closer_to_ec)
cs <- chisq.test(freq_table)
chi_sq <- round(cs$statistic, 2)
df <- cs$parameter
p_val <- cs$p.value
p_str <- if (p_val < 0.001) "< 0.001" else paste0("= ", signif(p_val, 3))
N <- sum(freq_table)
cat("As a complementary categorical test, we classified each participant based on which benchmark their strategy more closely resembled. A chi-squared test showed the result was not statistically significant:")
cat(sprintf("χ²(%d, N = %d) = %.2f, p %s.", df, N, chi_sq, p_str))

# ========== 8. Hypothesis 2a: Are user strategies clustered around MWCs? ==========

strict_assignments <- assign_strict_mwc(participant_matrix_adj, mwcs)
mwc_labels <- sapply(mwcs, function(coal) paste(state_names[coal], collapse = ","))
mwc_sums <- sapply(mwcs, function(coal) sum(blotto_weights[coal]))

# Count how many strategies are strictly clustered on a single MWC
counts <- table(factor(strict_assignments, levels = c(1:10, NA)), useNA = "ifany")
names(counts) <- c(mwc_labels, "Other")
clustered <- sum(counts[!is.na(names(counts)) & names(counts) != "Other"])
total <- sum(counts)
percent_clustered <- round(clustered / total * 100, 1)
percent_non_clustered <- round(counts[is.na(names(counts))] / total * 100, 1)
cat("We find that", percent_clustered, "% of all simulated strategies allocate resources exclusively to the members of a single minimal winning coalition (MWC), with no allocation to any other states. This provides moderate support for Hypothesis 2. While a minority of strategies exhibit strict clustering around MWCs, the majority (", percent_non_clustered, "%) distribute resources more broadly—suggesting that while MWCs influence behavior, strategic diversity remains high.")

# Output: Table of MWCs and participant counts
df <- as.data.frame(counts)
mwc_table <- cbind(
    `MWC` = c(mwc_labels, "Other"),
    `Coverage` = c(mwc_sums, "NA"),
    `n` = df$Participants
)
knitr::kable(
    mwc_table,
    format = "simple",
    caption = "Minimum Winning Coalitions and Participant Counts"
)

# ========== 9. Hypothesis 2b: Do users favor small coalitions (few states)? ==========

active_states <- rowSums(participant_matrix_adj > 0)
active_states_table <- table(active_states)
freq <- as.vector(active_states_table)
percent <- round(100 * freq / sum(freq), 1)
df <- rbind(Number_of_Participants = freq, Percentage = percent)
colnames(df) <- 1:7
knitr::kable(
    df,
    format = "simple",
    caption = "Number and Percentage of Strategies Allocating to Each Number of States (A–G)"
)
var_active <- var(active_states)
p <- table(active_states) / length(active_states)
entropy <- -sum(p * log2(p)) / log2(length(p))
cat("• Variance of active state count: ", round(var_active, 2), "\n• Shannon entropy: ", round(entropy, 2), " (normalized)")

# ========== 10. Hypothesis 3a: Preference for State F over E ==========

F_mean <- mean(participant_matrix_adj[, 6]) # State F
E_mean <- mean(participant_matrix_adj[, 5]) # State E
wtest <- wilcox.test(
    participant_matrix_adj[, 6], 
    participant_matrix_adj[, 5], 
    paired = TRUE)
ttest <- t.test(
    participant_matrix_adj[, 6], 
    participant_matrix_adj[, 5])
cat("• Mean allocation to State F: ", round(F_mean, 2), "\n• Mean allocation to State E: ", round(E_mean, 2))
cat(sprintf("V = %.1f, p = %.5f\n", wtest$statistic, wtest$p.value))
cat(sprintf("p-value = %.3f\n", ttest$p.value))

# ========== 11. Hypothesis 2c: Expanded MWCs ==========

expanded_matches <- generate_expanded_mwcs(mwcs, blotto_weights, quota)
get_expanded_mwc_match <- function(
    strategy_row,
    mwcs,
    blotto_weights,
    quota = 70,
    tol = 1e-6) {
    supported <- which(strategy_row > tol)
    for (i in seq_along(mwcs)) {
        coal <- mwcs[[i]]
        if (!all(coal %in% supported)) next
        extra <- setdiff(supported, coal)
        if (length(extra) != 1) next
        extra_state <- extra[1]
        w_extra <- blotto_weights[extra_state]
        w_mwc <- blotto_weights[coal]
        pair_sums <- outer(w_mwc, w_mwc, "+")[upper.tri(matrix(0, length(w_mwc), length(w_mwc)))]
        if (w_extra >= min(w_mwc) && all(w_extra < pair_sums)) {
            if (sum(blotto_weights[supported]) >= quota) {
                return(list(mwc_index = i, extra_state = extra_state))
            }
        }
    }
    return(NULL)
}
expanded_matches <- apply(
    participant_matrix_adj, 1,
    get_expanded_mwc_match,
    mwcs = mwcs,
    blotto_weights = blotto_weights
)
matched_flags <- sapply(expanded_matches, function(x) !is.null(x))
table(matched_flags)

# ========== 12. Hypothesis 2d: MWC Support Counts and Performance ==========

support_counts <- rowSums(sapply(mwcs, function(coal) rowSums(participant_matrix_adj[, coal, drop = FALSE] > 0) == length(coal)))
table(support_counts)
support_levels <- sort(unique(support_counts))
groups <- lapply(support_levels, function(k) participant_matrix_adj[support_counts == k, , drop = FALSE])
names(groups) <- as.character(support_levels)
win_matrix <- matrix(NA,
    nrow = length(groups), ncol = length(groups),
    dimnames = list(names(groups), names(groups))
)
for (i in seq_along(groups)) {
    for (j in seq_along(groups)) {
        wins <- blotto_compare(
            strategy_set_A = groups[[i]],
            strategy_set_B = groups[[j]],
            weights = blotto_weights,
            tie_method = "winhalf"
        )
        win_matrix[i, j] <- 100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))
    }
}
knitr::kable(
    round(win_matrix, 1),
    format = "simple",
    caption = "Percent Win by Number of MWCs"
)
win_matrix_all <- rep(NA, length(groups))
names(win_matrix_all) <- names(groups)
for (i in seq_along(groups)) {
    wins <- blotto_compare(
        strategy_set_A = groups[[i]],
        strategy_set_B = participant_matrix_adj,
        weights = blotto_weights,
        tie_method = "winhalf"
    )
    win_matrix_all[i] <- 100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))
}
knitr::kable(
    t(round(win_matrix_all, 1)),
    format = "simple",
    caption = "Overall Win Rate by Number of MWCs"
)

wins <- blotto_compare(strategy_set_A = groups[[2]], strategy_set_B = participant_matrix_adj, weights = blotto_weights, tie_method = "winhalf")
100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))

wins <- blotto_compare(strategy_set_A = groups[[8]], strategy_set_B = participant_matrix_adj, weights = blotto_weights, tie_method = "winhalf")
100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))

# Median allocation by mwc # group
medians <- lapply(groups, function(mat) apply(mat, 2, median))


# Label each strategy according to all their MWC
# Step 1: Build MWC labels
state_names <- c("A", "B", "C", "D", "E", "F", "G") # adjust as needed
mwc_labels <- sapply(mwcs, function(coal) paste(sort(state_names[coal]), collapse = ","))

# Step 2: Get MWC support per participant
mwc_support_matrix <- sapply(mwcs, function(coal) {
    rowSums(participant_matrix_adj[, coal, drop = FALSE] > 0) == length(coal)
})

# Step 3: Assign labels per participant
participant_mwc_labels <- apply(mwc_support_matrix, 1, function(row) {
    supported <- which(row)
    if (length(supported) == 0) {
        return("None")
    }
    paste(mwc_labels[supported], collapse = " | ")
})


# ===========================================================
# Hypothesis 3b: Preference for State D over E

# Mean allocations for all states
part_means <- colMeans(participant_matrix_adj)

D_mean <- part_means[4] # State D

# Compare State D (column 4) and State E (column 5)
cat("Mean allocation to State D:", D_mean, "\n")
cat("Mean allocation to State E:", E_mean, "\n")


pdf(paste0(figure_directory, "participant-alloc-distributions.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)
stacked_hist_plot(participant_matrix_adj, main = "State Allocation Distributions", show_density = FALSE)
dev.off()

wtest <- wilcox.test(participant_matrix_adj[, 5], participant_matrix_adj[, 4], paired = TRUE)

ttest <- t.test(participant_matrix_adj[, 5], participant_matrix_adj[, 4])

cat("• Mean allocation to State D: ", round(D_mean, 2), "\n• Mean allocation to State E: ", round(E_mean, 2))
cat(sprintf("V = %.1f, p = %.5f\n", wtest$statistic, wtest$p.value))
cat(sprintf("p-value = %.3f\n", ttest$p.value))

# ==

no_state_D <- participant_matrix_adj[participant_matrix_adj[, 4] == 0 & participant_matrix_adj[, 5] > 0, ]
no_state_E <- participant_matrix_adj[participant_matrix_adj[, 5] == 0 & participant_matrix_adj[, 4] > 0, ]

mean(no_state_D[, 5])
mean(no_state_E[, 4])

# === Coalition Logic Analysis: State D vs State E ===

# Logical flags for who allocated to D or E
gave_D <- participant_matrix_adj[, 4] > 0 # State D = column 4
gave_E <- participant_matrix_adj[, 5] > 0 # State E = column 5

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
eval_sample_strat <- evaluate_strategies(participant_matrix_adj, blotto_weights, quota = quota)

# Endowment violations
below_endow <- sum(1 * rowSums(participant_matrix) < 100)
above_endow <- sum(1 * rowSums(participant_matrix) > 100)

cat(length(eval_sample_strat$ec_coverage) - eval_sample_strat$rationality_rate / 100 * length(eval_sample_strat$ec_coverage), "submitted strategies that relied on tie-breaking outcomes to reach the 70-vote majority threshold")
cat("An additional", below_endow + above_endow, "participants submitted resource allocations that did not sum to the required 100 units:", below_endow, "were under-allocated, and", above_endow, "exceeded the budget.")
cat("# relying on tie-breaking cointoss:", nrow(participant_matrix_raw) - nrow(participant_matrix_adj))



# ========== Strategic Effectiveness of Allocation Rules
## ========== 1. Round-Robin Comparison of Participant Strategies ==========

compare_coinflip_df <- as.data.frame(blotto_compare(participant_matrix_adj, blotto_weights, tie_method = "coinflip"))
compare_p2wins_df <- as.data.frame(blotto_compare(participant_matrix_adj, blotto_weights, tie_method = "p2wins"))
compare_winhalf_df <- as.data.frame(blotto_compare(participant_matrix_adj, blotto_weights, tie_method = "winhalf"))
compare_tie_df <- as.data.frame(blotto_compare(participant_matrix_adj, blotto_weights, tie_method = "tie"))

# # ========== 2. Top 10 Strategies ==========
colnames(compare_coinflip_df)[1:7] <-
    colnames(compare_p2wins_df)[1:7] <-
    colnames(compare_winhalf_df)[1:7] <-
    colnames(compare_tie_df)[1:7] <- LETTERS[1:7]



## Produce Table with Participant Strategies

# Table: Participant Strategies
knitr::kable(
    compare_winhalf_df[1:10],
    format = "simple",
    align = c("l", "r"),
    caption = "Participant Strategies"
)


## Strategic Efficiency and the Margin of Victory

net_winners <- compare_winhalf_df[compare_winhalf_df$Win_Percentage > 50, , drop = FALSE]
net_losers <- compare_winhalf_df[compare_winhalf_df$Win_Percentage < 50, , drop = FALSE]

# Then compute average margins:
apply(net_winners, 2, mean)
apply(net_losers, 2, mean)
s
# Suppose you want bins of width 10 for column "G"
bin_width <- 10
bins <- cut(compare_winhalf_df[, "F"],
    breaks = seq(min(compare_winhalf_df[, "F"]),
        max(compare_winhalf_df[, "F"]),
        by = bin_width
    ),
    include.lowest = TRUE, right = FALSE
)

# Compute average Win_Percentage by bin
avg_by_bin <- tapply(compare_winhalf_df[, "Win_Percentage"], bins, mean, na.rm = TRUE)
n_by_bin <- as.numeric(table(bins))

# Convert to data frame
avg_by_bin_df <- data.frame(
    `F Allocation Size` = names(avg_by_bin),
    `Average Win Percentage` = round(as.numeric(avg_by_bin), 2),
    `n` = n_by_bin,
    check.names = FALSE
)

# Table: Average Win Percentage by F Allocation
knitr::kable(
    avg_by_bin_df,
    format = "simple",
    align = c("l", "r"),
    caption = "Average Win Percentage by F Allocation"
)

###

# Suppose you want bins of width 10 for column "G"
bin_width <- 10
bins <- cut(compare_winhalf_df[, "G"],
    breaks = seq(min(compare_winhalf_df[, "G"]),
        max(compare_winhalf_df[, "G"]),
        by = bin_width
    ),
    include.lowest = TRUE, right = FALSE
)

# Compute average Win_Percentage by bin
avg_by_bin <- tapply(
    compare_winhalf_df[, "Win_Percentage"],
    bins,
    mean,
    na.rm = TRUE
)
n_by_bin <- as.numeric(table(bins))

# Convert to data frame
avg_by_bin_df <- data.frame(
    `G Allocation Size` = names(avg_by_bin),
    `Average Win Percentage` = round(as.numeric(avg_by_bin), 2),
    `n` = n_by_bin,
    check.names = FALSE
)

# Table: Average Win Percentage by G Allocation
knitr::kable(
    avg_by_bin_df,
    format = "simple",
    align = c("l", "r"),
    caption = "Average Win Percentage by G Allocation"
)

# ========== Check for Monotonicity ==========

# monotonic_w_zeroes_flags <- monotonicity(participant_matrix_adj, include_zeros = TRUE)
# monotonic_wo_zeroes_flags <- monotonicity(participant_matrix_adj, include_zeros = FALSE)

# table(monotonic_wo_zeroes_flags)
# table(monotonic_w_zeroes_flags)

# monotonic_strategies <- participant_matrix_adj[monotonic_wo_zeroes_flags, , drop = FALSE]
# non_monotonic_strategies <- participant_matrix_adj[!monotonic_wo_zeroes_flags, , drop = FALSE]
# evaluate_strategies(monotonic_strategies, blotto_weights)
# evaluate_strategies(non_monotonic_strategies, blotto_weights)


# ========== Strategy Typologies and Allocation Patterns ==========
k <- 5
# Perform clustering
kmeans_participant <- kmeans(participant_matrix_adj, centers = k, nstart = 10)

centroids_k5 <- kmeans_participant$centers
colnames(centroids_k5) <- paste("State", LETTERS[1:7])
rownames(centroids_k5) <- paste("Cluster", 1:5)

centroid_table <- as.data.frame(round(centroids_k5, 2))
centroid_table$n <- as.vector(table(kmeans_participant$cluster))
# Assign cluster to a named vector
cluster_vec <- kmeans_participant$cluster

knitr::kable(
    centroid_table,
    format = "simple",
    caption = "Allocation Profiles by Cluster (k = 5) with Avg Win %"
)


# ========== Testing Clusters against Participants ==========

# Extract strategies only (drop 'n' column)
cluster_strategies <- as.matrix(centroid_table[, 1:7])

# Compare each cluster against each other direclty
clusters_apportioned <- t(apply(centroids_k5, 1, function(x) apportion_values(x, 100)))
rownames(clusters_apportioned) <- 1:5
blotto_compare(clusters_apportioned, blotto_weights)



# Run comparisons
cluster_results <- lapply(1:5, function(i) {
    blotto_compare(
        strategy_matrix = participant_matrix_adj,
        weights = blotto_weights,
        single_strategy = clusters_apportioned,
        tie_method = "winhalf"
    )
})

# Extract results into a data frame
cluster_summary <- data.frame(
    Cluster = paste0("Cluster ", 1:5),
    Win_Count = sapply(cluster_results, function(x) x$Win_Count),
    Opponents_Faced = sapply(cluster_results, function(x) x$Opponents_Faced),
    Win_Percentage = sapply(cluster_results, function(x) x$Win_Percentage)
)

# === Table : Performance of Cluster Centroids Against Participant Strategies in Blotto Game ==== #
knitr::kable(
    cluster_summary,
    format = "simple",
    caption = "Performance of Cluster Centroids Against Participant Strategies in Blotto Game"
)



# Initialize win percentage matrix
win_matrix <- matrix(NA,
    nrow = k, ncol = k,
    dimnames = list(rownames(centroids_k5), rownames(centroids_k5))
)

# Fill matrix with average win percentages
for (i in 1:k) {
    for (j in 1:k) {
        wins <- blotto_compare(strategy_set_A = participant_matrix_adj[cluster_vec == i, ], strategy_set_B = participant_matrix_adj[cluster_vec == j, ], weights = blotto_weights, tie_method = "winhalf")
        win_matrix[i, j] <- 100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))
    }
}

win_matrix_full <- rbind(win_matrix, n = centroid_table$n)

knitr::kable(round(win_matrix_full, 1), format = "simple", caption = "Percent Win by Cluster")



# 4. Initialize win percentage matrix
win_matrix_cluster <- rep(NA, k)
names(win_matrix_cluster) <- rownames(cluster_strategies)

# 5. Fill matrix with average win percentages
for (i in 1:k) {
    wins <- blotto_compare(strategy_set_A = participant_matrix_adj[cluster_vec == i, ], strategy_set_B = participant_matrix_adj, weights = blotto_weights, tie_method = "winhalf")
    win_matrix_cluster[i] <- 100 * (sum(wins[, "Win_Count"]) / sum(wins[, "Opponents_Faced"]))
}

knitr::kable(t(round(win_matrix_cluster, 1)), format = "simple", caption = "Overall Win Rate by Cluster")




# Assign cluster vector directly (same order)
compare_coinflip_df$Cluster <-
    compare_p2wins_df$Cluster <-
    compare_winhalf_df$Cluster <-
    compare_tie_df$Cluster <- factor(kmeans_participant$cluster)


top_coinflip <- head(compare_coinflip_df[order(compare_coinflip_df$Rank), ], 5)
top_p2wins <- head(compare_p2wins_df[order(compare_p2wins_df$Rank), ], 5)
top_winhalf <- head(compare_winhalf_df[order(compare_winhalf_df$Rank), ], 5)
top_tie <- head(compare_tie_df[order(compare_tie_df$Rank), ], 5)

cols <- c(LETTERS[1:7], "Win_Count", "Win_Percentage", "Cluster")
top_both <- rbind(
    cbind(Tie_Break = "Coin Flip", top_coinflip[, cols]),
    cbind(Tie_Break = "Player 2 Wins", top_p2wins[, cols]),
    cbind(Tie_Break = "Tie", top_tie[, cols]),
    cbind(Tie_Break = "Split Prize", top_winhalf[, cols])
)



# ==== Top/Bottom 10 Strategies
# Compute EC coverage
ec_covered <- apply(participant_matrix_adj > 0, 1, function(row) sum(blotto_weights[row]))

# Filter rational strategies
rational_ids <- which(ec_covered >= quota)
rational_df <- compare_winhalf_df[rational_ids, ]

# Define columns for display
cols <- c(paste(LETTERS[1:7]), "Win_Count", "Win_Percentage", "Cluster")

# Extract top/bottom 10 rational strategies
top_10 <- rational_df[order(rational_df$Rank), ][1:10, cols]
bottom_10 <- rational_df[order(rational_df$Rank), ][nrow(rational_df):(nrow(rational_df) - 9), cols]


apply(top_10[, 1:7], 2, FUN = median)
apply(bottom_10[, 1:7], 2, FUN = median)

# --- Output

cat("Table: Top/Bottom 10 Strategies Among Participants\n\n")
cat("Top 10 Strategies\n")
knitr::kable(top_10, format = "simple", row.names = FALSE)
cat("\nBottom 10 Strategies\n")
knitr::kable(bottom_10, format = "simple", row.names = FALSE)

cat("\nNote: This table excludes strategies that fail to allocate resources to a combination of states meeting the 70-vote quota. While such strategies were submitted, they are excluded here due to their limited analytical value.\n")


# Create binary outcome
win_flag <- as.integer(compare_winhalf_df$Win_Percentage > 50)
table(compare_winhalf_df$Cluster, win_flag)


# ========== ENCM (Effective Number of Coalition Members) Analysis ==========

encm_values <- apply(participant_matrix_adj, 1, calc_encm)

# Summary stats for all strategies
summary(encm_values)



# === # Plot histogram: Effective Number of Coalition Members in Participant Allocations
# Transparent gray color
bar_col <- adjustcolor("gray40", alpha.f = 0.5)

pdf(paste0(figure_directory, "encm_values.pdf"), width = 8, height = 5)
par(family = "Palatino", mar = c(3, 4, 2, 1), xpd = NA)
hist(encm_values,
    breaks = seq(1, 7, by = 0.25),
    col = bar_col,
    border = NA,
    main = "",
    xlab = "Allocation",
    xlim = c(1, 7),
    # ylim = c(0, 0.5),  # leave space for density
    yaxt = "n",
    freq = TRUE
)

# Add horizontal base line (like stacked plot)
lines(c(0, 100), c(0, 0), col = "gray90")
axis(2,
    at = seq(0, 25, by = 5),
    labels = seq(0, 25, by = 5), las = 1
)
dev.off()

# Compare by win group
win_group <- ifelse(compare_winhalf_df$Win_Percentage > 50, ">50%", "\u226450%")
encm_grouped <- data.frame(ENCM = encm_values, Win_Group = win_group)

group_levels <- unique(win_group)
summary_matrix <- sapply(group_levels, function(g) {
    vals <- encm_grouped$ENCM[encm_grouped$Win_Group == g]
    c(
        Mean = mean(vals), Median = median(vals), SD = sd(vals),
        Min = min(vals), Max = max(vals)
    )
})

summary_table <- round(t(summary_matrix), 2)
knitr::kable(summary_table,
    format = "simple",
    caption = "Effective Number of Coalition Members by Win Group"
)

# ========== 14. Logistic Regression: Win ~ ENCM ==========

# --- 1. Construct binary win outcome
win_flag <- as.integer(compare_winhalf_df$Win_Percentage > 50)

# --- 2. Fit logistic model
encm_logit <- glm(win_flag ~ encm_values, family = binomial)

# --- 3. Generate prediction grid and confidence bands
encm_seq <- seq(min(encm_values), max(encm_values), length.out = 100)
pred_frame <- data.frame(encm_values = encm_seq)
pred <- predict(encm_logit, newdata = pred_frame, type = "link", se.fit = TRUE)

fit <- pred$fit
se <- pred$se.fit
prob <- plogis(fit)
upper <- plogis(fit + 1.96 * se)
lower <- plogis(fit - 1.96 * se)

# --- 4. Plot
pdf(paste0(figure_directory, "logit_encm_full.pdf"), width = 5, height = 5)
par(family = "Palatino", xpd = NA)

# Main logistic curve
plot(encm_seq, prob,
    type = "l", lwd = 2, col = "black",
    ylim = c(0, 1),
    xlab = "Effective Number of Coalition Members (ENCM)",
    ylab = "Predicted Probability of Winning Record",
    main = "ENCM and Probability of Winning",
    yaxt = "n"
)

# Confidence band
polygon(c(encm_seq, rev(encm_seq)),
    c(lower, rev(upper)),
    col = adjustcolor("gray80", alpha.f = 0.2), border = NA
)

# Jittered observed outcomes
set.seed(42)
jitter_y <- jitter(win_flag, factor = 0.1)
points(encm_values, jitter_y, pch = 20, col = adjustcolor("black", alpha.f = 0.05))

# Percentage-style y-axis
y_vals <- seq(0, 1, by = 0.1)
axis(side = 2, at = y_vals, labels = paste0(y_vals * 100, "%"), las = 2, cex.axis = 0.65)

dev.off()

# ========== Match-Level Logistic Regression ==========

strategy_ids <- seq_len(nrow(participant_matrix_adj))
encm_lookup <- setNames(encm_values, strategy_ids)

results <- data.frame(ENCM = numeric(), Outcome = integer(), stringsAsFactors = FALSE)

for (i in seq_along(strategy_ids)) {
    for (j in seq_along(strategy_ids)) {
        if (i == j) next
        if (i > j) next
        p1 <- participant_matrix_adj[i, ]
        p2 <- participant_matrix_adj[j, ]
        win_rate <- blotto_compare(strategy_set_A = p1, strategy_set_B = p2, weights = blotto_weights, tie_method = "winhalf")
        outcome <- win_rate$Win_Count
        results <- rbind(results, data.frame(
            ENCM = encm_lookup[strategy_ids[i]],
            Outcome = outcome
        ))
    }
}
results$Outcome <- ifelse(results$Outcome < 1, 0, 1)
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

pdf(paste0(figure_directory, "dyadic_logit_encm.pdf"), width = 5, height = 5)
par(family = "Palatino", xpd = NA)
plot(encm_seq, prob,
    type = "l", lwd = 2, col = "black",
    ylim = c(0, 1),
    xlab = "Effective Number of Coalition Members (ENCM)",
    ylab = "Predicted Win Probability",
    main = "ENCM and Probability of Winning a Matchup",
    yaxt = "n"
)
polygon(c(encm_seq, rev(encm_seq)), c(lower, rev(upper)),
    col = adjustcolor("gray80", alpha.f = 0.2), border = NA
)
set.seed(42)
jitter_y <- jitter(results$Outcome, factor = 0.1)
points(results$ENCM, jitter_y, pch = 20, col = adjustcolor("black", alpha.f = 0.05))
y_vals <- seq(0, 1, by = 0.1)
axis(side = 2, at = y_vals, labels = paste0(y_vals * 100, "%"), las = 2, cex.axis = 0.65) # las=2 rotates labels
# grid()
dev.off()

# Logistic regression summary output
stargazer::stargazer(logit_dyadic,
    type = "html",
    title = "Logistic Regression: Match-Level Win Outcome ~ ENCM",
    digits = 3,
    header = FALSE,
    dep.var.labels = "Win Outcome",
    covariate.labels = c("ENCM"),
    star.cutoffs = c(0.05, 0.01, 0.001)
)



# ========== 9. Strategy Comparisons ==========


# ========== Count MWC by Participants ==========

counts
sum(1 * !is.na(strict_assignments))

mwc_strict_performance <- t.test(
    compare_winhalf_df[is.na(strict_assignments), "Win_Percentage"],
    compare_winhalf_df[!is.na(strict_assignments), "Win_Percentage"]
)
mwc_strict_performance

# # ========== Count MWC and Expanded Coalitions by Participants ==========

# expanded_matches <- apply(participant_matrix_adj, 1, get_expanded_mwc_match, mwcs = mwcs, blotto_weights = blotto_weights)
# matched_flags <- sapply(expanded_matches, function(x) !is.null(x))
# table(matched_flags)

# expanded_matches <- apply(participant_matrix_adj, 1, get_expanded_mwc_match, mwcs = mwcs, blotto_weights = blotto_weights)

# # Filter non-null entries (actual expanded MWC users)
# expanded_valid <- Filter(Negate(is.null), expanded_matches)

# # Count them
# length(expanded_valid)

# # Optional: tabulate MWC usage
# table(sapply(expanded_valid, `[[`, "mwc_index"))


# # Run matcher
# expanded_matches <- apply(participant_matrix_adj, 1, get_expanded_mwc_match, mwcs = mwcs, blotto_weights = blotto_weights)

# # Get matching row indices
# valid_ids <- which(!sapply(expanded_matches, is.null))

# # Build data frame
# expanded_df <- data.frame(
#   row_number = valid_ids,
#   mwc_index = sapply(expanded_matches[valid_ids], `[[`, "mwc_index"),
#   extra_state = sapply(expanded_matches[valid_ids], `[[`, "extra_state")
# )

# print(expanded_df)

# dim(expanded_df)


# # Expanded vs. Everyone Else
# t.test(
#   compare_winhalf_df[-expanded_df[,"row_number"], "Win_Percentage"],
#   compare_winhalf_df[expanded_df[,"row_number"], "Win_Percentage"]
# )

# # Expanded vs. Strict MWC strategies
# t.test(
#   compare_winhalf_df[expanded_df[,"row_number"], "Win_Percentage"],
#   compare_winhalf_df[!is.na(strict_assignments), "Win_Percentage"])




# ========== 6. MWC Coverage by Winning Strategies ==========
winning_ids <- compare_winhalf_df[compare_winhalf_df$Win_Percentage > 50, ]
winning_allocs <- winning_ids[, 1:7]

count_mwcs_covered <- function(strategy_row, mwcs) {
    sum(sapply(mwcs, function(coal) all(strategy_row[coal] > 0)))
}

mwc_counts_all <- apply(participant_matrix_adj, 1, count_mwcs_covered, mwcs = mwcs)
print(table(mwc_counts_all))

mwc_counts <- apply(winning_allocs, 1, count_mwcs_covered, mwcs = mwcs)
print(table(mwc_counts))


cat("\u2192 Mean number of MWCs covered by net-winning strategies:", round(mean(mwc_counts), 2), "\n")
cat("\u2192 Distribution:\n")
print(table(mwc_counts))


table(mwc_counts_all)[2] / sum(table(mwc_counts_all))
table(mwc_counts)[1] / sum(table(mwc_counts))


# ========== 10. Compare Key Strategies ==========


# ==========  The Strategic Superiority of State G ==========

sum(grepl("A", results_df$Members)) # Count how many combinations include A
sum(grepl("B", results_df$Members)) # Count how many combinations include B
sum(grepl("C", results_df$Members)) # Count how many combinations include C
sum(grepl("D", results_df$Members)) # Count how many combinations include D
sum(grepl("E", results_df$Members)) # Count how many combinations include E
sum(grepl("F", results_df$Members)) # Count how many combinations include F
sum(grepl("G", results_df$Members)) # Count how many combinations include G

sum((grepl("F", results_df$Members) & grepl("G", results_df$Members))) # Count combinations including both F and G



label_f_g_heavy <- function(mat, f_col = 6, g_col = 7, threshold = 50) {
    apply(mat, 1, function(row) {
        f <- row[f_col]
        g <- row[g_col]
        if (f >= threshold && g < threshold) {
            return("F-Heavy")
        } else if (g >= threshold && f < threshold) {
            return("G-Heavy")
        } else {
            return("Other")
        }
    })
}
label_fg_or <- function(label_vector) {
    ifelse(label_vector %in% c("F-Heavy", "G-Heavy"), "F-or-G-Heavy", "Other")
}

results <- blotto_compare(participant_matrix_adj, weights = blotto_weights, tie_method = "winhalf")
results$FG_Label <- label_f_g_heavy(as.matrix(results[, 1:7]))
results$cluster <- cluster_vec

results$FG_Label <- label_f_g_heavy()
results$FG_Or_Label <- label_fg_or(results$FG_Label)

table(results$FG_Label, results$cluster)

# boxplot(Win_Percentage ~ FG_Label, data = results,
#         main = "Win % by Strategy Type (F vs G)",
#         ylab = "Win Percentage", xlab = "Strategy Type")


# Compute core stats
means <- tapply(results$Win_Percentage, results$FG_Label, mean)
sds <- tapply(results$Win_Percentage, results$FG_Label, sd)
ns <- tapply(results$Win_Percentage, results$FG_Label, length)
ses <- sds / sqrt(ns)
ci_lowers <- means - 1.96 * ses
ci_uppers <- means + 1.96 * ses
cis <- sprintf("[%.1f%%, %.1f%%]", ci_lowers, ci_uppers)

summary_df <- data.frame(
    `Strategy Type` = as.character(names(means)),
    `Mean Win Percent` = round(means, 1),
    `CI [95%]` = cis,
    `n` = as.integer(ns),
    check.names = FALSE
)

summary_df <- summary_df[order(summary_df[, "Strategy Type"]), ]

knitr::kable(summary_df,
    format = "simple",
    align = "lccc",
    caption = "Win Percentages by Strategy Type",
    row.names = FALSE
)



# # ==== Appendix Table: Top 10 Strategies by Tie-Breaking Rule
# knitr::kable(top_both, format = "simple", row.names = FALSE,
#              caption = "Top 10 Strategies by Tie-Breaking Rule")

# ==== Appendix Table: Top 10 Strategies

# ========== 3. Strategy Rank Stability ==========
# rank_cf <- rank(-compare_coinflip_df$Win_Count, ties.method = "first")
# rank_p2 <- rank(-compare_p2wins_df$Win_Count, ties.method = "first")
# names(rank_cf) <- rownames(compare_coinflip_df)
# names(rank_p2) <- rownames(compare_p2wins_df)

# cf_top <- rank_cf <= floor(length(rank_cf) / 2)
# p2_top <- rank_p2 <= floor(length(rank_p2) / 2)

# moved_up_ids   <- names(rank_cf)[!cf_top & p2_top]
# moved_down_ids <- names(rank_cf)[cf_top & !p2_top]

# common_ids <- intersect(names(rank_cf), names(rank_p2))
# x <- rank_cf[common_ids]; y <- rank_p2[common_ids]

# point_color <- rep("gray70", length(x))
# names(point_color) <- names(x)
# point_color[moved_up_ids]   <- "blue"
# point_color[moved_down_ids] <- "red"


# === Figure A: The Effect of Tie-Breaker Rule on Participate Win Success

# pdf(paste0(figure_directory, "appendix-tie-breaker-win-success.pdf"), width = 5, height = 5)
# par(family = "Palatino", xpd = NA)

# plot(x, y, pch = 21, bg = point_color, col = "black",
#      xlab = "Rank under Coin Flip", ylab = "Rank under Player 2 Wins",
#      main = "Rank Stability with Movers Highlighted")
# abline(a = 0, b = 1, col = "darkgray", lty = 2); grid()
# legend("bottomright", legend = c("Stable", "Moved Up", "Moved Down"),
#        pt.bg = c("gray70", "blue", "red"), pch = 21, title = "Strategy Type")
# dev.off()


# avg_rank_up_cf   <- mean(rank_cf[moved_up_ids], na.rm = TRUE)
# avg_rank_up_p2   <- mean(rank_p2[moved_up_ids], na.rm = TRUE)
# avg_rank_down_cf <- mean(rank_cf[moved_down_ids], na.rm = TRUE)
# avg_rank_down_p2 <- mean(rank_p2[moved_down_ids], na.rm = TRUE)

# cat("\u2192 Movers (Up): Avg Rank under coin flip:", round(avg_rank_up_cf, 1),
#     "| under Player 2 wins:", round(avg_rank_up_p2, 1), "\n")
# cat("\u2192 Movers (Down): Avg Rank under coin flip:", round(avg_rank_down_cf, 1),
#     "| under Player 2 wins:", round(avg_rank_down_p2, 1), "\n")

# # ========== 4. Transition Table ==========
# cf_top <- rank_cf <= floor(nrow(compare_coinflip_df) / 2)
# p2_top <- rank_p2 <= floor(nrow(compare_p2wins_df) / 2)

# transition_table <- table(cf_top, p2_top)
# cat("\u2192 Strategies moving from bottom to top half:", sum(!cf_top & p2_top), "\n")
# cat("\u2192 Strategies moving from top to bottom half:", sum(cf_top & !p2_top), "\n")
# print(transition_table)

# # ========== 5. Allocation Profiles ==========
# stable_high_ids <- cf_top & p2_top
# stable_low_ids  <- !cf_top & !p2_top
# mover_ids       <- xor(cf_top, p2_top)

# mean_high  <- colMeans(participant_matrix_adj[stable_high_ids, , drop = FALSE])
# mean_low   <- colMeans(participant_matrix_adj[stable_low_ids, , drop = FALSE])
# mean_mover <- colMeans(participant_matrix_adj[mover_ids, , drop = FALSE])

# print(round(mean_high, 2))
# print(round(mean_mover, 2))
# print(round(mean_low, 2))




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
#   compute_margin_over_threshold(top_alloc, participant_matrix_adj[id, ], blotto_weights)
# }, USE.NAMES = FALSE)

# mean_margin_over <- mean(margins_over_70[margins_over_70 > 0], na.rm = TRUE)

# cat(sprintf("\u2713 Avg margin *above threshold* for wins: %.2f electoral votes\n", mean_margin_over))




# ========== 3. Strategy Generator ==========

# methods_ec <- c("uniform", "uniform_skewed", "normal", "poisson", "gamma", "beta", "exponential", "geometric")

# generate_sample_set <- function(vector_weights, methods, n = 10000) {
#   setNames(lapply(methods, function(m) {
#     t(replicate(n, new_combinations(vector_weights = vector_weights, target_sum = 100, method = m)))
#   }), methods)
# }

# all_random_samples_ec <- generate_sample_set(blotto_weights, methods_ec)
# all_random_samples_banzhaf <- generate_sample_set(ec_banzhaf, methods_ec)
# all_random_samples_equal <- generate_sample_set(rep(1/7, 7), methods_ec)

# # ========== 4. Combine All Samples ==========

# all_random_samples_raw <- rbind(
#   do.call(rbind, all_random_samples_ec),
#   do.call(rbind, all_random_samples_banzhaf),
#   do.call(rbind, all_random_samples_equal)
# )

# cat("✓ Total Number of Random Samples Generated:", nrow(all_random_samples_raw), "\n")
# all_random_samples <- unique(all_random_samples_raw)
# cat("Original:", nrow(all_random_samples_raw), "→ Unique:", nrow(all_random_samples), "\n")

# # ========== 5. MWC Sampling ==========

# mwc_samples <- sample_mwc_distributions(
#   mwcs, vector_weights = blotto_weights, target_sum = 100, total_samples_goal = 1e6
# )
# all_mwc_samples <- do.call(rbind, mwc_samples)
# cat("✓ Total Number of MWC Samples Generated:", nrow(all_mwc_samples), "\n")


# # ========== 6. Sample Validation ==========

# validate_sampling_methods(blotto_weights, methods_ec)

# # ========== 7. Visualization ==========

# par(mfrow = c(1, 3))
# stacked_hist_plot(all_random_samples, main = "All Random Simulations", show_density = TRUE)
# stacked_hist_plot(all_mwc_samples, main = "MWC Random Distributions", show_density = TRUE)
# stacked_hist_plot(rbind(all_mwc_samples, all_random_samples), main = "Full Sample", show_density = TRUE)

# # ========== 8. Strategy Evaluation and Round Robin ==========

# all_mwc_results <- blotto_compare(all_mwc_samples, game_weights = blotto_weights, tie_method = "p2wins")

# cat("\n🏆 Top 10 Strategies:\n")
# print(head(all_mwc_results, 10))

# cat("\n🔻 Bottom 10 Strategies:\n")
# print(tail(all_mwc_results, 10))

# cat("\n📊 Summary Stats:\n")
# cat("→ Mean Win %:", round(mean(all_mwc_results$Win_Percentage), 2), "\n")
# cat("→ Max Win %:", max(all_mwc_results$Win_Percentage), "\n")
# cat("→ Min Win %:", min(all_mwc_results$Win_Percentage), "\n")
# cat("→ # of Unique Ranks:", length(unique(all_mwc_results$Rank)), "\n")

# sample_strategies_raw <- all_random_samples[sample(nrow(all_random_samples), 100000), , drop = FALSE]
# sample_strategies <- drop_irrational_allocations(sample_strategies_raw, blotto_weights, quota = quota)
# eval_sample_strat <- evaluate_strategies(sample_strategies, blotto_weights, quota = quota)

# start_time <- Sys.time()
# all_sample_results <- blotto_compare(sample_strategies, game_weights = blotto_weights, tie_method = "p2wins")
# end_time <- Sys.time()
# cat(sprintf("\nTotal execution time: %.2f minutes\n", as.numeric(difftime(end_time, start_time, units = "mins"))))

# top_mwc_strategies <- as.matrix(all_mwc_results[1:1000, 1:7])
# top_1000_results <- blotto_compare(top_mwc_strategies, game_weights = blotto_weights, tie_method = "p2wins")
