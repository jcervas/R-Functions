# ========== ecgame_run.R — Cleaned and Modularized Execution Script ==========

# ========== 1. Setup ==========
rm(list = ls(all = TRUE))                            # Clear environment
options(scipen = 999)                                # Avoid scientific notation
set.seed(66)                                         # Reproducibility

# Load main game functions
source('/Users/cervas/.../colonial_blotto.R')

# Define Electoral College weights and quota
ec_weights <- c(3, 5, 8, 13, 21, 34, 55)
quota <- 70
names(ec_weights) <- LETTERS[1:7]

# ========== 2. Power Indices and Coalitions ==========
ec_Banzhaf <- banzhaf(names(ec_weights), ec_weights, quota)    # Banzhaf index
mwcs <- find_mwcs(ec_weights, quota)                           # Minimal winning coalitions

# ========== 3. Load and Process User Data ==========
user_data <- jsonlite::read_json('/Users/cervas/.../results.json')
user_matrix_raw <- as.matrix(do.call(rbind, lapply(user_data, unlist)))
user_matrix <- deduplicate_combinations(user_matrix_raw)

# Normalize to sum to 100
user_matrix_adj <- t(apply(user_matrix, 1, apportion_values, target_sum = 100))

# ========== 4. Evaluate User Strategies ==========
results <- evaluate_strategies(user_matrix_adj, ec_weights, quota)
summary_table <- results$position_summary
colnames(summary_table) <- c("State", "Zeroes", "Zero (%)", "Mean", "Median", "SD", "Min", "Max")
summary_table$State <- LETTERS[1:7]

# Print table
knitr::kable(summary_table, format = "simple", align = "c",
             caption = "Summary of Allocation Statistics by State (N = 214)")

# ========== 5. Normalized Means ==========
normalize <- function(x) x / sum(x)

ec_means <- normalize(ec_weights)
banzhaf_means <- normalize(ec_Banzhaf)
user_means <- normalize(colMeans(user_matrix_adj))
user_medians <- normalize(apply(user_matrix_adj, 2, median))

data_matrix <- t(rbind(ec_means, banzhaf_means, user_means, user_medians))
colnames(data_matrix) <- c("Vote-Share Weights", "Banzhaf", "Avg Allocation", "Median Allocation")
rownames(data_matrix) <- LETTERS[1:7]

# Bar plot
barplot(t(data_matrix) * 100, beside = TRUE, col = gray.colors(4, 0.1, 0.9),
        ylim = c(0, 50), yaxt = "n", names.arg = rownames(data_matrix),
        legend.text = colnames(data_matrix),
        args.legend = list(x = "topleft", bty = "n", inset = 0.01), las = 1)
axis(2, at = seq(0, 50, 10), labels = paste0(seq(0, 50, 10), "%"), las = 1)

# ========== 6. Hypothesis Testing ==========
# --- Regression Models ---
models <- list(
  mean_ec = lm(user_means ~ ec_means),
  median_ec = lm(user_medians ~ ec_means),
  mean_bz = lm(user_means ~ banzhaf_means),
  median_bz = lm(user_medians ~ banzhaf_means)
)

# Extract regression info
extract_model_info <- function(model, predictor, outcome, label) {
  s <- summary(model)
  data.frame(
    Model = label,
    Predictor = predictor,
    Outcome = outcome,
    `Adj. R²` = round(s$adj.r.squared, 2),
    AIC = round(AIC(model), 2),
    `t-value` = round(s$coefficients[2, "t value"], 2),
    check.names = FALSE
  )
}

comparison_table <- rbind(
  extract_model_info(models$mean_ec, "EC Weights", "Mean Allocation", "Model 1"),
  extract_model_info(models$mean_bz, "Banzhaf Power", "Mean Allocation", "Model 2"),
  extract_model_info(models$median_ec, "EC Weights", "Median Allocation", "Model 3"),
  extract_model_info(models$median_bz, "Banzhaf Power", "Median Allocation", "Model 4")
)

knitr::kable(comparison_table, format = "simple",
             caption = "Comparison of Regression Models: EC Weights vs. Banzhaf Power")

# ========== 7. MSE Comparison ==========
mse <- function(x, ref) mean((x - ref)^2)
user_mse_ec <- apply(user_matrix_adj, 1, mse, ref = ec_means)
user_mse_bz <- apply(user_matrix_adj, 1, mse, ref = banzhaf_means)

tt <- t.test(user_mse_ec, user_mse_bz, paired = TRUE)
p_str <- if (tt$p.value < 0.001) "< 0.001" else paste0("= ", signif(tt$p.value, 3))
cat(sprintf("t(%d) = %.2f, p %s, 95%% CI = [%.3f, %.3f].",
            tt$parameter, tt$statistic, p_str, tt$conf.int[1], tt$conf.int[2]))

# Chi-squared on frequency of better fit
freq_table <- table(Banzhaf = user_mse_bz < user_mse_ec, EC = user_mse_ec < user_mse_bz)
cs <- chisq.test(c(Banzhaf = sum(user_mse_bz < user_mse_ec),
                   EC = sum(user_mse_ec < user_mse_bz)))
p_str <- if (cs$p.value < 0.001) "< 0.001" else paste0("= ", signif(cs$p.value, 3))
cat(sprintf("χ²(%d, N = %d) = %.2f, p %s.", cs$parameter, sum(freq_table), cs$statistic, p_str))

# ========== 8. Hypotheses 2–5 (Strategy Structure) ==========
# Cluster proximity to MWCs
strict_assignments <- assign_strict_mwc(user_matrix_adj, mwcs)
percent_clustered <- round(mean(!is.na(strict_assignments)) * 100, 1)

# Distribution of active states
active_states <- rowSums(user_matrix_adj > 0)
active_states_table <- table(active_states)
percent <- round(100 * active_states_table / sum(active_states_table), 1)
df <- rbind(Frequency = as.vector(active_states_table), Percentage = percent)
colnames(df) <- names(active_states_table)
knitr::kable(df, format = "simple", caption = "States Allocated to per Strategy")

# Entropy and variance
entropy <- -sum(prop.table(active_states_table) * log2(prop.table(active_states_table))) / log2(length(active_states_table))
cat("• Variance of active state count: ", round(var(active_states), 2),
    "\n• Shannon entropy: ", round(entropy, 2), " (normalized)")

# F vs. E preference
F_mean <- mean(user_matrix_adj[,6])
E_mean <- mean(user_matrix_adj[,5])
cat("• Mean F: ", round(F_mean, 2), " | Mean E: ", round(E_mean, 2), "\n")
cat("Wilcoxon p:", wilcox.test(user_matrix_adj[,6], user_matrix_adj[,5], paired = TRUE)$p.value, "\n")
cat("T-test p:", t.test(user_matrix_adj[,6], user_matrix_adj[,5])$p.value, "\n")

# Rationality check
cat("Total raw strategies:", nrow(user_matrix_raw), "\n")
cat("Adjusted:", nrow(user_matrix_adj), "\n")
cat("Underfunded:", sum(rowSums(user_matrix) < 100), "\n")
cat("Overfunded:", sum(rowSums(user_matrix) > 100), "\n")

# ========== 9. Round-Robin Comparison of Participant Strategies ==========

# Evaluate strategies using two tie-breaking rules
compare_coinflip <- blotto_compare(user_matrix_adj, ec_weights, tie_method = "cointoss")
compare_p2wins   <- blotto_compare(user_matrix_adj, ec_weights, tie_method = "p2wins")

compare_coinflip_df <- as.data.frame(compare_coinflip)
compare_p2wins_df   <- as.data.frame(compare_p2wins)

# Compute ranks
rank_cf <- rank(-compare_coinflip_df$Win_Count, ties.method = "first")
rank_p2 <- rank(-compare_p2wins_df$Win_Count, ties.method = "first")
names(rank_cf) <- rownames(compare_coinflip_df)
names(rank_p2) <- rownames(compare_p2wins_df)

# Classify movers
cf_top <- rank_cf <= floor(length(rank_cf) / 2)
p2_top <- rank_p2 <= floor(length(rank_p2) / 2)
moved_up_ids   <- names(rank_cf)[!cf_top & p2_top]
moved_down_ids <- names(rank_cf)[cf_top & !p2_top]

# Plot rank stability
point_color <- rep("gray70", length(rank_cf))
point_color[moved_up_ids] <- "blue"
point_color[moved_down_ids] <- "red"

plot(rank_cf, rank_p2, pch = 21, bg = point_color, col = "black",
     xlab = "Rank under Coin Flip", ylab = "Rank under Player 2 Wins",
     main = "Rank Stability with Movers Highlighted")
abline(0, 1, col = "darkgray", lty = 2)
grid()
legend("bottomright", legend = c("Stable", "Moved Up", "Moved Down"),
       pt.bg = c("gray70", "blue", "red"), pch = 21, title = "Strategy Type")

# Report average ranks of movers
avg_up_cf <- mean(rank_cf[moved_up_ids], na.rm = TRUE)
avg_up_p2 <- mean(rank_p2[moved_up_ids], na.rm = TRUE)
avg_down_cf <- mean(rank_cf[moved_down_ids], na.rm = TRUE)
avg_down_p2 <- mean(rank_p2[moved_down_ids], na.rm = TRUE)

cat("→ Movers Up: Avg Coin Flip Rank:", round(avg_up_cf, 1),
    "| Player 2 Rank:", round(avg_up_p2, 1), "\n")
cat("→ Movers Down: Avg Coin Flip Rank:", round(avg_down_cf, 1),
    "| Player 2 Rank:", round(avg_down_p2, 1), "\n")

# Transition table (top/bottom half)
transition_table <- table(CoinFlipTop = cf_top, P2Top = p2_top)
cat("→ From bottom to top half:", sum(!cf_top & p2_top), "\n")
cat("→ From top to bottom half:", sum(cf_top & !p2_top), "\n")
print(transition_table)

# ========== 10. Clustering Analysis ==========

# Strategy clustering (k-means)
k <- 3
strategy_clusters <- kmeans(user_matrix_adj, centers = k)
cluster_labels <- factor(strategy_clusters$cluster)

# Combine with win percentages
cluster_df <- data.frame(
  cluster = cluster_labels,
  win_pct = compare_p2wins_df$Win_Percentage
)

# Mean win rate per cluster
winrate_by_cluster <- aggregate(win_pct ~ cluster, data = cluster_df, mean)

# Cluster centroids (allocation profiles)
centroids <- round(strategy_clusters$centers, 2)
rownames(centroids) <- paste("Cluster", 1:k)
colnames(centroids) <- paste("State", LETTERS[1:7])

# Print cluster profiles
knitr::kable(as.data.frame(centroids), format = "markdown",
             caption = "Allocation Profiles by Cluster")

# Bar plot of cluster allocation profiles
barplot(t(centroids), beside = TRUE,
        col = gray.colors(k),
        ylim = c(0, 75), ylab = "Average Allocation (%)",
        main = "Allocation Profiles by Strategy Cluster", las = 1)
legend("topleft", legend = rownames(centroids),
       fill = gray.colors(k), title = "Cluster")

# Cluster sizes + win rate overlay
cluster_sizes <- table(cluster_labels)
bar_midpoints <- barplot(cluster_sizes, names.arg = paste("Cluster", 1:k),
                         col = "lightgray", ylim = c(0, max(cluster_sizes) + 10),
                         ylab = "Cluster Size", main = "Cluster Size and Avg Win %")

# Overlay win rate
par(new = TRUE)
plot(bar_midpoints, winrate_by_cluster$win_pct, type = "b", col = "blue",
     axes = FALSE, xlab = "", ylab = "", pch = 19)
axis(4, col.axis = "blue", col.ticks = "blue")
mtext("Avg Win %", side = 4, line = 3, col = "blue")


# Plot predicted win probability from logistic model
encm_seq <- seq(min(results$ENCM), max(results$ENCM), length.out = 100)
pred <- predict(logit_dyadic, newdata = data.frame(ENCM = encm_seq), type = "link", se.fit = TRUE)
prob <- plogis(pred$fit)
upper <- plogis(pred$fit + 1.96 * pred$se.fit)
lower <- plogis(pred$fit - 1.96 * pred$se.fit)

plot(encm_seq, prob, type = "l", lwd = 2, col = "blue", ylim = c(0, 1),
     xlab = "ENCM", ylab = "Predicted Win Probability",
     main = "ENCM and Probability of Winning a Matchup")
polygon(c(encm_seq, rev(encm_seq)), c(lower, rev(upper)),
        col = adjustcolor("blue", alpha.f = 0.2), border = NA)
points(results$ENCM, jitter(results$Outcome, 0.1), pch = 20,
       col = adjustcolor("black", 0.05))
grid()
