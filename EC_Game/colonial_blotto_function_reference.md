
# Function Reference: `colonial_blotto.R`

This reference outlines all functions defined in the `colonial_blotto.R` file, including their purpose, key inputs, outputs, and usage context.

---

## 🔧 Utility Functions

### `all_combinations(vector_weights)`
- Generate all possible combinations of players (by index).
- **Input:** vector_weights (only length is used)
- **Output:** List of index combinations
- **Used in:** `find_mwcs()`

### `is_minimum(x, quota)`
- Determine if a coalition is minimal.
- **Input:** vector of weights in a coalition, quota
- **Output:** TRUE/FALSE

### `find_mwcs(vector_weights, quota)`
- Find minimal winning coalitions (MWCs).
- **Output:** List of coalitions

### `drop_irrational_allocations(matrix_data, vector_weights, quota)`
- Remove strategies that fail to meet the quota.
- **Output:** Filtered strategy matrix

### `deduplicate_combinations(mat)`
- Remove duplicate rows in a matrix.

---

## 🔢 Allocation Functions

### `adjust_allocation(raw_values, target_sum, bias_weights)`
- Normalize and round a vector to sum to `target_sum`.

### `apportion_values(values, target_sum, method, initial_seats)`
- Classical apportionment methods.

### `generate_random_allocations(...)`
- Generate a random allocation using probabilistic methods.

### `new_combinations(...)`
- Smart generator for Blotto strategies with quota enforcement.

---

## 🧮 Power Index

### `banzhaf(member, vector_weights, quota)`
- Banzhaf power index.

---

## 🎯 Strategy Evaluation

### `blotto_compare(...)`
- Core head-to-head evaluation engine.

### `evaluate_strategies(...)`
- Report coverage, zero distributions, and rationality.

---

## 📊 Visualization

### `stacked_hist_plot(...)`
- Visualize allocation distributions.

### `validate_sampling_methods(...)`
- Compare outputs from various randomization methods.

---

## 🔁 MWC Sampling

### `sample_mwc_distributions(...)`
- Generate sample distributions for each MWC.

---

## 🧠 Misc

### `progress_reporter(...)`
- Utility for console progress logging.
