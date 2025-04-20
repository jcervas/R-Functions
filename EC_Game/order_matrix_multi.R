# order_matrix_multi.R

#' Order a matrix or data frame by multiple columns with flexible sort directions
#'
#' @param data A data.frame or matrix to sort
#' @param order_cols A vector of column indices or names specifying the sort priority
#' @param decreasing A logical vector of the same length as order_cols specifying sort order per column
#' @return A reordered version of the input
order_matrix_multi <- function(data, order_cols, decreasing = TRUE) {
  if (length(decreasing) == 1) {
    decreasing <- rep(decreasing, length(order_cols))
  }
  if (length(decreasing) != length(order_cols)) {
    stop("Length of `decreasing` must be 1 or equal to length of `order_cols`.")
  }

  df <- as.data.frame(data)
  sort_list <- lapply(seq_along(order_cols), function(i) {
    col <- order_cols[i]
    values <- if (is.character(col)) df[[col]] else df[[col]]
    if (decreasing[i]) -xtfrm(values) else xtfrm(values)
  })

  data[do.call(order, sort_list), , drop = FALSE]
}

# Example usage:
# order_matrix_multi(score_df, c("Win_Percentage", "Win_Count"), decreasing = c(TRUE, FALSE))
# order_matrix_multi(matrix_data, c(7,6,5,4,3,2,1), decreasing = FALSE)