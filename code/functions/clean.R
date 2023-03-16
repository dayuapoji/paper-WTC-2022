# UTILITIES

# ================================================================================

'%!in%' <- function(x,y)!('%in%'(x,y))

# ================================================================================

set_df <- function(df) {
  df <- df %>%
    .[-c(1), ] %>%
    lapply(., as.numeric) %>%
    as_tibble(.)
  return(df)
}

# ================================================================================

replace_zero <- function(x) {
  y <- x
  y[x == 0] <- NA
  return(y)
}

replace_zero_df <- function(df) {
  df <- lapply(df, replace_zero) %>% 
    as_tibble(.)
  return(df)
}



# ================================================================================

replace_negative <- function(x) {
  y <- x
  y[x < 0] <- NA
  return(y)
}

replace_negative_df <- function(df) {
  df <- lapply(df, replace_negative) %>% 
    as_tibble(.)
  return(df)
}

# ================================================================================
replace_outliers <- function(x, na.rm = TRUE) {
  Q1 <- quantile(x, probs=.25, na.rm = na.rm)
  Q3 <- quantile(x, probs=.75, na.rm = na.rm)
  IQR = Q3-Q1
  
  # lower_limit = Q1 - (IQR * 1.5)
  upper_limit = Q3 + (IQR * 10) 

  y <- x
  # y[x < lower_limit] <- NA
  y[x > upper_limit] <- NA
  return(y)
}

replace_outliers_df <- function(df) {
  df <- lapply(df, replace_outliers) %>% 
    as_tibble(.)
  return(df)
}
