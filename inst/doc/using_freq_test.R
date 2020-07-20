## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(freqtables)
library(dplyr)

## -----------------------------------------------------------------------------
data(mtcars)

## -----------------------------------------------------------------------------
set.seed(123)
students <- tibble(
  male = rbinom(20, 1, 0.5)
)

## -----------------------------------------------------------------------------
students %>% 
  freq_table(male)

## -----------------------------------------------------------------------------
students %>% 
  freq_table(male) %>% 
  
  # Test for equal proportions
  summarise(
    p_bar = n[1] / n_total[1],
    p0 = 0.5,
    n = n_total[1],
    z = abs((p_bar - p0)) / sqrt(p0 * (1 - p0) / n),
    p = 2 * pnorm(z, lower.tail=FALSE)
  )

## -----------------------------------------------------------------------------
prop.test(9, 20, p = 0.5, correct = FALSE)

## -----------------------------------------------------------------------------
chisq.test(c(9, 11), p = c(0.5, 0.5), correct = FALSE)

## -----------------------------------------------------------------------------
students %>% 
  freq_table(male) %>% 
  freq_test()

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am) %>% 
  freq_test() %>% 
  select(1:6, p_chi2_pearson)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(cyl) %>% 
  freq_test()

## -----------------------------------------------------------------------------
set.seed(456)
students <- tibble(
  male  = rbinom(20, 1, 0.5),
  binge = if_else(male == 0, 
                  rbinom(20, 1, 0.10), # 10% of females binge drink 
                  rbinom(20, 1, 0.30)) # 30% of males binge drink
)

## -----------------------------------------------------------------------------
students %>% 
  freq_table(male, binge) %>% 
  freq_test()

## -----------------------------------------------------------------------------
students %>% 
  freq_table(male, binge) %>% 
  freq_test()

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, vs) %>% 
  freq_test() %>% 
  select(1:8, p_chi2_pearson)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, cyl) %>% 
  freq_test()

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, cyl) %>% 
  freq_test()

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, vs) %>% 
  freq_test() %>% 
  select(1:4, n_col, n_total, n_expected:chi2_pearson)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, vs) %>% 
  freq_test() %>% 
  select(1:3, chi2_pearson:p_chi2_pearson)

## -----------------------------------------------------------------------------
chisq.test(mtcars$am, mtcars$vs, correct = FALSE)

## -----------------------------------------------------------------------------
m_observed <- matrix(c(5, 0, 1, 4), nrow = 2, byrow = TRUE)
m_observed

## -----------------------------------------------------------------------------
p <- function(m) {
  
  # Calculate the marginal totals
  r1  <- sum(m[1, ])
  r2  <- sum(m[2, ])
  c1  <- sum(m[, 1])
  c2  <- sum(m[, 2])
  tot <- sum(m)
  
  # Calculate the conditional probability of getting the actual matrix given 
  # the particular row and column sums
  r1_fac  <- factorial(r1)
  r2_fac  <- factorial(r2)
  c1_fac  <- factorial(c1)
  c2_fac  <- factorial(c2)
  tot_fac <- factorial(tot)
  
  m_fac      <- factorial(m) # factorial of each cell of the matrix (i.e., 5!, 0!, 1!, 4!)
  prod_m_fac <- prod(m_fac)  # Multiply all of those values together
  
  numerator   <- r1_fac * r2_fac * c1_fac * c2_fac
  denominator <- tot_fac * (prod_m_fac)
  p <- numerator / denominator
  
  # Return p
  p
}

## -----------------------------------------------------------------------------
p_cutoff <- p(m_observed)
p_cutoff

## -----------------------------------------------------------------------------
# Create a empty vector to hold the p values created in the loop below
p_values <- vector(mode = "numeric")

# Create a list of all possible combinations, given our margins
combinations <- list(
  c(4, 1 ,2, 3),
  c(3, 2, 3, 2),
  c(2, 3, 4, 1),
  c(1, 4, 5, 0)
)

# Calculate the p value for each combination and save it
for (i in seq_along(combinations)) {
  
  # Turn into a matrix
  m <- matrix(combinations[[i]], nrow = 2, byrow = TRUE)
  
  # Caclulate p
  p_val <- p(m)
  
  # Save p to vector
  p_values <- c(p_values, p_val)
}

## -----------------------------------------------------------------------------
sum(p_values, p_cutoff)

## -----------------------------------------------------------------------------
df <- tibble(
  p_value  = p_values,
  p_cutoff = p_cutoff
) %>% 
  mutate(
    less_or_equal = p_value <= p_cutoff
  ) %>% 
  print()

## -----------------------------------------------------------------------------
df %>% 
  filter(less_or_equal) %>% 
  summarise(
    `p-value` = sum(p_value, p_cutoff)
  )

## -----------------------------------------------------------------------------
fisher.test(m_observed)

## ----echo=FALSE---------------------------------------------------------------
# Clean up
rm(combinations, df, m, m_observed, i, p_cutoff, p_val, p_values, p)

