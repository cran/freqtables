## ----message=FALSE------------------------------------------------------------
library(dplyr)
library(freqtables)

## -----------------------------------------------------------------------------
data(mtcars)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, ci_type = "wald")

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(am, percent_ci = 99)

## -----------------------------------------------------------------------------
mtcars %>% 
  freq_table(cyl, am)

