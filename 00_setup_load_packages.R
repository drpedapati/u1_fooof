library(crayon)  # For colored terminal output

# Start Generation Here
cat("\n", crayon::bold$cyan("╔════════════════════════════════════════════════════════════╗"))
cat("\n", crayon::bold$cyan("║                     SETUP AND LOAD PACKAGES                 ║"))
cat("\n", crayon::bold$cyan("╚════════════════════════════════════════════════════════════╝\n"))
# End Generation Here


library(tidyverse)
library(conflicted)


library(arrow)
library(dplyr)
library(purrr)
library(fs)
library(data.table)
library(testthat)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("summarize", "dplyr")
conflict_prefer("mutate", "dplyr")
conflict_prefer("group_by", "dplyr")
conflict_prefer("ungroup", "dplyr")
conflicts_prefer(dplyr::lag)