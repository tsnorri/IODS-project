#
# Tuukka Norri
# 19th November 2020
# RStudio Exercise 3: Data Wrangling
#

library(tidyverse)

# Once again I decided to use Tidyverse.

hd <- read.csv("human_development.csv", stringsAsFactors = F) %>% tibble
gii <- read.csv("gender_inequality.csv", stringsAsFactors = F, na.strings = "..") %>% tibble

hd
# A tibble: 195 x 8, i.e. 195 observations, 8 variables.
gii
# A tibble: 195 x 10, i.e. 195 observations, 10 variables.

summary(hd)
summary(gii)

# Rename variables by replacing runs of full stops with underscores and removing trailing underscores.
rename_fn <- function (x) { str_replace(str_replace_all(x, "[.]+", "_"), "[_]+$", "") }
hd <- rename_with(hd, rename_fn)
gii <- rename_with(gii, rename_fn)

gii <- gii %>% mutate(
	`Secondary education ratio F/M` = `Population_with_Secondary_Education_Female` / `Population_with_Secondary_Education_Male`,
	`Labour force participation ratio F/M` = `Labour_Force_Participation_Rate_Female` / `Labour_Force_Participation_Rate_Male`
)

# Join.
human <- inner_join(hd, gii, by = c("Country"))

human
# A tibble: 195 x 19

# Output the data.
write_tsv(human, "human.tsv")
