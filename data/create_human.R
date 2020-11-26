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

# Convert the GNI column to numeric.
human$Gross_National_Income_GNI_per_Capita <- sub(",", "", human$Gross_National_Income_GNI_per_Capita) %>% as.numeric

human
# A tibble: 195 x 19

# Output the data.
#write_tsv(human, "human.tsv")

# Since I am using Tidyverse’s data handling functions, GNI is already numeric.

# The variable names in the lined meta file are not at all descriptive. I hope these were the correct ones.
# Entries for regions are also removed below, and row names assigned.
human_ <- human %>%
	rename(Percent_Representation_in_Parliament_F = Percent_Representation_in_Parliament) %>%
	select(
		Country,
		`Secondary education ratio F/M`,
		`Labour force participation ratio F/M`,
		`Expected_Years_of_Education`,
		`Life_Expectancy_at_Birth`,
		`Gross_National_Income_GNI_per_Capita`,
		`Maternal_Mortality_Ratio`,
		`Adolescent_Birth_Rate`,
		`Percent_Representation_in_Parliament_F`
	) %>% drop_na %>%
	filter(! Country %in% c(
		"Arab States",
		"East Asia and the Pacific",
		"Europe and Central Asia",
		"Latin America and the Caribbean",
		"South Asia",
		"Sub-Saharan Africa",
		"World"
	)) %>% column_to_rownames(var = "Country")

# Output as an R object file.
write_rds(human_, "human.rds")
