#
# Tuukka Norri
# 6th November 2020
# RStudio Exercise 3: Data Wrangling
#

library(tidyverse)

# Read the data.
student_por <- read_csv2("student-por.csv", col_types = cols(
  .default = col_character(),
  age = col_integer(),
  Medu = col_integer(),
  Fedu = col_integer(),
  traveltime = col_integer(),
  studytime = col_integer(),
  failures = col_integer(),
  famrel = col_integer(),
  freetime = col_integer(),
  goout = col_integer(),
  Dalc = col_integer(),
  Walc = col_integer(),
  health = col_integer(),
  absences = col_integer(),
  G1 = col_integer(),
  G2 = col_integer(),
  G3 = col_integer()
))

student_mat <- read_csv2("student-mat.csv", col_types = cols(
  .default = col_character(),
  age = col_integer(),
  Medu = col_integer(),
  Fedu = col_integer(),
  traveltime = col_integer(),
  studytime = col_integer(),
  failures = col_integer(),
  famrel = col_integer(),
  freetime = col_integer(),
  goout = col_integer(),
  Dalc = col_integer(),
  Walc = col_integer(),
  health = col_integer(),
  absences = col_integer(),
  G1 = col_integer(),
  G2 = col_integer(),
  G3 = col_integer()
))

# Create the cartesian product of the data with the given join columns.
join_columns = c("school", "sex", "age", "address", "famsize", "Pstatus", "Medu", "Fedu", "Mjob", "Fjob", "reason", "nursery", "internet")
df1 <- inner_join(student_mat, student_por, by = join_columns, suffix = c(".math", ".por"))
df2 <- df1 %>% select(one_of(join_columns))

# student_por contains 649 rows, student_mat 395 rows and df1 382 rows.

# Determine the other (non-joining) column names.
cn <- colnames(student_mat)
non_joining <- cn[!(cn %in% join_columns)]

# Iterate over the non-joining columns (in student_mat).
# For numeric columns, copy the mean value of the two values given to df2.
# For others, take the left one.
for (cn in non_joining) {
	cp <- select(df1, starts_with(cn))
	lhs <- cp[[1]]
	if (is.numeric(lhs)) {
		df2[cn] <- round(rowMeans(cp))
	}
	else {
		df2[cn] <- lhs
	}
}

# Add the mean alcohol use column.
df2 <- df2 %>%
	mutate(alc_use = ave(Dalc, Walc)) %>%
	mutate(high_use = alc_use > 2)

glimpse(df2)
write_tsv(df2, "student_alcohol_use.tsv")
