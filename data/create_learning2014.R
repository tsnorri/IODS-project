#
# Tuukka Norri
# 3rd November 2020
# RStudio Exercise 2: Data Wrangling
#

# I use the Tidyverse library since I’m more familiar with it than R’s built-in functions.
# To run the code, please do install.packages("tidyverse")

library("tidyverse")

df <- read_tsv("JYTOPKYS3-data.txt", col_types = cols(
  .default = col_integer(),
  gender = col_character()
))

# The data set contains 183 rows and 60 columns.
# Values for all columns except for “gender” are integers.
# The distinct values for gender are “F” and “M”, checked with the following:
# df %>% select(gender) %>% arrange() %>% unique()

# Analysis data set.
df_ <- df %>%
	mutate(
		d_sm = D03 + D11 + D19 + D27,
		d_ri = D07 + D14 + D22 + D30,
		d_ue = D06 + D15 + D23 + D31,
		st_os = ST01 + ST09 + ST17 + ST25,
		st_tm = ST04 + ST12 + ST20 + ST28,
		su_lp = SU02 + SU10 + SU18 + SU26,
		su_um = SU05 + SU13 + SU21 + SU29,
		su_sb = SU08 + SU16 + SU24 + SU32
	) %>%
	mutate(
		deep = (d_sm + d_ri + d_ue) / 12.0,
		stra = (st_os + st_tm) / 8.0,
		surf = (su_lp + su_um + su_sb) / 12.0
	) %>%
	select(gender, Age, Attitude, deep, stra, surf, Points) %>%
	filter(Points != 0)

# Use readr’s write_tsv (part of tidyverse).
# Uncomment to test.
#write_tsv(df_, "learning2014.tsv")

# Read again using read_tsv.
df2 <- read_tsv("learning2014.tsv", col_types = cols(
  .default = col_double(),
  gender = col_character(),
  Age = col_integer(),
  Attitude = col_integer(),
  Points = col_integer()
))

head(df2)
str(df2)
