library(tidyverse)


bprs <- read_delim("bprs.txt", delim = " ", col_types = cols(.default = col_integer(), treatment = col_factor()))

# rats.txt is malformed in the sense that there is an extra ID column without a column header.
# Solve the issue by skipping the column headers, specifying correct headers as a parameter to read_delim()
# and finally dropping the extra ID column.
# Additionally, the factor variables are read as such. I decided to keep the identifier columns as integers
# and handle the values in the course diary if needed.
rats <- read_delim(
	"rats.txt",
	skip = 1,
	delim = "\t",
	col_types = cols(.default = col_integer(), ID_text = col_character(), Group = col_factor()),
	col_names = c("ID_text", "ID", "Group", "WD1", "WD8", "WD15", "WD22", "WD29", "WD36", "WD43", "WD44", "WD50", "WD57", "WD64")
) %>% select(-ID_text)

# Check the structure of the data.
str(bprs)
bprs
summary(bprs)
str(rats)
rats
summary(rats)

# Convert to long form.
bprs_ <- bprs %>%
	gather(key = "week", value = "value", -c(treatment, subject)) %>%
	mutate(week = as.integer(substring(week, 5)))

rats_ <- rats %>%
	gather(key = "Day", value = "value", -c(ID, Group)) %>%
	mutate(Day = as.integer(substring(Day, 3)))

# The long form data has one observation per row whereas wid form data has one observation per column on a given row.
# (I fail to see why understanding this difference is emphasised so much in the exercise text. Perhaps the text
# refers to some other difference.)
# Some of the same summaries as before can be calculated with the long form data like so:
bprs_ %>% group_by(week) %>% summarise(mean = mean(value), sd = sd(value), median = median(value), min = min(value), max = max(value))

# A more interesting option is to group by treatment in addition to week:
bprs_ %>% group_by(week, treatment) %>% summarise(mean = mean(value), sd = sd(value), median = median(value), min = min(value), max = max(value))

# Output the data.
write_rds(bprs_, "bprs.rds")
write_rds(rats_, "rats.rds")
