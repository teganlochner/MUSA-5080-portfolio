#load the tools
library(tidyverse)
library(tidycensus)

#get real data
pa_income <- get_acs(
  geography = "county",
  variables = "B19013_001",
  state = "PA",
  year = 2023,
  survey = "acs5"
)

#look at it
pa_income

#inspect it
dim(pa_income)
glimpse(pa_income)
head(pa_income)
#there are 67 rows to represent all 67 counties

#look at GEOID
pa_income$GEOID
as.numeric("01001")
#this command deletes the leading zero, this is why GEOIDs are written as text, never numbers

#filter() - picks ROWS
filter(pa_income, estimate > 60000)

#counties where the margin of error is bigger than 3000
filter(pa_income, moe > 3000)

#counties where the estimate is under 50000
filter(pa_income, estimate < 50000)

#select() - picks COLUMNS
select(pa_income, NAME, estimate, moe)

#show only GEOID and estimate
select(pa_income, GEOID, estimate)

#mutate() - makes a NEW COLUMN
mutate(pa_income, moe_pct = moe / estimate *100)
moe_pct
pa_income <- mutate(pa_income, moe_pct = moe / estimate * 100)

#arrange() - SORTS
arrange(pa_income, moe_pct)
arrange(pa_income, desc(moe_pct))

#the pipe
step1 <- filter(pa_income, moe_pct > 5)
step2 <- arrange(step1, desc(moe_pct))
step3 <- select(step2, NAME, estimate, moe, moe_pct)
step3

pa_income |>
  filter(moe_pct > 5) |>
  arrange(desc(moe_pct)) |>
  select(NAME, estimate, moe, moe_pct)

#keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
pa_income |>
  filter(moe_pct > 8) |>
  arrange(desc(estimate)) |>
  select(NAME, moe_pct)

#group_by() + summarize() - MANY rows become FEW
pa_income <- mutate(pa_income, reliable = moe_pct < 5)
pa_income |>
  group_by(reliable) |>
  summarize(n = n(),
            avg_income = mean(estimate))

#case_when() - sorting into categories
pa_income <- pa_income |>
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE ~ "Low confidence"
  ))
count(pa_income, reliability)
# 26 high, 7, low, 34 moderate

#two variables, and a shape problem
pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)

pa_two

#change the shape
pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "b19013_001",
                pop = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)

pa_wide

#THE question
pa_wide |>
  mutate(more_pct = incomeM / incomeE * 100) |>
  arrange(desc(moe_pct)) |>
  select(NAME, popE, incomeE, moe_pct) |>
  head(10)
