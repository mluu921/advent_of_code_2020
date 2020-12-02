library(tidyverse)

## read the data 
df <- readxl::read_excel(here::here('data/data_aoc_day2.xlsx'), col_names = 'x')

## separate the columns 
df <- df %>%
  separate(
    x, c('policy', 'password'), ': '
  ) %>%
  separate(
    policy, c('criteria', 'char'), ' '
  ) %>%
  separate(
    criteria, c('criteria_min', 'criteria_max'), '-'
  ) %>%
  mutate(
    across(c(criteria_min, criteria_max), ~ as.numeric(.x))
  )


# part 1 ------------------------------------------------------------------


## count the valids 
temp <- df %>%
  mutate(
    char_count = str_count(password, char)
  )

## confirm validity
temp <- temp %>%
  mutate(
    valid = pmap_dbl(
      list(criteria_min, criteria_max, char_count), ~ 
        ifelse(..3 %in% ..1:..2, 1, 0)
    )
  )

## get the total valids
temp %>%
  summarise(
    total_valid = sum(valid)
  )


# part 2 ------------------------------------------------------------------


find_positions <- function(password, char) {
  str_locate_all(password, char)[[1]][,1]
}

temp <- df %>%
  mutate(
    positions = map2(password, char, ~ find_positions(..1, ..2))
  )

temp <- temp %>%
  mutate(
    position1_valid = map2_dbl(criteria_min, positions, ~ ifelse(..1 %in% ..2, 1, 0)),
    position2_valid = map2_dbl(criteria_max, positions, ~ ifelse(..1 %in% ..2, 1, 0))
  )

temp %>%
  mutate(
    position_sum_valid = position1_valid + position2_valid
  ) %>%
  summarise(
    total_valid = sum(position_sum_valid == 1)
  )
