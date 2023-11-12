# 0. Libraries ---
library(fastverse)
library(readr)
library(rmarkdown)
library(DT)
library(stringr)
library(ggplot2)
library(joyn)
library(waldo)
library(dplyr)
library(purrr)

# Data ----
tag      <- "202311081903"
base_url <- "https://github.com/randrescastaneda/pub_data/raw/"
data_url <- paste0(base_url, tag, "/data/Rtest1/")

wdi <-
  readr::read_rds(paste0(data_url, "wdi_in1.Rds"))

l_svy <- 
  readr::read_rds(paste0(data_url, "svy_sim_in1.Rds"))


# Basic Stats ----
## 1. Summary Statistics ----
wdi |> 
  fgroup_by(region, date) |>
  fsummarise(
    N = fnobs(gdp),
    mean = round(fmean(gdp, w = pop, na.rm=TRUE)),
    SD = round(fsd(gdp, w = pop, na.rm=TRUE)),
    Min = round(fmin(gdp, na.rm=TRUE)),
    Max = round(fmax(gdp, na.rm=TRUE))
  )


## 2. Aggregate Stats ----
wdi |>
  fselect(region, date, lifeex, gdp, pov_intl, pop) |>
  fgroup_by(region, date) |>
  fsummarise(across(c(lifeex, gdp, pov_intl), 
                    list(mean = fmean, sd = fsd, min = fmin, max = fmax), 
                    w = pop),
             pop = fsum(pop)) |>
  pivot(ids = c('region', 'date', 'pop'),
        values = c('lifeex_mean', 'lifeex_sd', 'lifeex_min', 'lifeex_max', 
                   'gdp_mean', 'gdp_sd', 'gdp_min', 'gdp_max', 
                   'pov_intl_mean', 'pov_intl_sd', 'pov_intl_min', 'pov_intl_max')) |>
  fmutate(variable2 = str_extract(variable, ".*?(?=_[^_]+$)"),
          estimate = str_extract(variable, "(mean|sd|min|max)$")) |>
  fselect(-variable) |>
  pivot(how = "wider",
        ids = c('region', 'date', 'estimate', 'pop'),
        names = c('variable2'),
        values = c('value')) |>
  fselect(estimate, region, date, pop, lifeex, gdp, pov_intl)

## 3.1 Find Outliers ----
# Find the outliers of lifeex, gpd, and gini by year above and below 2.5 standard 
# deviations from the mean. 
# Ignore NAs in all your calculations. Remember to weigh by population.

### Load Data to replicate----
wdi_outliers_out<-readr::read_rds(paste0(data_url, "wdi_outliers_out.Rds"))

### Yearly stats ----
yearly_stats <- wdi|>
  fgroup_by(date) |>
  fsummarise(
    mean_lifeex = fmean(lifeex, na.rm = TRUE, w = pop),
    sd_lifeex = fsd(lifeex, na.rm = TRUE, w = pop),
    mean_gdp = fmean(gdp, na.rm = TRUE, w = pop),
    sd_gdp = fsd(gdp, na.rm = TRUE, w = pop),
    mean_gini = fmean(gini, na.rm = TRUE, w = pop),
    sd_gini = fsd(gini, na.rm = TRUE, w = pop)
  )

### Outlier Function ----
is_low_outlier <- function(value, mean, sd) {
  threshold = mean - 2.5 * sd
  return(value < threshold)
}

is_high_outlier <- function(value, mean, sd) {
  threshold = mean + 2.5 * sd
  return(value > threshold)
}

### Calculate Outliers ----
wdi_outliers <- wdi |>
  joyn::merge(yearly_stats, by = "date") |> # had to try your package!
  fmutate(
    hl_lifeex = ifelse(!is.na(lifeex), is_high_outlier(lifeex, mean_lifeex, sd_lifeex), NA),
    ll_lifeex = ifelse(!is.na(lifeex), is_low_outlier(lifeex, mean_lifeex, sd_lifeex), NA),
    hl_gdp = ifelse(!is.na(gdp), is_high_outlier(gdp, mean_gdp, sd_gdp), NA),
    ll_gdp = ifelse(!is.na(gdp), is_low_outlier(gdp, mean_gdp, sd_gdp), NA),
    hl_gini = ifelse(!is.na(gini), is_high_outlier(gini, mean_gini, sd_gini), NA),
    ll_gini = ifelse(!is.na(gini), is_low_outlier(gini, mean_gini, sd_gini), NA)
  )


### Quick Compare ----
# Manual
wdi_outliers_out |>
  fsubset(country == "Angola") |>
  fselect(date, country, gdp, gini, lifeex, pop, mean_lifeex, sd_lifeex, 
          mean_gini, sd_gini, mean_gdp, sd_gdp, hl_lifeex, ll_lifeex, 
          hl_gdp, ll_gdp, hl_gini, ll_gini) ->angola_old


wdi_outliers |>
  fsubset(country == "Angola") |>
  fselect(date, country, gdp, gini, lifeex, pop, mean_lifeex, sd_lifeex, 
          mean_gini, sd_gini, mean_gdp, sd_gdp, hl_lifeex, ll_lifeex, 
          hl_gdp, ll_gdp, hl_gini, ll_gini) -> angola_new

# Waldo
waldo::compare(angola_old, angola_new) 


## 3.2 Plot Lifeex Outliers ----
wdi_outliers |>
  ggplot() +
  geom_point(aes(x = date, y = lifeex, color = region), size = 1) +
  geom_line(aes(x = date, y = mean_lifeex), linewidth = 0.5, colour = 'blue') +
  geom_ribbon(aes(x = date, ymin = mean_lifeex - sd_lifeex*2.5, 
                  ymax = mean_lifeex + sd_lifeex*2.5), alpha = 0.2)+
  theme_minimal()+
  labs(x = "Date", y = "Life Exp", color = NULL) +
  theme(legend.position=c(.5,.15), legend.direction = "horizontal")

# Simulation Data ----
## Load Data ----
## 4.1 Poverty measures ----
# Assuming that 'area' does not count?
poverty_lines <- c(2.15, 3.65, 6.85)

### Test with one survey ----
one_survey <- l_svy[1]$Y2001

one_survey |>
  fmutate(
    poor = ifelse(income <= 2.15, 1, 0),
    poor_weight = poor * weight,
    poor_gap = ifelse(poor == 1, 2.15 - income, 0)
  ) |>
  fsummarise(
    FGT0 = fsum(poor_weight) / fsum(weight),
    FGT1 = fsum(poor_gap * poor_weight) / (2.15 * fsum(weight)),
    FGT2 = fsum((poor_gap / 2.15)^2 * poor_weight) /fsum(weight))

### function ---
calculate_FGT_indices <- function(survey, poverty_line) {
  survey %>%
    mutate(
      poor = ifelse(income <= poverty_line, 1, 0),
      poor_weight = poor * weight,
      poor_gap = ifelse(poor == 1, poverty_line - income, 0)
    ) %>%
    summarise(
      headcount = sum(poor_weight) / sum(weight),
      povgap = sum(poor_gap * poor_weight) / (poverty_line * sum(weight)),
      povseverity = sum((poor_gap / poverty_line)^2 * poor_weight) / sum(weight)
    )
}

calculate_FGT_indices_v2 <- function(survey, poverty_line) {
  survey %>%
    mutate(
      poor = ifelse(income <= poverty_line, 1, 0),
      poor_weight = poor * weight,
      poor_gap = ifelse(poor == 1, poverty_line - income, 0)
    ) %>%
    summarise(
      headcount = sum(poor_weight) / sum(weight),
      povgap = sum(poor_gap * poor_weight) / (poverty_line * sum(weight)),
      povseverity = sum((poor_gap / poverty_line)^2 * poor_weight) / sum(weight)
    )
}

results <- map(names(l_svy), ~{
  year <- .x
  survey_data <- l_svy[[year]]
  map(poverty_lines, ~{
    line <- .x
    fgt_results <- calculate_FGT_indices_v2(survey_data, line)
    data.table(year = year, pov_line = line, fgt_results)
  }) %>% bind_rows()
}) %>% bind_rows()




### Apply as a data.table ----
svy_dt <- data.table(year = character(), pov_line = numeric(), 
                     headcount = numeric(), povgap = numeric(), povseverity = numeric())

year_names <- names(l_svy)

for (year in year_names) {
  survey_data <- l_svy[[year]]
  for (line in poverty_lines) {
    fgt_results <- calculate_FGT_indices(survey_data, line)
    svy_dt <- rbindlist(list(svy_dt, data.table(year = year, pov_line = line, fgt_results)), 
                        use.names = TRUE)
  }
}


## 4.2 Poverty headcount graph ----
svy_dt |>
  fmutate(year = str_remove(year, "Y")) |>
  ggplot(aes(x = year, y = headcount, 
             group = factor(pov_line),
             colour = factor(pov_line))) +
  geom_line(linewidth = 0.3) +
  geom_point(size = 1) +
  theme_minimal()+
  labs(x = "Year", y = "Headcount", color = NULL)+
  theme(legend.position='bottom')

## 5.1 Lorenz Curve ----
### Test with one survey ----
one_survey |>
  fmutate(cumsum_pop = fcumsum(weight),
          sum_pop = fsum(weight),
          cumsum_income = fcumsum(income * weight),
          sum_income = fsum(income * weight)) |>
  fmutate(cum_share_pop = cumsum_pop / sum_pop,
          cum_share_income = cumsum_income / sum_income) |>
  fmutate(percentile_group = floor(cum_share_pop * 100)) |>
  fgroup_by(percentile_group) |>
  fsummarise(
    cum_share_pop = first(cum_share_pop),
    cum_share_welfare = first(cum_share_income),
    welfare = first(income)) |>
  fmutate(bin = percentile_group)


### Function ----
calculate_lorenz_data <- function(survey) {
  
  survey |>
    fmutate(cumsum_pop = fcumsum(weight),
            sum_pop = fsum(weight),
            cumsum_income = fcumsum(income * weight),
            sum_income = fsum(income * weight)) |>
    fmutate(cum_share_pop = cumsum_pop / sum_pop,
            cum_share_income = cumsum_income / sum_income) |>
    fmutate(percentile_group = floor(cum_share_pop * 100)) |>
    fgroup_by(percentile_group) |>
    fsummarise(
      cum_share_pop = first(cum_share_pop),
      cum_share_welfare = first(cum_share_income),
      welfare = first(income)) |>
    fmutate(bin = percentile_group) |>
    fsubset(bin > 0)
}


### Apply using map_df() ----
process_survey <- function(survey, year_name) {
  numeric_year <- as.numeric(sub("Y", "", year_name))
  lorenz_results <- calculate_lorenz_data(survey)
  
  # Add the year to the results
  lorenz_results$year <- numeric_year
  return(lorenz_results)
}

lorenz_dt <- map_df(names(l_svy), ~process_survey(l_svy[[.x]], .x))

### Apply using map() because map_df() is superseded ----
lorenz_dt <- map(names(l_svy), ~process_survey(l_svy[[.x]], .x)) %>%
  bind_rows()

## 5.3 Lorenz Curve Graph ----
lorenz_dt |>
  ggplot(aes(x = cum_share_pop, y = cum_share_welfare, 
             group = year, colour = factor(year))) +
  geom_line(size = 0.3) +
  theme_minimal()+
  labs(x = "Cumulative Share of Population", y = "Cumulative Share of Welfare", color = NULL)+
  theme(legend.position=c(0.1, 0.6))




