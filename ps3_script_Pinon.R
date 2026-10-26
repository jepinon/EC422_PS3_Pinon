dir.create("data")

# Part 2: Getting Comfortable with Time-Series Data in R


# 2.2 Loading the Data

library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

retail_raw <- read_csv(
  "data/retail_sales_raw.csv",
  na = ".",
  col_types = cols(
    observation_date = col_date(format = "%Y-%m-%d"),
    RSAFS = col_double(),
    RSAFSNA = col_double()
  )
)

# 2.3 Reshaping the Data

retail_raw_long <- retail_raw |>
  select(observation_date, RSAFS, RSAFSNA) |>
  pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")

head(retail_raw_long)

# 2.4 Building tsibble

library(fpp3)

retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)

head(retail_ts)

has_gaps(retail_ts)

# 2.5 Canadian Gas Production

head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# 2.6 Working with Dates in a tsibble

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# The month with the highest average production is December, and the month with the lowest is June.


can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

view(can_gas_recent)

can_gas_q <- can_gas |>
  filter(Month <= yearmonth("2004 Dec")) |>
  index_by(Quarter = yearquarter(Month)) |>
  summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)

tail(can_gas_q)

# We are summing the monthly gas production values because we want to create quarters that are made up of the aggregate of their respective months. Q1 should be equal to January production plus February production plus March production.
# The quarterly series still shows a significant degree of seasonality.

# 2.7 Plotting with autoplot()

retail_ts |> autoplot(sales)

retail_ts |>
  autoplot(sales) +
  labs(
    title = "U.S. Retail and Food Services Sales",
    subtitle = "Seasonally adjusted vs. not seasonally adjusted",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()
  
# In the series that is not seasonally adjusted, we can see a clear pattern where production is high in Q4 and Q1, and low in Q2 and Q3. This pattern is much less visible in the seasonally adjusted series.
# The two series more or less follow the same path over time, although dramatic shifts in production are less visible in the seasonally adjusted data.


ggplot(can_gas, aes(x = Month, y = Volume)) +
  geom_line() +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Volume"
  ) +
  theme_minimal()

