dir.create("data")

# Part 2: Getting Comfortable with Time-Series Data in R


# 2.2 Loading the Data

library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")

head(retail_raw)

# 2.2b.

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

# 2.4b.

head(retail_ts)

has_gaps(retail_ts)

# 2.5 Canadian Gas Production

head(canadian_gas)

key_vars(canadian_gas)
index_var(canadian_gas)

# 2.5b.

can_gas <- canadian_gas |>
  select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

# 2.6 Working with Dates in a tsibble

can_gas_recent <- can_gas |>
  filter(Month >= yearmonth("1990 Jan"))

head(can_gas_recent)

# 2.6b.

can_gas_recent <- can_gas_recent |>
  mutate(year = year(Month),
         month = month(Month, label = TRUE),
         quarter = quarter(Month))

can_gas_recent |>
  as_tibble() |>
  group_by(month) |>
  summarize(avg_volume = mean(Volume))

# The month with the highest average production is December, and the month with the lowest is June.

# 2.6c.

can_gas_recent |>
  mutate(next_month = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

view(can_gas_recent)

# 2.6d.

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

# 2.7b.

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

# 2.7c.

can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
      x = NULL, y = "Billions of Cubic Meters", color = NULL
  ) +
  theme_minimal()

# 2.7d.

can_gas |>
  autoplot(Volume) +
  geom_smooth() +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Billions of Cubic Meters", color = NULL
  ) +
  theme_minimal()

# I chose to try layering geom_smooth() on top of the original plot because I've never used it before, and I thought it'd be interesting to examine the smooth trend line through the data.

  
# Part 3: Data Transformations (Logs and Growth Rates)
  
# 3.1 Log Transformations

# 3.1a. 
  
# The seasonal swings in the plot of can_gas do appear to be increasing over time, but are largest when the growth of the series is very slow, specifically in the 1980s. When the growth speeds up again in the 1990s, the size of the swings decreases.

# 3.1b. 
  
can_gas$log_volume <- log(can_gas$Volume)

can_gas |>
  autoplot(log_volume) +
  geom_smooth() +
labs(
  title = "Log Canadian Gas Production",
  x = NULL, y = "Billions of Cubic Meters", color = NULL
) +
  theme_minimal()
  

# The new plot shows a reshaped trend which is much less consistent over time. Rather than a linear-like growth, the log production shoots up until about 1975, then grows at a much flatter rate through the rest of the graph. The size of the seasonal swings is also much less dramatic, particularly in the 1980's, which showed great seasonality in the original plot.

# While the seasonal swings of the series do increase over time, I don't actually think the increase is consistent enough to justify logging in this way. We can still see swings of various size in the logged graph, which improved the series somewhat, but not as much as I would expect of a series that is ideal for a log transformation.


# 3.1c.

retail_RSAFSNA <- retail_ts |>
  filter(series_id == "RSAFSNA")

retail_RSAFSNA$log_sales <- log(retail_RSAFSNA$sales)

retail_RSAFSNA |>
  autoplot(log_sales) +
  geom_smooth() +
  labs(
    title = "Log U.S. Retail and Food Services Sales",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

retail_RSAFSNA |>
  autoplot(sales) +
  geom_smooth() +
  labs(
    title = "Log U.S. Retail and Food Services Sales",
    x = NULL, y = "Millions of dollars", color = NULL
  ) +
  theme_minimal()

# There is a stronger case for doing a log transformation on this dataframe, as the logged plot of RSAFSNA shows a much more linear and constant absolute change than the logged gas production does.


# 3.2 Growth Rates

# 3.2a.

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume) / lag(Volume),
    growth_logdiff = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  geom_smooth() +
  labs(
    title = "Growth Rate of Canadian Gas Production",
    x = NULL, y = "Percentage", color = NULL
  ) +
  theme_minimal()

can_gas$growth_gap <- abs(can_gas$growth_direct - can_gas$growth_logdiff)

can_gas |>
  autoplot(growth_gap) +
  geom_smooth() +
  labs(
    title = "Growth Gap of Canadian Gas Production",
    x = NULL, y = "Percentage", color = NULL
  ) +
  theme_minimal()

# After plotting the gap between direct growth and log growth, we can see that the gap is significant throughout the entire series, suggesting the logged growth rate is a very good approximation of the series.


# 3.3 SA vs NSA Growth Rates

retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

# We can see that the month over month growth of the seasonally adjusted series is much more stable and less noisy than that of the non-adjusted series. We saw a very similar effect to the plot from 2.7b, which showed a vast difference in swings between the two series.







