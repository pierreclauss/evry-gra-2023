library(tidyverse)
library(tidyquant)


# import data
(cac <- tq_get(
  "^FCHI",
  from = '1990-03-01',
  to = "2023-10-01",
  get = "stock.prices"
))
tail(cac)


# write.table(
#   na.omit(cac),
#   file = "cac.csv",
#   sep = ";",
#   dec = ",",
#   row.names = F
# )
# 

# plot
cac %>%
  ggplot(aes(x = date, y = adjusted)) +
  geom_line() +
  ggtitle("CAC 40 since 1990") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  labs(x = "Date", y = "Adjusted Price") +
  theme_bw()

# transmute
cac_monthly_returns <- cac %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "monthly",
    col_rename = "benchmark.returns"
  )
tail(cac_monthly_returns)

