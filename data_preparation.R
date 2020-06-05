library(dplyr)
library(lubridate)
library(ggplot2)

# import data ----
dataset <- read.csv("WideWorldImporters_SalesPerEmployee.csv")
substr(dataset$Date, start = 1, stop = 2) <- "01"
dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y")
dataset$Month <- months(dataset$Date, abbreviate = TRUE)
dataset$SalesPerson <- as.factor(dataset$SalesPerson)

# separate data per metric ----
sales <- dataset %>% 
  select(Date, Month, SalesPerson, Sales) %>% 
  group_by(Date, Month, SalesPerson) %>% 
  summarise_all(sum)

total_excluding_tax <- dataset %>% 
  select(Date, Month, SalesPerson, Total.Excluding.Tax) %>% 
  group_by(Date, Month, SalesPerson) %>% 
  summarise_all(sum)

total_including_tax <- dataset %>% 
  select(Date, Month, SalesPerson, Total.Including.Tax) %>% 
  group_by(Date, Month, SalesPerson) %>% 
  summarise_all(sum)

profit <- dataset %>% 
  select(Date, Month, SalesPerson, Profit) %>% 
  group_by(Date, Month, SalesPerson) %>% 
  summarise_all(sum)

# get historic seasonality ----
seasonality_sales <- sales %>% 
  mutate(Month = months(Date, abbreviate = TRUE)) %>% 
  select(Month, Sales) %>% 
  group_by(Month) %>% 
  summarise_all(mean) %>% 
  mutate(Proportion = Sales/(sum(Sales)/12)) %>% 
  select(Month, Proportion)

seasonality_total_excluding_tax <- total_excluding_tax %>% 
  mutate(Month = months(Date, abbreviate = TRUE)) %>% 
  select(Month, Total.Excluding.Tax) %>% 
  group_by(Month) %>% 
  summarise_all(mean) %>% 
  mutate(Proportion = Total.Excluding.Tax/(sum(Total.Excluding.Tax)/12)) %>% 
  select(Month, Proportion)

seasonality_total_including_tax <- total_including_tax %>% 
  mutate(Month = months(Date, abbreviate = TRUE)) %>% 
  select(Month, Total.Including.Tax) %>% 
  group_by(Month) %>% 
  summarise_all(mean) %>% 
  mutate(Proportion = Total.Including.Tax/(sum(Total.Including.Tax)/12)) %>% 
  select(Month, Proportion)

seasonality_profit <- profit %>% 
  mutate(Month = months(Date, abbreviate = TRUE)) %>% 
  select(Month, Profit) %>% 
  group_by(Month) %>% 
  summarise_all(mean) %>% 
  mutate(Proportion = Profit/(sum(Profit)/12)) %>% 
  select(Month, Proportion)

# get employee averages ----
average_sales <- sales %>%
  select(SalesPerson, Sales) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(mean) %>% 
  mutate(Mean = Sales) %>% 
  select(SalesPerson, Mean)

average_total_excluding_tax <- total_excluding_tax %>%
  select(SalesPerson, Total.Excluding.Tax) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(mean) %>% 
  mutate(Mean = Total.Excluding.Tax) %>% 
  select(SalesPerson, Mean)

average_total_including_tax <- total_including_tax %>%
  select(SalesPerson, Total.Including.Tax) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(mean) %>% 
  mutate(Mean = Total.Including.Tax) %>% 
  select(SalesPerson, Mean)

average_profit <- profit %>%
  select(SalesPerson, Profit) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(mean) %>% 
  mutate(Mean = Profit) %>% 
  select(SalesPerson, Mean)

# get standard deviations for last 12 months ----
standard_deviation_sales <- sales %>% 
  filter(Date >= max(dataset$Date) %m-% years(1)) %>% 
  select(SalesPerson, Sales) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(sd) %>% 
  mutate(SD = Sales) %>% 
  select(SalesPerson, SD)

standard_deviation_total_excluding_tax <- total_excluding_tax %>% 
  filter(Date >= max(dataset$Date) %m-% years(1)) %>% 
  select(SalesPerson, Total.Excluding.Tax) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(sd) %>% 
  mutate(SD = Total.Excluding.Tax) %>% 
  select(SalesPerson, SD)

standard_deviation_total_including_tax <- total_including_tax %>% 
  filter(Date >= max(dataset$Date) %m-% years(1)) %>% 
  select(SalesPerson, Total.Including.Tax) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(sd) %>% 
  mutate(SD = Total.Including.Tax) %>% 
  select(SalesPerson, SD)

standard_deviation_profit <- profit %>% 
  filter(Date >= max(dataset$Date) %m-% years(1)) %>% 
  select(SalesPerson, Profit) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(sd) %>% 
  mutate(SD = Profit) %>% 
  select(SalesPerson, SD)

# join to data ----
# seasonality
sales <- left_join(sales, seasonality_sales, by = "Month")
total_excluding_tax <- left_join(total_excluding_tax, seasonality_total_excluding_tax, by = "Month")
total_including_tax <- left_join(total_including_tax, seasonality_total_including_tax, by = "Month")
profit <- left_join(profit, seasonality_profit, by = "Month")

# average
sales <- left_join(sales, average_sales, by = "SalesPerson")
total_excluding_tax <- left_join(total_excluding_tax, average_total_excluding_tax, by = "SalesPerson")
total_including_tax <- left_join(total_including_tax, average_total_including_tax, by = "SalesPerson")
profit <- left_join(profit, average_profit, by = "SalesPerson")

# standard deviation
sales <- left_join(sales, standard_deviation_sales, by = "SalesPerson")
total_excluding_tax <- left_join(total_excluding_tax, standard_deviation_total_excluding_tax, by = "SalesPerson")
total_including_tax <- left_join(total_including_tax, standard_deviation_total_including_tax, by = "SalesPerson")
profit <- left_join(profit, standard_deviation_profit, by = "SalesPerson")

# put into list ----
metric_list <- list(sales, total_excluding_tax, total_including_tax, profit)

# seasonality adjusted averages ----
metric_list <- lapply(metric_list, function(x) cbind(x, SeasonalityAdjustedMean = x$Mean * x$Proportion))

# generate control limits ----
metric_list <- lapply(metric_list, function(x) cbind(x, SD3Above = (x$SeasonalityAdjustedMean + (3 * x$SD))))
metric_list <- lapply(metric_list, function(x) cbind(x, SD2Above = (x$SeasonalityAdjustedMean + (2 * x$SD))))
metric_list <- lapply(metric_list, function(x) cbind(x, SD1Above = (x$SeasonalityAdjustedMean + (1 * x$SD))))
metric_list <- lapply(metric_list, function(x) cbind(x, SD1Below = (x$SeasonalityAdjustedMean - (1 * x$SD))))
metric_list <- lapply(metric_list, function(x) cbind(x, SD2Below = (x$SeasonalityAdjustedMean - (2 * x$SD))))
metric_list <- lapply(metric_list, function(x) cbind(x, SD3Below = (x$SeasonalityAdjustedMean - (3 * x$SD))))

plotdata <- data.frame(metric_list[[4]])
plotdata %>% 
  filter(SalesPerson == "Kayla Woodcock") %>%
  ggplot(aes(Date, Profit)) +
  geom_ribbon(aes(ymin = SD2Below, ymax = SD2Above), alpha = .1) +
  geom_line() +
  theme_minimal()
