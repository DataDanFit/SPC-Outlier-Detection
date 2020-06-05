library(dplyr)
library(lubridate)
dataset <- read.csv("WideWorldImporters_SalesPerEmployee.csv")
dataset$Date <- as.Date(dataset$Date, format = "%d/%m/%Y")
dataset$SalesPerson <- as.factor(dataset$SalesPerson)

seasonality <- dataset %>% 
  mutate(Month = factor(months(Date, abbreviate = TRUE), levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))) %>% 
  select(Month, Sales, Total.Excluding.Tax, Total.Including.Tax, Profit) %>% 
  group_by(Month) %>% 
  summarise_all(mean) %>% 
  mutate(Sales.Proportion = Sales/(sum(Sales)/12),
         Total.Excluding.Tax.Proportion = Total.Excluding.Tax/(sum(Total.Excluding.Tax)/12),
         Total.Including.Tax.Proportion = Total.Including.Tax/(sum(Total.Including.Tax)/12),
         Profit.Proportion = Profit/(sum(Profit)/12)) %>% 
  select(Month, Sales.Proportion, Total.Excluding.Tax.Proportion, Total.Including.Tax.Proportion, Profit.Proportion)

averages <- dataset %>%
  select(SalesPerson, Sales, Total.Excluding.Tax, Total.Including.Tax, Profit) %>% 
  group_by(SalesPerson) %>% 
  summarise_all(mean) %>% 
  mutate(Mean.Sales = Sales, 
         Mean.Total.Excluding.Tax = Total.Excluding.Tax, 
         Mean.Total.Including.Tax = Total.Including.Tax, 
         Mean.Profit = Profit) %>% 
  select(SalesPerson, Mean.Sales, Mean.Total.Excluding.Tax, Mean.Total.Including.Tax, Mean.Profit)
  

