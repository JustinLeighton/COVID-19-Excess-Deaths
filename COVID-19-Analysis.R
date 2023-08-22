
setwd("C:/Users/Justin Leighton/Desktop/Development/COVID-19")
library(tidyverse)
library(forecast)
library(lubridate)
library(tidyverse)
library(scales)
knitr::opts_chunk$set(echo = TRUE)
theme_set(theme_minimal())
options(dplyr.summarise.inform = FALSE)

# https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/3yf8-kanr
df1 <- read.csv("./Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2014-2018.csv") %>%
  filter(Jurisdiction.of.Occurrence != "United States") %>%
  select("Jurisdiction.of.Occurrence", "Week.Ending.Date", "MMWR.Year", "MMWR.Week", "All..Cause") %>%
  `colnames<-`(c("State", "Date", "Year", "Week", "Deaths")) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y"))

# https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6
df2 <- read.csv("./Weekly_Counts_of_Deaths_by_State_and_Select_Causes__2019-2020.csv") %>%
  filter(Jurisdiction.of.Occurrence != "United States") %>%
  select("Jurisdiction.of.Occurrence", "Week.Ending.Date", "MMWR.Year", "MMWR.Week", "All.Cause") %>%
  `colnames<-`(c("State", "Date", "Year", "Week", "Deaths")) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# https://data.cdc.gov/NCHS/Weekly-Counts-of-Deaths-by-State-and-Select-Causes/muzy-jte6
df3 <- read.csv("./Weekly_Provisional_Counts_of_Deaths_by_State_and_Select_Causes__2020-2023.csv") %>%
  filter(Jurisdiction.of.Occurrence != "United States") %>%
  select("Jurisdiction.of.Occurrence", "Week.Ending.Date", "MMWR.Year", "MMWR.Week", "All.Cause") %>%
  `colnames<-`(c("State", "Date", "Year", "Week", "Deaths")) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d"))

# Merge and aggregate
df <- df2 %>%
  anti_join(df3, by=c("Year", "Week")) %>%
  rbind(df3) %>%
  rbind(df1) %>%
  arrange(Date) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  group_by(State, Date, Year, Week) %>%
  summarise(Deaths = sum(Deaths)) %>%
  as.data.frame() %>%
  filter(!is.na(Deaths) & Date > "2014-01-04") %>%
  filter(Date < "2023-01-01")

df %>% head()

## Analysis
df %>%
  group_by(Date) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ggplot(aes(x = Date, y = Deaths)) +
  geom_line() +
  theme(plot.caption=element_text(color='grey')) +
  labs(title = 'Total weekly US deaths over time',
       caption = 'Analysis by Justin Leighton')

df %>%
  group_by(Week, Year) %>%
  summarise(Deaths = sum(Deaths)) %>%
  ggplot(aes(x = Week, y = Deaths, group = Year, col = Year)) +
  geom_line() +
  theme(plot.caption=element_text(color='grey')) +
  labs(title = 'Total weekly US deaths over time, year over year',
       caption = 'Analysis by Justin Leighton')

# Plot deaths by year
df %>%
  group_by(Year) %>%
  summarise(Deaths = sum(Deaths)) %>%
  mutate(label = paste0(round(Deaths / 1e6,1),'M')) %>%
  ggplot(aes(x = Year, y = Deaths)) +
  geom_col(fill = "dodgerblue", col = "black") + 
  geom_text(aes(label=label), col='white', fontface='bold', position=position_stack(vjust=0.95)) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.caption=element_text(color='grey')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  labs(title = 'Total annual US deaths by year',
       caption = 'Analysis by Justin Leighton')

# Auto Arima
model_ts <- df %>%
  filter(Year < 2020) %>%
  group_by(Date) %>%
  summarise(Deaths = sum(Deaths)) %>%
  arrange(Date) %>%
  select(Deaths) %>%
  ts(start = decimal_date(ymd(min(df$Date))), frequency = 367.25/7) %>%
  auto.arima(trace=TRUE, test = "kpss", approximation = FALSE)

## Plot fitted and extrapolate excess deaths

# Predict Excess Deaths
pred <- forecast(model_ts, length(unique(filter(df, Year >= 2020)$Date))) %>%
  as.data.frame() %>% .[,1] %>% `length<-`(length(unique(df$Date)))
df_fit <- df %>%
  group_by(Date, Year, Week) %>%
  summarise(Deaths = sum(Deaths)) %>%
  as.data.frame() %>%
  mutate(flag = Year >= 2020) %>%
  arrange(-flag, Year, Week) %>%
  cbind(Pred = pred) %>%
  select(!flag) %>%
  `colnames<-`(c("Date", "Year", "Week", "Deaths", "Pred")) %>%
  mutate(Excess = as.integer(Deaths - Pred),
         Excess = ifelse(Excess < 0, 0, Excess))

# Quantify total excess deaths
TotalExcess <- paste0(round(sum(df_fit$Excess, na.rm=TRUE)/1e6,1),'M')

# Plot deaths over time with predicted values
df_fit %>%
  ggplot(aes(x = Date, y = Deaths, col="Deaths")) +
  geom_line(na.rm=TRUE) +
  geom_line(aes(y = Pred, col="Predicted"), na.rm=TRUE) +
  geom_line(aes(y = (Deaths - Pred) * (Deaths > Pred), col="Excess"), na.rm=TRUE) +
  scale_color_manual(name = "Color", values = c("Deaths" = "black", 
                                                "Predicted" = "firebrick1",
                                                "Excess" = "darkorchid4")) +
  annotate("text", label=TotalExcess, x=as.Date('2022-01-01'), y=27500) +
  theme(plot.caption=element_text(color='grey')) +
  labs(title="Deaths vs. Predicted",
       caption = "Analysis by Justin Leighton") +
  geom_blank()

# Import VAX data - https://covid.cdc.gov/covid-data-tracker/#vaccination-trends
df_vax <- read.csv("./trends_in_number_of_covid19_vaccinations_in_the_us.csv") %>% 
  `colnames<-`(c("Date", "Year", "Week", "Dose1", "Dose2", "Booster")) %>%
  mutate(Date = as.Date(Date, format="%m/%d/%Y")) %>%
  group_by(Year, Week) %>%
  mutate(Date = max(Date)) %>%
  group_by(Date, Week, Year) %>%
  summarise(Dose1 = sum(Dose1),
            Dose2 = sum(Dose2),
            Booster = sum(Booster)) %>%
  ungroup() %>% arrange(Date) %>% as.data.frame() %>%
  mutate(Dose1_cs = cumsum(Dose1),
         Dose2_cs = cumsum(Dose2),
         Booster_cs = cumsum(Booster))

# Plot Excess vs. Vaccinations
scale <- 7000
df_fit %>%
  filter(Year >= 2020) %>%
  left_join(df_vax, on=c("Week", "Year")) %>%
  ggplot(aes(x = Date)) +
  geom_line(aes(y = Excess * scale, col = "Excess Deaths"), na.rm=TRUE) + 
  geom_line(aes(y = Dose1_cs, col="Dose 1"), na.rm=TRUE) + 
  geom_line(aes(y = Dose2_cs, col="Dose 2"), na.rm=TRUE) +
  geom_line(aes(y = Booster_cs, col="Booster"), na.rm=TRUE) +
  scale_y_continuous(sec.axis = sec_axis(~./scale, name="Excess Deaths")) +
  scale_color_manual(name = "Color", values = c("Excess Deaths" = "dodgerblue", 
                                                "Dose 1" = "deeppink",
                                                "Dose 2" = "darkorchid",
                                                "Booster" = "firebrick")) +
  labs(title="Vaccinated vs. Excess Deaths",
       y = "Number Vaccinated",
       caption = "Justin Leighton 2023-04-15") +
  geom_blank()









