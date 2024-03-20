# Ice cream data source: https://fred.stlouisfed.org/series/IPN31152A
# Industrial Production: Manufacturing: Non-Durable Goods: Ice Cream and Frozen Dessert (NAICS = 31152)

# Shark attack data source: https://public.opendatasoft.com/explore/dataset/global-shark-attack/export/?flg=en-us&disjunctive.country&disjunctive.area&disjunctive.activity&dataChart=eyJxdWVyaWVzIjpbeyJjb25maWciOnsiZGF0YXNldCI6Imdsb2JhbC1zaGFyay1hdHRhY2siLCJvcHRpb25zIjp7ImZsZyI6ImVuLXVzIiwiZGlzanVuY3RpdmUuY291bnRyeSI6dHJ1ZSwiZGlzanVuY3RpdmUuYXJlYSI6dHJ1ZSwiZGlzanVuY3RpdmUuYWN0aXZpdHkiOnRydWV9fSwiY2hhcnRzIjpbeyJhbGlnbk1vbnRoIjp0cnVlLCJ0eXBlIjoibGluZSIsImZ1bmMiOiJDT1VOVCIsInNjaWVudGlmaWNEaXNwbGF5Ijp0cnVlLCJjb2xvciI6IiNGRjUxNUEifV0sInhBeGlzIjoieWVhciIsIm1heHBvaW50cyI6IiIsInRpbWVzY2FsZSI6InllYXIiLCJzb3J0IjoiIn1dLCJkaXNwbGF5TGVnZW5kIjp0cnVlLCJhbGlnbk1vbnRoIjp0cnVlfQ%3D%3D
# global-shark-attack

library(tidyverse)
library(ggthemes)

ice_cream <- read.csv(file = "IPN31152A.csv")
shark_attacks <- read.csv(file = "global-shark-attack.csv", sep = ";")

ice_cream <- ice_cream %>% 
  mutate(year = year(as.Date(DATE))) %>% 
  rename(date = DATE, 
         production_index = IPN31152A) %>% 
  arrange(year) %>% 
  select(year, production_index)

yearly_attacks <- shark_attacks %>% 
  count(Year) %>% 
  rename(year = Year, 
         shark_attacks = n) %>% 
  filter(year >= 1972)

df <- full_join(ice_cream, yearly_attacks) %>% 
  pivot_longer(!year, 
               names_to = "metric", values_to = "value")

ggplot(data = df, aes(x = year, y = value)) + 
  geom_line(aes(group = metric, color = metric), linewidth = 1) + 
  scale_color_manual(
    labels = c("Ice Cream Production Index", "Shark Attacks"), 
    values = c("brown", "purple")
  ) + 
  ggtitle("Correlation of Ice Cream Production and Shark Attacks") + 
  ggthemes::theme_solarized() + 
  theme(legend.title = element_blank(), 
        axis.title = element_blank(), 
        legend.position = "bottom")

# Obviously, ice cream production does not correlate to or cause shark attacks
# Be wary of claims using little, insufficient, and/or misrepresented data!
