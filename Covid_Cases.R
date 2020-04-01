library(tidyverse)
library(lubridate)

df <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv')

c_list <- c('US','China','Italy','Spain','Korea, South')

df_plot <- df %>%
  select(2, 5:length(df)) %>%
  gather(key = time_period, value = number_of_cases, 2:(length(df)-3)) %>%
  mutate(time_period = str_remove(time_period, "X")) %>%
  mutate(date = time_period %>% mdy()) %>%
  select(Country.Region, date, number_of_cases) %>%
  group_by(Country.Region, date) %>%
  summarise(Cases = sum(number_of_cases)) %>%
  filter(Country.Region %in% c_list)

ggplot(df_plot, aes(x = date, y = Cases)) +
  geom_line(aes(col = Country.Region), size = 2) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(labels = scales::comma) +
  labs(color = 'Country', y = 'Confirmed Cases', x = 'Date',
       title = 'Global Confirmed Cases of COVID-19 by Selected Country',
       caption = 'Data from Johns Hopkins Center for Systems Science and Engineering') +
  theme(legend.position = "top", axis.title.x = element_blank())
