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

# States

df_state <- read.csv('https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv')

call <- c('New York', 'Ohio', 'New Jersey', 'California')

df_sp$Province_State <- as.character(df_sp$Province_State)

df_sp <- df_state %>%
  filter(iso2 == 'US') %>%
  gather(key = time_period, value = number_of_cases, 12 : length(df_state)) %>%
  mutate(time_period = str_remove(time_period, "X")) %>%
  mutate(date = time_period %>% mdy()) %>%
  mutate(NY = ifelse(Province_State == 'New York','New York','Other'),
         call_out = ifelse(Province_State %in% call, as.character(Province_State), 'Other')) %>%
  select(Province_State, call_out, date, number_of_cases) %>%
  group_by(Province_State, call_out, date) %>%
  summarise(Cases = sum(number_of_cases)) %>%
  filter(date >= '2020-03-10')

ggplot(df_sp[df_sp$call_out == 'Other',], aes(x = date, y = Cases, group = Province_State)) +
  geom_line(col = 'gray') +
  geom_line(data = df_sp[df_sp$call_out != 'Other',], aes(col = Province_State), size = 2) +
  geom_label(data = df_sp[df_sp$call_out != 'Other' & df_sp$date == '2020-04-02',], aes(label = scales::comma(Cases)), size = 3) +
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_blank(), legend.position = 'top') +
  labs(title = 'Growth in Confirmed COVID-19 Cases by State', col = 'State', caption = 'Data from Johns Hopkins Center for Systems Science and Engineering')