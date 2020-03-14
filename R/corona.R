library(tidyverse)
library(plotly)

confirmed_raw <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv") 
deaths_raw <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered_raw <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv") 

theme_set(theme_minimal())

df <- tibble(x = 1:2, y = 3:4, z = 4:5)
df %>% rowwise()

# wide to long
confirmed <- confirmed_raw %>% 
  pivot_longer(cols = matches("/[0-9]"), # columns to gather into long format
               names_to = "date",        # name of 1. new, key column 
               values_to = "conf") %>%  # name of 2  new, value column
  janitor::clean_names(., "snake") %>%
  group_by(country_region, date) %>% 
  summarise(conf = sum(conf)) %>% 
  filter(conf > 100, # only days with more than 50 cases
         max(conf) > 500) %>% # only countries with more than 500 cases at max point
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
  arrange(country_region, date) %>% 
  mutate(day = row_number(),
         conf_diff = lead(conf) - conf, # index running within group_by groups (_n) 
         log_conf = log(conf),
         conf_growth = (lead(conf) / conf) -1,
         conf_days_to_double = log(2, base = conf_growth)) 


# Lineplot: Date vs. Infected
confirmed %>% 
  ggplot(aes(x = date, y = conf, col = country_region)) + 
  geom_point() +
  geom_line() +
  scale_y_log10() +
  # scale_x_date() +
  annotation_logticks(sides = "l") 
  # scale_y_log10(labels = c("100", "1k", "10k", "100k"))
  # scale_y_log10(labels = scales::breaks_log(n = 5, base = 100)) 

# Linear models by country
library(broom)
by_country <- confirmed %>% 
  group_by(country_region)
do(by_country, glance(lm(log_conf ~ day, data = .)))

# Linear model for germany
confirmed %>% 
  filter(country_region == "Germany") %>% 
  lm(log_conf ~ date, data = .) %>% summary()


# Deaths ------------------------------------------------------------------
# wide to long
# confirmed <- 
deaths <- deaths_raw  %>% 
  pivot_longer(cols = matches("/[0-9]"), # columns to gather into long format
               names_to = "date",        # name of 1. new, key column 
               values_to = "deaths") %>%  # name of 2  new, value column
  janitor::clean_names(., "snake") %>% 
  group_by(country_region, date) %>% 
  summarise(deaths = sum(deaths)) %>% 
  # filter(cases > 100, # only days with more than 50 cases
  #        max(cases) > 500) %>% # only countries with more than 500 cases at max point
  mutate(date = as.Date(date, format = "%m/%d/%Y")) %>% 
  arrange(country_region, date) %>% 
  mutate(diff_death = lead(deaths) - deaths,
         death_log = log(deaths),
         growth_death = (lead(deaths) / deaths) -1,
         death_days_double = log(2, base = growth_death)) # index running within group_by groups (_n) 



mutate(day = row_number(),
       conf_diff = lead(conf) - conf, # index running within group_by groups (_n) 
       log_conf = log(conf),
       conf_growth = (lead(conf) / conf) -1,
       conf_days_to_double = log(2, base = conf_growth)) 

# https://twitter.com/jburnmurdoch/status/1237737352879112194/photo/1

# var "cases" to "deaths"
deaths %>% 
  mutate() # mulitplicative term between each day

# italien 
all <- confirmed %>% 
  left_join(deaths) %>% 
  filter(country_region != "Cruise Ship" & country_region != "China")






# Lineplot: Date vs. Infected
all %>% 
  ggplot(aes(x = date, y = conf, col = country_region)) + 
  geom_point() +
  geom_line() +
  scale_y_log10() +
  # scale_x_date() +
  annotation_logticks(sides = "l")
  # scale_y_log10(labels = c("100", "1k", "10k", "100k"))
# scale_y_log10(labels = scales::breaks_log(n = 5, base = 100)) 


all %>% 
  ggplot(aes(x = day, y = conf, col = country_region)) + 
  geom_point() +
  geom_line() +
  scale_y_log10() +
  # scale_x_date() +
  annotation_logticks(sides = "l")
# scale_y_log10(labels = c("100", "1k", "10k", "100k"))
# scale_y_log10(labels = scales::breaks_log(n = 5, base = 100)) 


# Percentage growth by day
growth <- all %>% 
  ggplot(aes(x = day, y = conf_growth, col = country_region)) +
  geom_point(size = 2) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent_format()) +
  geom_smooth()
growth

plotly::ggplotly(growth)
  
# Days to double by day
double <- all %>% 
  ggplot(aes(x = day, y = conf_days_to_double, col = country_region)) +
  geom_point(size = 2) +
  geom_line(size = 1.3)
double


growth_death <- all %>% 
  ggplot(aes(x = day, y = growth_death, col = country_region)) +
  geom_point(size = 2) +
  geom_line(size = 1.3) +
  scale_y_continuous(labels = scales::percent_format())
growth_death

gggplotly(growth_death)
