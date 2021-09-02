library(tidyverse)
library(extrafont)
library(ggtext)
library(lubridate)
loadfonts(dev = "win")

load("ttTweets2021.rda")
load("ttTweets2020.rda")
load("ttTweets2019.rda")
load("ttTweets2018.rda")

tttweets_all = ttTweets2021 %>%
  bind_rows(ttTweets2020) %>%
  bind_rows(ttTweets2019) %>%
  bind_rows(ttTweets2018) %>%
  mutate(date = lubridate::date(TweetDate),
         alt = case_when(is.na(AltText) ~ "without",
                         AltText=="Image" ~ "without",
                         AltText=="ScrapingError" ~ "without",
                         TRUE ~ "with")) %>%
  group_by(date, alt) %>%
  summarise(count = dplyr::n()) %>%
  ungroup() %>%
  arrange(date) %>%
  mutate(week = lag(lubridate::epiweek(date), 1),
         week = case_when(is.na(week) ~ 14,
                          TRUE ~ week),
         year = lubridate::year(date)) %>%
  filter(date != lubridate::date("2021-04-04")) %>%
  pivot_wider(names_from = alt, values_from = count,
              values_fill = 0) %>%
  mutate(year = case_when(week == 53 & year == 2021 ~ 2020,
                          TRUE ~ year)) %>%
  group_by(week, year) %>%
  mutate(with = sum(with), without = sum(without)) 

data_week = tttweets_all %>%
  select(with, without, week, year) %>%
  distinct()

plot_data = tibble(date = seq(date("2018-04-02"), date("2021-04-03"), "days")) %>%
  arrange(date) %>%
  mutate(week = lag(lubridate::epiweek(date), 1),
         week = case_when(is.na(week) ~ 14,
                          TRUE ~ week),
         year = lubridate::year(date),
         year = case_when(week == 53 & year == 2021 ~ 2020,
                          TRUE ~ year)) %>%
  filter(lubridate::year(date) >= 2020) %>%
  left_join(data_week, by = c("week" = "week", 
                              "year" = "year")) %>%
  mutate(monday = (lubridate::wday(date)==2),
         with_monday = lag(with, 1),
         without_monday = lag(without, 1))

monday_data = plot_data %>%
  filter(monday) %>%
  mutate(with = with_monday,
         without = without_monday,
         week = week - 1) %>%
  select(date, week, year, with, without)

plot_data = plot_data %>%
  select(date, week, year, with, without) %>%
  bind_rows(monday_data) %>%
  arrange(date, week)

plot_data %>%
  filter(lubridate::year(date) >= 2020) %>%
  ggplot() +
  geom_line(aes(date, without), 
            color = "#F279A6",
            size = 1) + 
  geom_line(mapping = aes(date, with), 
            color = "#038C3E",
            size = 1) +
  theme_minimal() +
  scale_x_date(expand = c(0, 0),
               breaks = c(lubridate::date("2020-01-01"), 
                          lubridate::date("2021-01-01")),
               labels = c("2020", "2021")) +
  scale_y_continuous(breaks = c(0, 50, 100), 
                     limits = c(0, 120),
                     expand = c(0, 0)) +
  labs(x = "", y = "# DATA VIZ", 
       title = "DATA VIZ <span style='color: #038C3E;font-weight:bold;'>WITH</span> AND <span style='color: #F279A6;font-weight:bold;'>WITHOUT</span> ALT-TEXT") +
  theme(panel.grid =  element_blank(),
        axis.line.x = element_line(),
        axis.line.y = element_line(),
        axis.text.x = element_text(hjust = 0),
        axis.text.y = element_text(vjust = 0), 
        text = element_text(family = "Lato", face = "bold", size = 14),
        plot.title = element_markdown(face = "bold", size = 19),
        plot.title.position = "plot")
ggsave("alfombra.png", dpi = 300, height = 4.5, width = 7)
