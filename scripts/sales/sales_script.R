# Libaries----
library(tidyverse)
library(janitor)
library(wesanderson)
library(DT)
library(forecast)

# Import----

vg_sales <- read.csv("scripts/sales/vgsales.csv") %>% 
  clean_names()
vg <- vg_sales %>% 
  filter(!year %in% c("N/A", "2017", "2020")) %>% 
  select(-one_of(c('global_sales', 'rank'))) %>% 
  pivot_longer(matches('sales'), names_to = 'region', values_to = 'sales') %>% 
  mutate(region = region %>% str_remove('_sales') %>% as.factor())

vg %>% 
  group_by(year, publisher) %>% 
  summarize(revenue = sum(sales)) %>%
  top_n(1) %>% 
  ggplot(aes(x = year, y = revenue, fill = publisher)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Publisher by Revenue each Year") +
  scale_fill_manual(values = wes_palette('Darjeeling1')) +
  theme_minimal() +
  theme(legend.position = "top")

vg %>% 
  group_by(year, genre) %>% 
  summarize(revenue = sum(sales)) %>%
  top_n(1) %>% 
  ggplot(aes(x = year, y = revenue, fill = genre)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Genre by Revenue each Year") +
  scale_fill_manual(values = wes_palette('Darjeeling1', n = 8, type = 'continuous')) +
  theme_minimal() +
  theme(legend.position = "top")

vg %>% 
  group_by(year, platform) %>% 
  summarize(revenue = sum(sales)) %>%
  top_n(1) %>% 
  ggplot(aes(x = year, y = revenue, fill = platform)) + 
  geom_bar(stat = "identity") +
  ggtitle("Top Genre by Revenue each Year") +
  scale_fill_manual(values = wes_palette('Darjeeling1', n = 10, type = 'continuous')) +
  theme_minimal() +
  theme(legend.position = "top")

# Top-----
.top_publisher_n <- vg %>% count(publisher) %>% top_n(10)
vg %>% 
  filter(publisher %in% .top_publisher_n$publisher) %>% 
  count(publisher, genre) %>% 
  ggplot(aes(x = publisher, y = n, fill = genre)) +
  geom_col() + 
  coord_flip() +
  scale_fill_manual(values = wes_palette('Darjeeling1', length(unique(vg$genre)), type = 'continuous'))

vg %>% 
  filter(publisher %in% .top_publisher_n$publisher) %>% 
  count(publisher, region) %>% 
  ggplot(aes(x = publisher, y = n, fill = region)) +
  geom_col() + 
  coord_flip() +
  scale_fill_manual(values = wes_palette('Darjeeling1', length(unique(vg$genre)), type = 'continuous'))

vg %>% 
  filter(publisher %in% .top_publisher_n$publisher) %>% 
  count(publisher, region) %>% 
  ggplot(aes(x = publisher, y = n, fill = region)) +
  geom_col() + 
  coord_flip() +
  scale_fill_manual(values = wes_palette('Darjeeling1', length(unique(vg$genre)), type = 'continuous'))
