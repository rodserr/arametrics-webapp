# Libaries----
library(tidyverse)
library(janitor)
library(naniar)
library(wesanderson)
library(DT)
library(forecast)
library(caret)

# Import----

vg_sales <- read.csv("data/VG-sales/vgsales.csv") %>% 
  clean_names()
vg <- vg_sales %>% 
  filter(!year %in% c("N/A", "2017", "2020")) %>% 
  select(-one_of(c('global_sales', 'rank'))) %>% 
  pivot_longer(matches('sales'), names_to = 'region', values_to = 'sales') %>% 
  mutate(region = region %>% str_remove('_sales') %>% as.factor())

# Descriptive Plots-----

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

# Top
.top_publisher_n <- vg_sales %>% count(publisher) %>% top_n(10)
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



# Model Tryouts----
gg_miss_case(vg_sales)
gg_miss_upset(vg_sales)

.publisher_count <- vg_sales %>% count(publisher)

table(vg_sales$platform) %>% sort()
.nintendoplatforms = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
.sonyplatforms = c("PS","PS2","PSP","PS3","PS4","PSV")
.segaplatforms = c("GEN","SCD","DC","GG","SAT")
.msplatforms = c("XB","X360", "XOne")
.otherplatforms = c("2600","3DO","NG","PCFX","TG16","WS")

dataBM <- vg_sales %>% 
  left_join(.publisher_count, by = 'publisher') %>% 
  transmute(name, 
            genre_fct = genre,
            platform_fct = case_when(platform %in% .nintendoplatforms ~ 'nintendo',
                                  platform %in% .sonyplatforms ~ 'sony',
                                  platform %in% .segaplatforms ~ 'sega',
                                  platform %in% .msplatforms ~ 'ms',
                                  platform %in% .otherplatforms ~ 'other',
                                  T ~ 'pc') %>% factor(),
            publisher_fct = case_when(n < 5 ~ 'small',
                                   n < 150 ~ 'medium',
                                   T ~ 'large') %>% factor(),
            avg_sales = global_sales/(2017-as.numeric(as.character(year)))) %>% 
  na.omit()

  calc_boxplot_stat <- function(x) {
    coef <- 1.5
    n <- sum(!is.na(x))
    # calculate quantiles
    stats <- quantile(x, probs = c(0.0, 0.25, 0.5, 0.75, 1.0))
    names(stats) <- c("ymin", "lower", "middle", "upper", "ymax")
    iqr <- diff(stats[c(2, 4)])
    # set whiskers
    outliers <- x < (stats[2] - coef * iqr) | x > (stats[4] + coef * iqr)
    if (any(outliers)) {
      stats[c(1, 5)] <- range(c(stats[2:4], x[!outliers]), na.rm = TRUE)
    }
    return(stats)
  }

dataBM %>% 
  ggplot(aes(x = platform_fct, y = avg_sales, fill = genre_fct)) +
  stat_summary(fun.data = calc_boxplot_stat, geom="boxplot", position = "dodge2") +
  coord_flip() +
  facet_wrap(~publisher_fct, scales = 'free')

.trainIndex <- createDataPartition(iris$Species, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train <- dataBM[.trainIndex, ]
test <- dataBM[-.trainIndex, ]

cntrl <- trainControl(method = 'cv', number = 5)
rfModel <- train(avg_sales ~ genre_fct + platform_fct + publisher_fct, data = train,
                 method = 'rf', 
                 trControl = cntrl)
rfModel




