read_delim("https://raw.githubusercontent.com/hiscod/hiscod-project/main/data_csv/db_hiscod_csv_v1_en.csv", delim = ';') %>%
  filter(country_name == "England" & author != "Matteo Tiratelli") -> data 

data$Weekday <- factor(data$day_week, 
                       levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                   'Saturday', 'Sunday'))
data %>%
  ggplot(aes(x = year)) + geom_density()

data %>%
  filter(author == "John Bohstedt") %>%
  drop_na(Weekday) %>%
  filter(year > 1730) %>%
  mutate(Period = cut(year, breaks = 3, dig.lab=10)) %>% 
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)

data %>%
  filter(author == "Family and Community Historical Research Society") %>% 
  filter(year > 1700) %>%
  drop_na(Weekday) %>%
  count(Weekday, .drop = FALSE) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)
