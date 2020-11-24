require(tidyverse)
require(rvest)
require(cowplot)

## Tiratelli riots data

riots <- read_csv('https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Riots.csv')
riots_nonind <- riots[riots$Industrial=='No',]
riots_nonind %>% drop_na(Weekday) -> riots_nonind

riots_nonind$Weekday <- factor(riots_nonind$Weekday, 
                               levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                           'Saturday', 'Sunday'))
riots_nonind$year <- as.numeric(riots_nonind$year)

riots_nonind %>%
  mutate(Period = cut(year, breaks = 4, labels = c('1800-1834','1834-1868',
                                                    '1868-1902','1902-1936'))) -> riots_nonind

riots_nonind %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) -> Totals_p

Totals_p %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_p

p1 <- ggplot(Totals_p, aes(x=Weekday, y=Period, fill=Proportion)) + geom_tile(color="black") + 
  theme_bw() + 
  coord_equal() + 
  scale_fill_distiller(palette="Greys", direction=1) +
  xlab(NULL) + ylab(NULL) + labs(title = "Figure 1: Riots from 1800 to 1939 (n = 311)", 
                                 caption = "Source: Tiratelli (2019)") +
  scale_x_discrete(position = "top", labels = c('M','T','W','T','F','S','S')) +
  theme(axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

save_plot("Tiratelli.pdf", p1, base_width = 6.8, base_height = 4, units = "in")


## Navickas data

read_html("http://historyofpublicspace.uk/political-meetings-mapper-2/") %>% 
  html_nodes(css = "table.tablepress") -> Navickas
html_table(Navickas[[1]]) -> Navickas
as_tibble(Navickas) -> Navickas
Navickas$year <- as.numeric(str_sub(Navickas$date, start= -4))
Navickas[is.na(Navickas$year),] %>%
  mutate(year = as.numeric(str_sub(Navickas[is.na(Navickas$year),]$date, 1, 4))) -> Navickas[is.na(Navickas$year),]
Navickas$day <- na_if(Navickas$day,'')
Navickas <- Navickas[Navickas$year > 1789,]
Navickas %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1790 - 1803","1804 - 1818",
                                                   "1819 - 1833","1834 - 1848"))) -> Navickas

Navickas %>% drop_na(day) -> Navickas

Navickas$day_1 <- factor(Navickas$day, 
                               levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                           'Saturday', 'Sunday'))
Navickas %>%
  group_by(Period) %>%
  count(day_1, .drop = FALSE) -> Totals_p

Totals_p %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_p

p2 <- ggplot(Totals_p, aes(x=day_1, y=Period, fill=Proportion)) + geom_tile(color="black") + 
  theme_bw() + 
  coord_equal() + 
  scale_fill_distiller(palette="Greys", direction=1) +
  xlab(NULL) + ylab(NULL) + labs(title = "Figure 2: Political meetings from 1790 to 1848 (n = 1,452)", 
                                 caption = "Source: Navickas (2020)")  +
  scale_x_discrete(position = "top", labels = c('M','T','W','T','F','S','S')) +
  theme(axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

save_plot("Navickas.pdf", p2, base_width = 6.8, base_height = 4, units = "in")

## Tilly data

Tilly <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Tilly_data.csv")
Tilly$day <- na_if(Tilly$WDAY,'')
Tilly$year <- as.numeric(paste('1',as.character(Tilly$CGIDYEAR),sep=""))
Tilly %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1758 - 1776","1777 - 1795",
                                                   "1796 - 1814","1815 - 1834"))) -> Tilly

Tilly <- Tilly[Tilly$ADAY=="EXACT" & Tilly$TYPE!='STRIKES, TURNOUTS',]
Tilly %>% drop_na(day) -> Tilly

Tilly$day <- factor(Tilly$day, 
                        levels = c ('MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 
                                    'SATURDAY', 'SUNDAY'))
Tilly %>%
  group_by(Period) %>%
  count(day, .drop = FALSE) -> Totals_p

Totals_p %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_p

p3 <- ggplot(Totals_p, aes(x=day, y=Period, fill=Proportion)) + geom_tile(color="black") + 
  theme_bw() + 
  coord_equal() + 
  scale_fill_distiller(palette="Greys", direction=1) +
  xlab(NULL) + ylab(NULL) + labs(title = "Figure 3: Contentious gatherings from 1758 to 1834 (n = 5,495)",
                                 caption = "Source: Tilly and Horn (1988)")  +
  scale_x_discrete(position = "top", labels = c('M','T','W','T','F','S','S')) +
  theme(axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

save_plot("Tilly.pdf", p3, base_width = 6.8, base_height = 4, units = "in")

## Combining

Tiratelli <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Riots.csv")
Tiratelli <- Tiratelli[Tiratelli$Industrial=='No',]
Tiratelli %>% drop_na(Weekday) -> Tiratelli
Tiratelli$year <- as.numeric(Tiratelli$year)
Tiratelli$Weekday <- as.character(Tiratelli$Weekday)
Tiratelli <- rename(Tiratelli, day = Weekday)

read_html("http://historyofpublicspace.uk/political-meetings-mapper-2/") %>% 
  html_nodes(css = "table.tablepress") -> Navickas
html_table(Navickas[[1]]) -> Navickas
as_tibble(Navickas) -> Navickas
Navickas$year <- as.numeric(str_sub(Navickas$date, start= -4))
Navickas[is.na(Navickas$year),] %>%
  mutate(year = as.numeric(str_sub(Navickas[is.na(Navickas$year),]$date, 1, 4))) -> Navickas[is.na(Navickas$year),]
Navickas$day <- na_if(Navickas$day,'')
Navickas %>% drop_na(day) -> Navickas
Navickas$year <- as.numeric(Navickas$year)
Navickas$day <- as.character(Navickas$day)


Tilly <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Tilly_data.csv")
Tilly$day <- na_if(Tilly$WDAY,'')
Tilly$year <- as.numeric(paste('1',as.character(Tilly$CGIDYEAR),sep=""))
Tilly <- Tilly[Tilly$ADAY=="EXACT" & Tilly$TYPE!='STRIKES, TURNOUTS',]
Tilly %>% drop_na(day) -> Tilly
Tilly$day <- as.character(Tilly$day)

Tiratelli %>%
  select(year, day) -> Tiratelli

Navickas %>%
  select(year, day) -> Navickas

Tilly %>%
  select(year, day) -> Tilly

Data <- bind_rows(Tiratelli, Tilly, Navickas)
Data$day <- as.factor(Data$day)
Data$day <- recode_factor(Data$day, 'MONDAY' = "Monday", 'TUESDAY' = "Tuesday", 'WEDNESDAY' = "Wednesday",
                          'THURSDAY' = "Thursday", 'FRIDAY' = "Friday", 'SATURDAY' = "Saturday", 'SUNDAY' = "Sunday")
Data %>%
  mutate(Period = cut(year, breaks = 8, labels = c("1758 - 1779","1780 - 1801","1802 - 1824",
                                                   "1825 - 1846","1847 - 1868","1869 - 1891",
                                                   "1892 - 1913","1914 - 1936"))) -> Data

Data %>%
  group_by(Period) %>%
  count(day, .drop = FALSE) -> Totals_p

Totals_p %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_p

p4 <- ggplot(Totals_p, aes(x=day, y=Period, fill=Proportion)) + geom_tile(color="black") + 
  theme_bw() + 
  coord_equal() + 
  scale_fill_distiller(palette="Greys", direction=1) +
  xlab(NULL) + ylab(NULL) + labs(title = "Figure 4: Political events from 1758 to 1936 (n = 7,233)",
                                 caption = "Sources: Tilly and Horn (1988), Tiratelli (2019) and Navickas (2020)")  +
  scale_x_discrete(position = "top", labels = c('M','T','W','T','F','S','S')) +
  theme(axis.ticks = element_blank(),
        plot.caption = element_text(hjust = 0),
        plot.caption.position =  "plot",
        plot.title.position = "plot",
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA))

save_plot("Combined.pdf", p4, base_width = 6.8, base_height = 4, units = "in")