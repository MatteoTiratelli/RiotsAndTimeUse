require(tidyverse)
require(rvest)
require(cowplot)
require(readxl)

graph_heatmap <- function(DF, Title, Caption) {
  ggplot(DF, aes(x=Weekday, y=Period, fill=Proportion)) + geom_tile(color="black") + 
    theme_bw() + 
    coord_equal() + 
    scale_fill_distiller(palette="Greys", direction=1, limits = c(0,0.4)) +
    xlab(NULL) + ylab(NULL) + labs(title = Title, caption = Caption) +
    scale_x_discrete(position = "top", labels = c('M','T','W','T','F','S','S')) +
    theme(axis.ticks = element_blank(),
          plot.caption = element_text(hjust = 0),
          plot.caption.position =  "plot",
          plot.title.position = "plot",
          panel.background = element_rect(fill = "transparent", colour = NA),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background= element_rect(fill = "transparent", colour = NA),
          text=element_text(family="serif"))
}

EventWorkingMonday_reg <- function(DF, MinYear) {
  DF %>%
    drop_na(Weekday, `Working hours (7-7)`) %>%
    mutate(Outcome = ifelse(`Working hours (7-7)` == "Y" & Weekday == "Monday",1,0),
           Treatment = year - MinYear) %>%
    glm(Outcome ~ Treatment, data=., family = binomial(link = "logit")) %>%
    summary()
}

EventWorkingHours_reg <- function(DF, MinYear) {
  DF %>%
    drop_na(Weekday, `Working hours (7-7)`) %>%
    mutate(Outcome = ifelse(`Working hours (7-7)` == "Y" & Weekday %in% c("Tuesday", "Wednesday","Thursday","Friday"),1,0),
           Treatment = year - MinYear) %>%
    glm(Outcome ~ Treatment, data=., family = binomial(link = "logit")) %>%
    summary()
}

########################################

# Navickas political meetings data: Table 2 and 3

Navickas <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Data_Navickas.csv")
Navickas$year <- as.numeric(str_sub(Navickas$date, start= -4))
Navickas[is.na(Navickas$year),] %>%
  mutate(year = as.numeric(str_sub(Navickas[is.na(Navickas$year),]$date, 1, 4))) -> Navickas[is.na(Navickas$year),]
Navickas$day <- na_if(Navickas$day,'')
Navickas <- Navickas[Navickas$year > 1789,]
Navickas$Weekday <- factor(Navickas$day, 
                           levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                       'Saturday', 'Sunday'))

# Navickas analysis

Navickas %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1790 - 1803","1804 - 1818",
                                                   "1819 - 1833","1834 - 1848"))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf) # Table 2

Navickas %>%
  mutate(Period = cut(year, breaks = 2, labels = c("1790 - 1818",
                                                   "1819 - 1848"))) %>%
  drop_na(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  count(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf) # Table 3

EventWorkingMonday_reg(Navickas, 1790) # n = 454
EventWorkingHours_reg(Navickas, 1790) # n = 454

# Political meetings in different local economic contexts

MancAndBlackburn <- c("Manchester", "Blackburn")

Navickas %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1790 - 1803","1804 - 1818",
                                                   "1819 - 1833","1834 - 1848"))) %>%
  drop_na(Weekday) %>%
  filter(place %in% MancAndBlackburn) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)

Navickas %>%
  mutate(Period = cut(year, breaks = 2, labels = c("1790 - 1818",
                                                   "1819 - 1848"))) %>%
  drop_na(Weekday, `Working hours (7-7)`) %>%
  filter(place %in% MancAndBlackburn) %>%
  group_by(Period) %>%
  count(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)

########################################

# Tiratelli riots data: Table 4 and 5

Tiratelli <- read_csv('https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Data_Tiratelli.csv')
Tiratelli <- Tiratelli[Tiratelli$Industrial=='No',]
Tiratelli$year <- as.numeric(Tiratelli$year)
Tiratelli$Weekday <- factor(Tiratelli$Weekday, 
                            levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                        'Saturday', 'Sunday'))

# Tiratelli analysis

Tiratelli %>%
  mutate(Period = ifelse(year<1835,"1800 - 1834",
                         ifelse(year<1870 & year>1834,"1835 - 1869",
                                ifelse(year<1905 & year>1869,"1870 - 1904","1905 - 1939")))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf) # Table 4

Tiratelli %>%
  mutate(Period = ifelse(year>1869,"1870 - 1939","1800 - 1869")) %>%
  drop_na(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  count(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf) # Table 5

EventWorkingMonday_reg(Tiratelli, 1800) # n = 261
EventWorkingHours_reg(Tiratelli, 1800) # n = 261

# Riots in different local economic contexts

Tiratelli %>%
  mutate(Period = ifelse(year<1835,"1800 - 1834",
                         ifelse(year<1870 & year>1834,"1835 - 1869",
                                ifelse(year<1905 & year>1869,"1870 - 1904","1905 - 1939")))) %>%
  drop_na(Weekday) %>%
  filter(City %in% c("Liverpool","Glasgow")) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)
  
Tiratelli %>%
  mutate(Period = ifelse(year<1835,"1800 - 1834",
                         ifelse(year<1870 & year>1834,"1835 - 1869",
                                ifelse(year<1905 & year>1869,"1870 - 1904","1905 - 1939")))) %>%
  drop_na(Weekday) %>%
  filter(City %in% c("Manchester")) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf)

########################################

# Tilly contentious events data: Table 6

Tilly <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Data_Tilly.csv")
Tilly <- Tilly[Tilly$ADAY=="EXACT" & Tilly$TYPE!='STRIKES, TURNOUTS',]
Tilly$year <- as.numeric(paste('1',as.character(Tilly$CGIDYEAR),sep=""))
Tilly$Weekday <- na_if(Tilly$WDAY,'')
Tilly$Weekday <- Tilly$Weekday
Tilly$Weekday <- factor(Tilly$Weekday, 
                        levels = c ('MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 
                                    'SATURDAY', 'SUNDAY'))

# Tilly analysis

Tilly %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1758 - 1776","1777 - 1795",
                                                   "1796 - 1814","1815 - 1834"))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) %>%
  print(., n= Inf) # Table 6


########################################

# Historical estimates of days worked per year: Figure 1

temp = tempfile(fileext = ".xlsx")
URL <- "https://www.bankofengland.co.uk/-/media/boe/files/statistics/research-datasets/a-millennium-of-macroeconomic-data-for-the-uk.xlsx"
download.file(URL, destfile=temp, mode='wb')

data <- read_excel(temp, sheet = 'A54. Hours worked', skip = 4)
data <- data[c(1,3,5,6,7,8,10)]
names(data) <- c('Years', "Allen & Weisdorf (2011): Agriculture", "Allen & Weisdorf (2011): Construction",
                 "Humphries & Weisdorf (2016)","Blanchard (1978)", "Clark & van der Werf (1998)", "Voth (2001)")

data$`Voth (2001)` <- data$`Voth (2001)`/10  # Turn estimates of hours worked into estimates of days worked

data %>%
  pivot_longer(-Years, names_to = "Source", values_to = "Value") -> data

points <- c("Blanchard (1978)","Voth (2001)")

wrapper <- function(x, ...) {paste(strwrap(x, ...), collapse = "\n")}

ggplot() +
  geom_line(na.omit(data[!data$Source %in% points,]), mapping = aes(x = Years, y = Value, linetype = Source)) +
  geom_point(na.omit(data[data$Source %in% points,]), mapping = aes(x = Years, y = Value, shape = Source), size = 2) +
  theme_classic() + ylab("Days worked per year") + xlab(NULL) +
  guides(linetype = guide_legend(nrow=2,byrow=TRUE, order = 1), shape = guide_legend(nrow=2,byrow=TRUE, order = 2)) +
  labs(title = "Figure 3: Estimates of days worked per year",
       caption = wrapper("Sources: Allen & Weisdorf (2011) estimate the total number of working days needed to purchase a basket of goods for agricultural labourers in Southern England and builders in London; Blanchard (1978) estimates days worked per year for English miners; Clark & van der Werf (1998) assume perfect arbitrage and divide the annual salary by the day wage for agricultural labourers in Britain; Humphries & Weisdorf (2016) repeat the arbitrage calculation for a larger sample of annually and casually contracted workers in different trades across Britain; Voth (2001) estimates days worked on the basis of court records and witness accounts from London and northern England.", width = 150)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot",
        text=element_text(family="Times"),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background= element_rect(fill = "transparent", colour = NA),
  )

ggsave(filename = "Figure_1.eps",
       family = "Times",
       dpi = 300,
       bg = "transparent",
       width=8, height=4.5)


########################################

# Robustness tests

# Navickas large events

Navickas$attendance <- ifelse(Navickas$attendance == "20,000", "20000", Navickas$attendance)
Navickas$NUM <- as.numeric(Navickas$attendance)

for (i in c(99,499,999)) {
  Navickas %>%
    mutate(Period = cut(year, breaks = 4, labels = c("1790 - 1803","1804 - 1818",
                                                     "1819 - 1833","1834 - 1848"))) %>%
    drop_na(Weekday, NUM) %>%
    filter(NUM > i) %>%
    group_by(Period) %>%
    count(Weekday, .drop = FALSE) %>%
    group_by(Period) %>%
    mutate(Proportion = n/sum(n)) %>%
    print(., n= Inf)
}

# Tilly large events

for (i in c(99,499,999)) {
  Tilly %>%
    mutate(Period = cut(year, breaks = 4, labels = c("1758 - 1776","1777 - 1795",
                                                     "1796 - 1814","1815 - 1834"))) %>%
    drop_na(Weekday,PNUM) %>%
    filter(PNUM > i) %>%
    group_by(Period) %>%
    count(Weekday, .drop = FALSE) %>%
    group_by(Period) %>%
    mutate(Proportion = n/sum(n)) %>%
    print(., n= Inf)
}

########################################

# Combining

Tiratelli$year <- as.numeric(Tiratelli$year)
Tiratelli$Weekday <- as.character(Tiratelli$Weekday)
Navickas$year <- as.numeric(Navickas$year)
Navickas$Weekday <- as.character(Navickas$Weekday)
Tilly$year <- as.numeric(Tilly$year)
Tilly$Weekday <- as.character(Tilly$Weekday)

Tiratelli %>%
  select(year, Weekday) -> Tiratelli
Navickas %>%
  select(year, Weekday) -> Navickas
Tilly %>%
  select(year, Weekday) -> Tilly

Data <- bind_rows(Tiratelli, Tilly, Navickas)
Data$Weekday <- as.factor(Data$Weekday)
Data$Weekday <- recode_factor(Data$Weekday, 'MONDAY' = "Monday", 'TUESDAY' = "Tuesday", 'WEDNESDAY' = "Wednesday",
                              'THURSDAY' = "Thursday", 'FRIDAY' = "Friday", 'SATURDAY' = "Saturday", 'SUNDAY' = "Sunday")

Data %>%
  mutate(Period = cut(year, breaks = 8, labels = c("1758 - 1779","1780 - 1801","1802 - 1824",
                                                   "1825 - 1846","1847 - 1868","1869 - 1891",
                                                   "1892 - 1913","1914 - 1936"))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals

graph_heatmap(Totals, "Figure 1: Political events from 1758 to 1936 (n = 7,233)", "Sources: Tilly and Horn (1988), Tiratelli (2019) and Navickas (2020)")
