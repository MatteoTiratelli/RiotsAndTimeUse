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
          legend.background= element_rect(fill = "transparent", colour = NA))
}

## Tiratelli riots data

Tiratelli <- read_csv('https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Riots.csv')
Tiratelli <- Tiratelli[Tiratelli$Industrial=='No',]
Tiratelli$year <- as.numeric(Tiratelli$year)
Tiratelli$Weekday <- factor(Tiratelli$Weekday, 
                            levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                        'Saturday', 'Sunday'))

Tiratelli %>%
  mutate(Period = ifelse(year<1835,"1800 - 1834",
                         ifelse(year<1870 & year>1834,"1835 - 1869",
                                ifelse(year<1905 & year>1869,"1870 - 1904","1905 - 1939")))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) -> Totals_MT

Totals_MT %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_MT

Totals_MT[,1:3] %>%
  pivot_wider(id_cols = Period, names_from = Weekday, values_from = n) %>%
  column_to_rownames(var="Period") %>%
  as.data.frame() %>%
  fisher.test(hybrid = TRUE, simulate.p.value = TRUE)

graph_heatmap(Totals_MT, "Heat map of riots from 1800 to 1939 n = 311", "Source: Tiratelli (2019)")


## Tiratelli working hours (7am - 7pm)

Tiratelli %>%
  mutate(Period = ifelse(year>1869,"1870 - 1939","1800 - 1869")) %>%
  drop_na(Weekday, `Working hours (7-7)`) %>%
  group_by(Period) %>%
  count(Weekday, `Working hours (7-7)`) -> Totals_MT_WH

Totals_MT_WH %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_MT_WH


## Navickas political meetings data

read_html("http://historyofpublicspace.uk/political-meetings-mapper-2/") %>% 
  html_nodes(css = "table.tablepress") -> Navickas
html_table(Navickas[[1]]) -> Navickas
as_tibble(Navickas) -> Navickas
Navickas$year <- as.numeric(str_sub(Navickas$date, start= -4))
Navickas[is.na(Navickas$year),] %>%
  mutate(year = as.numeric(str_sub(Navickas[is.na(Navickas$year),]$date, 1, 4))) -> Navickas[is.na(Navickas$year),]
Navickas$day <- na_if(Navickas$day,'')
Navickas <- Navickas[Navickas$year > 1789,]
Navickas$Weekday <- factor(Navickas$day, 
                           levels = c ('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 
                                       'Saturday', 'Sunday'))

Navickas %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1790 - 1803","1804 - 1818",
                                                   "1819 - 1833","1834 - 1848"))) %>%
  drop_na(day) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) -> Total_KN

Totals_KN %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_KN

Totals_KN[,1:3] %>%
  pivot_wider(id_cols = Period, names_from = Weekday, values_from = n) %>%
  column_to_rownames(var="Period") %>%
  as.data.frame() %>%
  chisq.test(simulate.p.value = TRUE)

graph_heatmap(Totals_KN, "Heat map of political meetings from 1790 to 1848, n = 1,452", "Source: Navickas (2020)")


## Tilly contentious events data

Tilly <- read_csv("https://raw.githubusercontent.com/MatteoTiratelli/RiotsAndTimeUse/main/Tilly_data.csv")
Tilly <- Tilly[Tilly$ADAY=="EXACT" & Tilly$TYPE!='STRIKES, TURNOUTS',]
Tilly$year <- as.numeric(paste('1',as.character(Tilly$CGIDYEAR),sep=""))
Tilly$Weekday <- na_if(Tilly$WDAY,'')
Tilly$Weekday <- Tilly$Weekday
Tilly$Weekday <- factor(Tilly$Weekday, 
                        levels = c ('MONDAY', 'TUESDAY', 'WEDNESDAY', 'THURSDAY', 'FRIDAY', 
                                    'SATURDAY', 'SUNDAY'))

Tilly %>%
  mutate(Period = cut(year, breaks = 4, labels = c("1758 - 1776","1777 - 1795",
                                                   "1796 - 1814","1815 - 1834"))) %>%
  drop_na(Weekday) %>%
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) -> Totals_CT

Totals_CT %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals_CT

Totals_CT[,1:3] %>%
  pivot_wider(id_cols = Period, names_from = Weekday, values_from = n) %>%
  column_to_rownames(var="Period") %>%
  as.data.frame() %>%
  chisq.test(simulate.p.value = TRUE)

graph_heatmap(Totals_CT, "Contentious gatherings from 1758 to 1834 (n = 5,495)", "Source: Tilly and Horn (1988)")


## Combining

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
  group_by(Period) %>%
  count(Weekday, .drop = FALSE) -> Totals

Totals %>%
  group_by(Period) %>%
  mutate(Proportion = n/sum(n)) -> Totals

graph_heatmap(Totals, "Political events from 1758 to 1936 (n = 7,233)", "Sources: Tilly and Horn (1988), Tiratelli (2019) and Navickas (2020)")


### Historical estimates of days worked per year

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

wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}


ggplot() +
  geom_line(na.omit(data), mapping = aes(x = Years, y = Value, colour = Source)) +
  geom_hline(aes(yintercept = 365), linetype = 'dashed') +
  theme_classic() + ylab("Days worked per year") + xlab("Year") +
  guides(colour=guide_legend(nrow=2,byrow=TRUE)) +
  labs(title = "Figure 5: Estimates of days worked per year",
       caption = wrapper("Sources: Allen & Weisdorf (2011) give total number of working days needed to purchase a basket of good for agricultural labourers in Southern England and builders in London; Blanchard (1978) estimates days worked per year for English miners; Clark & van der Werf (1998) calculate the annual wage divided by the day wage for agricultural labourers in Britain; Humphries & Weisdorf (2016) calculate the number of days a casual worker would need to work to earn an annually contracted workers wage; Voth (2001) estimates days worked on the basis of court records and witness accounts from London and northern England.", width = 150)) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        plot.title.position = "plot",
        plot.caption = element_text(hjust = 0),
        plot.caption.position = "plot")
