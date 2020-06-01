#Data from US Dept of Justice Office of Justice Programs
#  https://www.bjs.gov/

library(tidyverse)
library(readxl)
library(here)
library(RColorBrewer)

arrests_black <- read_excel("arrests_by_race_black.xlsx", sheet = "data", skip = 12)
arrests_all <- read_excel("arrests_by_race_all.xlsx", sheet = "data", skip = 12)

arrests_black_fmt <- arrests_black %>%
  janitor::clean_names()%>% 
  mutate(arrests_pct = round((total_arrests / total_population), digits = 3)) %>% 
  mutate(which = "African American")  %>% 
  select(year, which, arrests_pct)

arrests_all_fmt <- arrests_all %>%
  janitor::clean_names()%>% 
  mutate(arrests_pct = round((total_arrests / total_population), digits = 3)) %>% 
  mutate(which = "All Americans")  %>% 
  select(year, which, arrests_pct)

arrests_fmt <- rbind(arrests_black_fmt, arrests_all_fmt)

arrests_fmt %>% 
  filter(year >= 1980, year <= 2014) %>% 
  mutate(year = as.factor(year)) %>%
  ggplot(aes(x = year, y = arrests_pct, group = which)) +
  geom_line(aes(col = which), size=1.2) + 
  scale_color_manual(values = brewer.pal(8,"Set1")) + 
  scale_y_continuous(labels = function(x) paste0(sprintf("%.0f", x*100),"%"), limits = c(0, 0.15), breaks = seq(0, 0.14, by = 0.02)) + 
  labs(y="Percentage", x = "Year", caption = "Source: US Dept of Justice - https://www.bjs.gov/") + 
  theme(axis.text.x = element_text(face="bold", angle = 50, hjust = 1), 
        axis.text.y = element_text(face="bold"), 
        plot.title = element_text(size = 18, face = "bold")) +  
  ggtitle("Arrests Per Year as a Percentage of Population")

  