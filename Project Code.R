library(tidyverse)
library(usmap)
library(e1071)
library(scales)
crime_raw <- read.csv("C:/Users/baisdera/OneDrive - Seton Hall University/Fall 2022/BITM 3744/Project/Crime_Report.csv")
regions <- data.frame(state.region, state.abb)


crime <- na.omit(crime_raw) %>% 
  separate(agency_jurisdiction, c("city", "state"), sep = ", ") %>% 
  left_join(regions, by = c("state" = "state.abb"))


crime %>% 
  anti_join(regions)


crime$p.homicides <- round(100 * crime$homicides/crime$violent_crimes, 2)
crime$p.rapes <- round(100 * crime$rapes/crime$violent_crimes, 2)
crime$p.assaults <- round(100 * crime$assaults/crime$violent_crimes, 2)
crime$p.robberies <- round(100 * crime$robberies/crime$violent_crimes, 2)
crime$report_year <- as.Date(as.character(crime$report_year), "%Y")

ggplot(crime) + 
  geom_histogram(aes(violent_crimes), bins = 30)

ggplot(crime, aes(violent_crimes)) + 
  geom_boxplot()

iqr <- IQR(crime$violent_crimes)

ggplot(crime, aes(violent_crimes)) + 
  geom_boxplot() + 
  coord_cartesian(xlim = c(0, quantile(crime$violent_crimes, .75) + 1.5*iqr))

ggplot(crime, aes(violent_crimes, state.region)) + 
  geom_boxplot() + 
  coord_cartesian(xlim = c(0, quantile(crime$violent_crimes, .75) + 1.5*iqr))



#Time series of total crimes each year
crime %>% 
  group_by(report_year) %>% 
  summarise(total_crimes = sum(violent_crimes, na.rm = TRUE)) %>% 
  ggplot() + 
  geom_line(aes(x = report_year, y = total_crimes)) +
  scale_y_continuous(label = comma) +
  labs(title = "Total Crimes per Year", x = "Year", y = "Total Crimes")

crime %>% 
  group_by(report_year, state.region) %>% 
  summarise(avg_crimes = mean(violent_crimes, na.rm = TRUE)) %>% 
  filter(!is.na(state.region)) %>% 
  ggplot() + 
  geom_line(aes(x = report_year, y = avg_crimes, color = state.region)) +
  scale_y_continuous(label = comma) +
  labs(title = "Average Crimes per Year", x = "Year", y = "Average Crimes")

crime %>% 
  group_by(report_year, state.region, state) %>% 
  summarise(avg_crimes = mean(violent_crimes, na.rm = TRUE)) %>%
  ggplot() + 
  geom_line(aes(x = report_year, y = avg_crimes, color = state)) +
  scale_y_continuous(label = comma) +
  labs(title = "Average Crimes per Year", x = "Year", y = "Average Crimes") +
  facet_wrap( ~ state.region)

avg_crimes_percap_state <- crime %>% 
  group_by(state) %>% 
  summarise(avg_crimes_percap = mean(crimes_percapita)) 

plot_usmap(data = avg_crimes_percap_state, regions = "state", values = "avg_crimes_percap", color = "black") +
  labs(title="Yearly State Average Crimes per Capita ") +
  scale_fill_continuous(low = "grey", high = "red", name = "Average Crimes per Capita") +
  theme(legend.position = "right", legend.key.size = unit(1, 'cm'))
        
crime %>% 
  group_by(report_year) %>% 
  summarise(total_crimes = sum(violent_crimes)) %>%
  arrange(desc(total_crimes)) %>% 
  head(10) %>% 
  ggplot(aes(report_year, total_crimes)) + 
  geom_histogram(stat = "identity") +
  scale_y_continuous(label = comma) +
  labs(title = "Total Crimes per Year", x = "Year", y = "Total Crimes")




c <-crime %>% 
  gather(key = "crime_type", value = "counts", rapes, homicides, assaults, robberies) 

crime %>% 
  ggplot(aes(robberies)) + geom_histogram()

cor(crime[, c("rapes", "homicides", "assaults", "robberies", "population")])[, 5]

subset(regions, !(state.abb %in% crime$state))

str(crime_raw)


crime %>%
  group_by(state.region) %>%
  gather(key = "crime_type", value = "counts", rapes, homicides, robberies, assaults) %>%
  ggplot(aes(x = reorder(state.region, -counts), fill = crime_type)) + 
  geom_bar() + xlab("State Regions")

table(c[, c("crime_type", "state.region", "counts")])



library(lubridate)

floor_date(crime$report_year, "year")

crime %>% 
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>% 
  group_by(report_year, crime_type) %>% 
  summarize(tot_reports = sum(reports),
            tot_crimes = sum(violent_crimes)) %>% 
  ggplot(aes(report_year, tot_reports)) + 
  geom_line(aes(color = crime_type)) +
  geom_line(aes(y = tot_crimes))
  


crime %>%  
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%
  group_by(crime_type, state) %>% 
  summarize(tot_reports = max(sum(reports))) %>% 
  filter(tot_reports == max(tot_reports)) %>% 
  arrange(crime_type, desc(tot_reports)) %>% 
  ggplot(aes(state, tot_reports)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ crime_type) +
  coord_flip()

c <- crime %>% 
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) 

