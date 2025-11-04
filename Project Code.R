library(tidyverse)
library(usmap)
library(e1071)
library(scales)

# read in data set
crime_raw <- read.csv("crime_report.csv")

# ---------- Initial Computations ----------
## create data frame with state region and state abbreviation
regions <- data.frame(state.region, state.abb)

## separate "agency_jurisdiction" into "state" and "city" columns
crime <- na.omit(crime_raw) %>% 
  separate(agency_jurisdiction, c("city", "state"), sep = ", ") %>% 
  left_join(regions, by = c("state" = "state.abb"))

## use "South" to replace remander of missing values as D.C. was not classified
for (i in 1:nrow(crime)){ 
  if (is.na(crime$state.region[i])){ 
    crime$state.region[i] <- "South"  
  } 
}

# ---------- Basic Exploratory Analysis ----------
## distribution of report years
crime %>% 
  ggplot(aes(report_year)) +
  geom_bar()

table(crime$report_year) 

## Distribution of States
crime %>% 
  count(state) %>% 
  mutate(state = reorder(state, n)) %>% 
  ggplot(aes(state, n)) +
  geom_bar(stat = "identity") +
  coord_flip() 

## Distribution of United States Regions
crime %>% 
  ggplot() + 
  geom_bar(aes(state.region))  

## Distribution of Violent Crimes
ggplot(crime) +  
  geom_histogram(aes(violent_crimes), bins = 30) 

iqr <- IQR(crime$violent_crimes) 
 
ggplot(crime, aes(violent_crimes)) +  # without outliers
  geom_boxplot() +  
  coord_cartesian(xlim = c(0, quantile(crime$violent_crimes, .75) + 1.5*iqr)) 

## Distribution of Population
options(scipen = 1000) 
ggplot(crime) + # histogram
  geom_histogram(aes(population), bins = 35) 

ggplot(crime) + # boxplot
  geom_boxplot(aes(population)) 

## Distributions for Each Violent Crime
crime %>%  # histogram
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%  
  ggplot(aes(reports)) +  
  geom_histogram(bins = 25) +  
  coord_cartesian(xlim = c(0, quantile(crime$violent_crimes, .75) + 1.5*iqr)) + 
  facet_wrap(~ crime_type) 

crime %>% # boxplot
  gather(key = "crime_type", value = "counts", rapes, homicides, robberies, assaults) %>% 
  ggplot(aes(counts)) + 
  geom_boxplot() + 
  facet_wrap(~ crime_type) +
  coord_cartesian(xlim = c(0, quantile(crime$violent_crimes, .75) + 1.5*iqr))

# ---------- Advanced Analysis ----------
##  Population/Violent Crimes Correlation
options(scipen = 1000)
crime %>% 
  gather(key = "crime_type", value = "counts", rapes, homicides, robberies, assaults) %>% 
  ggplot(aes(population, violent_crimes)) +
  geom_jitter() +
  geom_smooth(method = lm)

cor(crime$population, crime$violent_crimes) 

cor(crime[, c("rapes", "homicides", "assaults", "robberies", "population")])[,5]

options(scipen = 1000)
crime %>% 
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>% 
  ggplot(aes(population, reports)) +
  geom_jitter() + 
  geom_polygon() +
  facet_wrap( ~ crime_type)

## Violent Crimes Time Series
crime$report_year <- as.Date(as.character(crime$report_year), "%Y")

crime %>% 
  group_by(report_year) %>% 
  summarise(avg_crimes = mean(violent_crimes)) %>% 
  ggplot() + 
  geom_line(aes(x = report_year, y = avg_crimes)) +
  scale_y_continuous(label = comma) +
  labs(title = "Average Crimes per Year", x = "Year", y = "Average Crimes")

crime %>% 
  group_by(report_year) %>% 
  summarise(avg_crimes = mean(violent_crimes)) %>%
  arrange(desc(avg_crimes)) %>% 
  head(5)

crime %>% 
  group_by(report_year, state.region) %>% 
  summarise(avg_crimes = mean(violent_crimes, na.rm = TRUE)) %>% 
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

crime %>% 
  group_by(report_year, state.region, state) %>% 
  summarise(avg_crimes = mean(violent_crimes, na.rm = TRUE)) %>%
  filter(state.region == "North Central") %>% 
  ggplot() + 
  geom_line(aes(x = report_year, y = avg_crimes, color = state)) + 
  scale_y_continuous(label = comma) +
  labs(title = "Average Crimes per Year", x = "Year", y = "Average Crimes") 

## Violent Crimes Proportions Time Series
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
  group_by(report_year, crime_type) %>% 
  summarize(tot_reports = sum(reports),
            tot_crimes = sum(violent_crimes)) %>% 
  ggplot(aes(report_year, tot_reports, fill = crime_type)) + 
  geom_histogram(stat = "identity", position = "fill") 

## Average Violent Crimes per Capita
avg_crimes_percap_state <- crime %>% 
  group_by(city, state, state.region) %>% 
  summarise(avg_crimes_percap = mean(violent_crimes/population)*100000) 

plot_usmap(data = avg_crimes_percap_state, regions = "state", values = "avg_crimes_percap", color = "black") +
  labs(title="Yearly State Average Crimes per Capita ") +
  scale_fill_continuous(low = "grey", high = "red", name = "Average Crimes per Capita") +
  theme(legend.position = "right", legend.key.size = unit(1, 'cm'))

avg_crimes_percap_state %>% 
  arrange(desc(avg_crimes_percap)) %>% 
  head(5)

avg_crimes_percap_state %>% 
  arrange(avg_crimes_percap) %>% 
  head(5)

## Crime Reports per Region
crime %>%  
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%
  group_by(state.region, crime_type) %>% 
  summarize(tot_reports = sum(reports)) %>%
  ggplot(aes(crime_type, tot_reports)) +
  geom_bar(stat = "identity") + facet_wrap(~ state.region)

crime %>%  
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%
  group_by(crime_type, state) %>% 
  summarize(tot_reports = sum(reports)) %>% 
  filter(tot_reports == max(tot_reports)) %>% 
  arrange(desc(tot_reports))

crime %>%  
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%
  group_by(crime_type, state) %>% 
  summarize(tot_reports = max(sum(reports))) %>% 
  filter(state == c("NY", "CA")) %>% 
  arrange(crime_type, desc(tot_reports)) %>% 
  ggplot(aes(state, tot_reports)) +
  geom_bar(stat = "identity") +
  facet_wrap( ~ crime_type) +
  coord_flip()

crime %>% 
  gather(key = "crime_type", value = "reports", rapes, homicides, robberies, assaults) %>%
  group_by(state) %>% 
  summarise(tot_pop = sum(population),
            tot_reports = sum(reports)) %>% 
  arrange(desc(tot_pop))



