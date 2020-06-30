# COVID 19 Dataset Visualizations

rm(list = ls())
# Set Directory
setwd("C:\\Users\\samwet.dutta\\Documents\\R Files\\Data Visualization - COVID19 Dataset")

# Turn Off Scientific Notation
options(scipen=999)

# Load Libraries
library(ggplot2)
library(dplyr)


# Import the Dataset
covid_data <- read.csv("covid_global_data_2020-06-07.csv")
View(covid_data)

# Check the Structure
str(covid_data)


# Plot a Bar Graph
plot_bar <- covid_data %>% 
  group_by(Country_Region) %>% 
  summarise(total_confirmed = sum(Confirmed)) %>%
  filter(total_confirmed >= 185750) %>%
  ggplot(aes(x = reorder(Country_Region, total_confirmed), y = total_confirmed)) + 
  geom_bar(stat = "identity", fill = "dodgerblue1", colour = "black") +
  geom_text(aes(label = total_confirmed), vjust = -0.5) +
  labs(title = "COVID-19 Confirmed Cases by Country", x = "Country", y = "Confirmed Cases") +
  theme_classic()

plot_bar

# Plot a Pie Chart
plot_pie <- covid_data %>% 
  group_by(Country_Region) %>% 
  summarise(total_confirmed = sum(Confirmed)) %>%
  filter(total_confirmed >= 185750) %>%
  ggplot(aes(x = "", y = total_confirmed, fill = Country_Region)) +
  geom_bar(stat = "identity", width = 1) + coord_polar("y", start = 0)
plot_pie


# Relation between Confirmed Cases and GDP (PPP) per Capita and Population

covid_ppp_data <- read.csv("COVID-19 Confirmed Cases and PPP.csv")


plot_scatter <- covid_ppp_data %>% filter(Confirmed.Cases > 10000) %>%
  ggplot(aes(x = Confirmed.Cases, y = Population, color = GDP.per.Capita)) +
  geom_point() + 
  labs(title =  "COVID-19 Cases by Population and GDP per Capita", x = "Confirmed Cases", y = "Population",
       caption = "Created by: Samwet, Tools Used: R Studio and ggplot2 Library \n
       Data Sources: Johns Hopkins CSSE (COVID19 Data - 7 Jun'20) and Wikipedia (GDP per Capita - 2020 Estimates and Population Data - 1 Jul'19)") +
  scale_color_gradient2(low = 'black', mid = 'deeppink', high = 'green') +
  geom_text(aes(label=Country, fontface = "bold"),  vjust = -0.75, check_overlap =  T) +
  theme_bw()

plot_scatter


# Zoomed In Visualization using X and Y Limits
plot_scatter_z <- covid_ppp_data %>% filter(Confirmed.Cases > 0) %>%
  ggplot(aes(x = Confirmed.Cases, y = Population, color = GDP.per.Capita)) +
  geom_point() + 
  labs(title =  "COVID-19 Cases by Population and GDP per Capita", x = "Confirmed Cases", y = "Population",
       caption = "Created by: Samwet, Tools Used: R Studio and ggplot2 Library \n
       Data Sources: Johns Hopkins CSSE (COVID19 Data - 7 Jun'20) and Wikipedia (GDP per Capita - 2020 Estimates and Population Data - 1 Jul'19)") +
  scale_color_gradient2(low = 'yellow', mid = 'deeppink', high = 'green') +
  geom_text(aes(label=Country, fontface = "bold"),  vjust = -0.75, check_overlap = T) +
  xlim(25000,250000) + ylim(1000000,500000000) +
  theme_bw()

plot_scatter_z
