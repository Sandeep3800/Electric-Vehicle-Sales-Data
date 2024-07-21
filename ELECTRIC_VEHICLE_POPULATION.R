install.packages('tidyverse')
install.packages('ggplot2')
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(ggplot2)

# Loading Data into Console
Electric_Vehicle_Population_Data <- read_csv(Electric_Vehicle_Population_Data)
# data cleaning



# creating individual TIBBLES for analysis by grouping and filtering
electric_vehicle_type <- Electric_Vehicle_Population_Data %>% group_by(`Electric Vehicle Type`) %>% count(`Electric Vehicle Type`)
electricvehicledistribution_eachyear <- Electric_Vehicle_Population_Data %>% group_by(`Model Year`) %>% count(`Model Year`) %>% arrange(desc(n))
top10_vehiclesales_basedon_model <- Electric_Vehicle_Population_Data %>% group_by(Make) %>% count(Make) %>% arrange(desc(n)) %>% head(10)
electricvehicledistribution_eachyear <- Electric_Vehicle_Population_Data %>% group_by(`Model Year`) %>% count(`Model Year`) %>% arrange(desc(n))
TESLA <- Electric_Vehicle_Population_Data %>% filter(Make == "TESLA") %>% count(Model)
CHEVROLET <- Electric_Vehicle_Population_Data %>% filter(Make == "CHEVROLET"&`Electric Range`>0) %>% count(Model)
FORD <- Electric_Vehicle_Population_Data %>% filter(Make == "FORD", `Electric Range`>0) %>% count(Model)
KIA <- Electric_Vehicle_Population_Data %>% filter(Make == "KIA", `Electric Range`>0) %>% count(Model)
AUDI <- Electric_Vehicle_Population_Data %>% filter(Make == "AUDI", `Electric Range`>0) %>% count(Model)
VOLVO <- Electric_Vehicle_Population_Data %>% filter(Make == "VOLVo", `Electric Range` >0) %>% count(Model)
avg_sales_eachyear <-Electric_Vehicle_Population_Data %>% group_by(Make, `Model Year`) %>% count(Make) %>% summarise(avg_sales=mean(n)) %>% arrange(desc(avg_sales)) %>% head(20)
Electric_Utility <- Electric_Vehicle_Population_Data%>% group_by(`Electric Utility`) %>% count(`Electric Utility`) %>% arrange(desc(n)) %>% head(20)
top10County <- Electric_Vehicle_Population_Data%>% group_by(County) %>% count(County) %>% arrange(desc(n)) %>% head(10)

# plotting for presentation

# Electric Vehicle Type
ggplot(data = electric_vehicle_type) +
  geom_col(mapping = aes(x = 1, y = n, fill = factor(`Electric Vehicle Type`)), stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1, y = n, label = n), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Distribution of Electric Vehicle Types", y = "Number of Vehicles") +
  theme_classic() +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Electric Vehicle Distribution Each Year
ggplot(data = electricvehicledistribution_eachyear) +
  geom_line(mapping = aes(x = `Model Year`, y = n, color = `Model Year`), stat = "identity") +
  labs(title = "Electric Vehicle Distribution Each Year", x = "Model Year", y = "Number of Electric Vehicles Sold") +
  theme_classic()

# Top 10 Models
ggplot(data = top10_vehiclesales_basedon_model, aes(x = Make, y = n)) + geom_bar(stat = "identity", fill = "purple") + labs(title = "Top 10 Selling Electric Car Models", x = "Make of Vehicle", y = "Number of Vehicles") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

#TESLA
ggplot(TESLA, aes(x = Model, y = n)) +
  +     geom_bar(stat = "identity", fill = "skyblue") +  # Set a specific color for the bars
  +     labs(title = "Number of Tesla Vehicles by Model",
             +          x = "Tesla Model",
             +          y = "Number of Vehicles") +
  +     theme_minimal()
#CHEVROLET
ggplot(data = CHEVROLET) + geom_bar(mapping = aes(x = Model, y =n), stat = 'identity', fill = 'skyblue') + labs(title = "Number of Chevrolet Vehicles by Model", x = "chevrolet Model", y = " Number of Vehicles") + theme_minimal()

#FORD
ggplot(data = FORD) + geom_bar(mapping = aes(x = Model, y =n), stat = 'identity', fill = 'skyblue') + labs(title = "Number of FORD Vehicles by Model", x = "Ford Model", y = " Number of Vehicles") + theme_minimal()

#KIA
ggplot(data = KIA) + geom_bar(mapping = aes(x = Model, y =n), stat = 'identity', fill = 'skyblue') + labs(title = "Number of KIA Vehicles by Model", x = "KIA Model", y = " Number of Vehicles") + theme_minimal()

# AUDI
ggplot(data = AUDI) + geom_bar(mapping = aes(x = Model, y =n), stat = 'identity', fill = 'skyblue') + labs(title = "Number of Audi Vehicles by Model", x = "Audi Model", y = " Number of Vehicles") + theme_minimal()

# VOLVO
ggplot(data = VOLVO) + geom_bar(mapping = aes(x = Model, y = n), stat = 'identity', fill = 'skyblue') + labs(title = "Number of VOLVO vehicles by Model", x = "Number of Volvo Vehicles by Model", y = "Number of Vehicles") + theme_minimal()

# Top 10 County 
ggplot(data = top10County) +
  +     geom_bar(mapping = aes(x = County, y = n, fill = factor(County)), stat = "identity") +
  +     geom_text(aes(x = County, y = n, label = n), angle = 90) +
  +     labs(title = "Top 10 County EV Registrations", x = "County", y = "Number of EV's") +
  +     theme_classic() +
  +     theme(axis.text.x = element_text(angle = 45, hjust = 1))


