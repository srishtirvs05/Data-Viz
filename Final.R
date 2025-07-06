#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("networkD3")
library(networkD3)
#install.packages("leaflet")
library(leaflet)
#install.packages("lubridate")
library(lubridate)

###Los Angeles crime dataset 

# Inputting the dataset and storing
crime_df <- read.csv(file.choose())

# Checking the substantial dataset 
NumberOfColumns = 28
NumberOfRows = 892934
(NumberOfColumns * 4) * (NumberOfRows/100) >= 100
dim(crime_df)

# Checking the structure of the dataset
str(crime_df)

# Checking for the missing values
sum(is.na(crime_df))

# Changing the date columns from character to datetime and creating a new column
crime_df$Date_occurence <- as.POSIXct(crime_df$DATE.OCC, format="%m/%d/%Y %H:%M:%S")

# Adding an additional column and retaining the rows only for 2023
LAPD_dataset <- crime_df %>%
  filter(format(Date_occurence, "%Y") == "2023")

# Checking the rows and columns of the new dataset
dim(LAPD_dataset)

# Checking for the missing values
is.na(LAPD_dataset)

# Removing the rows which have victim sex as unknown
LAPD_data1 <- LAPD_dataset %>%
  filter(Vict.Sex != 'X', Vict.Descent != 'X', Vict.Age > 0)

# View the dataset
View(LAPD_data1)

# Visualization 1: Developing a display text for reporting a number

# Extract the date from the POSIXct column (ignoring the time part)
LAPD_data1$date <- as.Date(LAPD_data1$Date_occurence)

# Group by date and count the number of crimes for each day
crime_counts <- LAPD_data1 %>%
  group_by(date) %>%
  summarise(crimes_reported = n())


# Calculate the average number of crimes reported per day
average_crimes <- mean(crime_counts$crimes_reported)

# Create the visualization
ggplot() +
  annotate("text", x = 1, y = 1, label = paste("Avg Crimes per Day:", round(average_crimes, 2)),
           size = 8, color = "blue", fontface = "bold") +
  theme_void() +  # Remove axis and background
  theme(plot.margin = margin(20, 20, 20, 20))  

# Visualization 2: Understanding the top 5 types of crime
top_5_crimes <- LAPD_data1 %>%
  group_by(Crm.Cd.Desc) %>%  
  summarise(crimes_reported = n()) %>%  
  arrange(desc(crimes_reported)) %>%  
  head(5)

# Plotting the top 5 crimes
ggplot(top_5_crimes, aes(x = reorder(Crm.Cd.Desc, crimes_reported), y = crimes_reported, fill = Crm.Cd.Desc)) +
  geom_bar(stat = "identity", show.legend = FALSE) +  
  coord_flip() + 
  labs(title = "Top 5 Most Frequent Crimes Reported", x = "Crime Type", y = "Number of Crimes") +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# Visualization 3: creating a dataset for top 5 crime types and their map visual
# Filter and process the data for specific crime descriptions
filtered_data <- LAPD_data1 %>%
  filter(
    !is.na(LAT) & !is.na(LON),            
    LAT > 33.5 & LAT < 34.4,              
    LON > -118.7 & LON < -118.1,          
    Vict.Sex %in% c("F", "M"),
    Crm.Cd.Desc %in% c(                   
      "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT",
      "BATTERY - SIMPLE ASSAULT",
      "BURGLARY FROM VEHICLE",
      "INTIMATE PARTNER - SIMPLE ASSAULT",
      "THEFT OF IDENTITY"
    )
  )

aggregated_data <- filtered_data %>%
  group_by(LAT, LON, Crm.Cd.Desc) %>%
  summarise(total_crimes = n(), .groups = "drop")  

extreme_crime_areas <- aggregated_data %>%
  group_by(Crm.Cd.Desc) %>%
  filter(total_crimes == max(total_crimes)) %>%
  ungroup()

crime_palette <- colorFactor(
  palette = c("red", "green", "blue", "cyan", "orange"),
  domain = c(
    "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT",
    "BATTERY - SIMPLE ASSAULT",
    "BURGLARY FROM VEHICLE",
    "INTIMATE PARTNER - SIMPLE ASSAULT",
    "THEFT OF IDENTITY"
  )
)

leaflet_map <- leaflet(data = extreme_crime_areas) %>%
  addTiles() %>%  
  addCircleMarkers(
    ~LON, ~LAT, 
    radius = ~sqrt(total_crimes) * 2,  
    color = ~crime_palette(Crm.Cd.Desc),  
    fill = TRUE,
    fillOpacity = 0.7,
    popup = ~paste("Crime Type:", Crm.Cd.Desc, "<br>",
                   "Total Crimes:", total_crimes)
  ) %>%
  addLegend(
    "topright", 
    pal = crime_palette, 
    values = extreme_crime_areas$Crm.Cd.Desc,
    title = "Crime Description"
  ) %>%
  setView(lng = -118.4, lat = 34.05, zoom = 10)

# Visualizayion 4: Sankey chart
top_5_crimes <- c("BATTERY - SIMPLE ASSAULT", 
                  "BURGLARY FROM VEHICLE", 
                  "THEFT OF IDENTITY", 
                  "ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT", 
                  "INTIMATE PARTNER - SIMPLE ASSAULT")

crime_data_filtered <- LAPD_data1 %>%
  filter(Crm.Cd.Desc %in% top_5_crimes) %>%
  filter(!is.na(Premis.Desc))

crime_premis_count <- crime_data_filtered %>%
  group_by(Crm.Cd.Desc, Premis.Desc) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(Crm.Cd.Desc, desc(count))

top_5_premises <- crime_premis_count %>%
  group_by(Crm.Cd.Desc) %>%
  slice_max(count, n = 5) %>%
  ungroup()

crime_nodes <- unique(c(top_5_premises$Crm.Cd.Desc, top_5_premises$Premis.Desc))

node_id <- 0:(length(crime_nodes) - 1)

node_df <- data.frame(name = crime_nodes, id = node_id)

link_df <- top_5_premises %>%
  left_join(node_df, by = c("Crm.Cd.Desc" = "name")) %>%
  rename(source = id) %>%
  left_join(node_df, by = c("Premis.Desc" = "name")) %>%
  rename(target = id) %>%
  mutate(value = count)

sankey_data <- list(
  nodes = node_df,
  links = link_df
)

sankey <- sankeyNetwork(
  Links = sankey_data$links,
  Nodes = sankey_data$nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  units = "Crimes"
)

# Display the Sankey diagram
sankey

#Visualization 5: Heat map
LAPD_data1$Date_occurence <- as.Date(LAPD_data1$Date_occurence)

LAPD_data1$Day_Of_Week <- weekdays(LAPD_data1$Date_occurence)

LAPD_data1$Month <- month(LAPD_data1$Date_occurence, label = TRUE)

head(LAPD_data1[, c("Date_occurence", "Day_Of_Week", "Month")])

heatmap_data <- LAPD_data1 %>%
  group_by(Day_Of_Week, Month) %>%
  summarise(crimes_reported = n(), .groups = "drop")

heatmap_data$Day_Of_Week <- factor(heatmap_data$Day_Of_Week, 
                                   levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#Heatmap
heatmap_plot <- ggplot(data = heatmap_data, aes(x = Month, y = Day_Of_Week, fill = crimes_reported)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Crime Count") +
  scale_x_discrete(labels = month.name) +  
  labs(title = "Crime Frequency by Day of Week and Month", 
       x = "Month", y = "Day of the Week") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

monthly_crimes <- LAPD_data1 %>%
  group_by(Month) %>%
  summarise(total_crimes = n(), .groups = "drop")

#Visualization 6: column chart
column_chart <- ggplot(monthly_crimes, aes(x = Month, y = total_crimes)) +
  geom_bar(stat = "identity", fill = "#8abece", color = "black") +  
  geom_text(aes(label = total_crimes), vjust = -0.3, color = "black", size = 3.5) +  
  scale_x_discrete(labels = month.name) +  
  labs(title = "Total Crimes by Month", x = "Month", y = "Total Crimes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#Visualization 7: Bar chart
crime_data_filtered <- LAPD_data1 %>%
  filter(Crm.Cd.Desc %in% top_5_crimes) %>%
  filter(!is.na(TIME.OCC))

crime_data_filtered$Hour <- as.numeric(substr(crime_data_filtered$TIME.OCC, 1, 2))

crime_data_filtered$Time_Interval <- cut(crime_data_filtered$Hour,
                                         breaks = seq(0, 24, by = 3),  # 3-hour intervals
                                         include.lowest = TRUE,
                                         labels = c("12:00-3:00", "3:00-6:00", "6:00-9:00", "9:00-12:00",
                                                    "12:00-15:00", "15:00-18:00", "18:00-21:00", "21:00-24:00"),
                                         right = FALSE)  

crime_counts_by_time <- crime_data_filtered %>%
  group_by(Time_Interval) %>%
  summarise(crimes_reported = n(), .groups = "drop") %>%
  filter(!is.na(Time_Interval))

ggplot(crime_counts_by_time, aes(x = crimes_reported, y = reorder(Time_Interval, crimes_reported))) +
  geom_bar(stat = "identity", fill = "#8abece", color = "black") +  
  geom_text(aes(label = crimes_reported), hjust = -0.3, color = "black", size = 3.5) +  
  labs(title = "Total Crimes by 3-Hour Interval", 
       x = "Total Crimes", 
       y = "Time Interval") +
  theme_minimal() +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))

#Visualization 8: Stacked column
sex_data <- LAPD_data1 %>%
  filter(Crm.Cd.Desc %in% top_5_crimes) %>%
  filter(!is.na(Vict.Descent)) 

top_2_descents <- sex_data %>%
  count(Vict.Descent) %>%
  top_n(2, n)  

descents_top2 <- sex_data %>%
  filter(Vict.Descent %in% top_2_descents$Vict.Descent)

descents_top2 <- descents_top2 %>%
  filter(Vict.Sex %in% c("M", "F"))  

ggplot(data = descents_top2, aes(x = "Combined Crimes", fill = Vict.Sex)) +
  geom_bar(stat = "count", position = "stack") +  
  scale_fill_manual(values = c("blue", "pink")) +  
  labs(
    title = "Combined Crimes: Victim Descent and Sex (Top 2 Victim Descents)",
    x = "Crime Category", 
    y = "Count",
    fill = "Victim Sex"
  ) +
  facet_wrap(~ Vict.Descent, scales = "free_y") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  geom_text(stat = "count", aes(label = ..count..), position = position_stack(vjust = 0.5), color = "white")  # Add data labels in the middle of each stacked section
