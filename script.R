# loading libraries

library(tidyverse)
library(lubridate)
library(ggthemes)
library(extrafont)
library(Cairo)

loadfonts("win")

# load data
shelters <- read_csv("shelters.txt")

# break the occupancy_date variable into individual components

shelters <- shelters %>%
  mutate(Year = year(occupancy_date),
         Month = month(occupancy_date, label = T),
         Day = day(occupancy_date)) %>%
  select(id,Year, Month, Day, everything())

# Select the top 7 Organizations and merge the others

shelters <- shelters %>%
  mutate(name = fct_lump(organization_name, n = 7)) 

# Calculate the mean occupancy per organization name
 shelters %>%
  select(name, Year, Month, Day, occupancy, capacity, sector) %>%
  group_by(name, Year, Month ) %>%
  summarise(avg_occupancy = round(mean(occupancy, na.rm = T),0)) %>%
  ungroup() %>%

# Variable to highlight COSTI Immigrant Services on the plot
  mutate(test = (name == "COSTI Immigrant Services")) %>%

# Start plot  
  ggplot(aes(Month, avg_occupancy,group = name, color = name))+
  geom_line(aes(linetype = test),alpha = 0.8, size = 1.2 )+
  facet_wrap(~Year)+

# Labels   
  labs(color = "Organisation Name",
       title = "Average Movement of Shelter Home occupacy across years",
       subtitle = "\nAlthough, City of Toronto has more number of occupants,
       \nCOSTI Immigrant Services has the highest average number of occupants per month.",
       x ="",
       y = "Number of people")+

# To design the legend area  
  guides(colour = guide_legend(  title = "Organisation Names",
                                 keywidth = 6,
                                 keyheight = 2, 
                                 title.position = "top",
                                 title.hjust = .5,
                                 title.theme = element_text(face = "bold")))+
  
  
# To manually highlight COSTI Immigrant Services on the graph  
  scale_linetype_manual(values = c("solid","dashed"),guide = "none") +

# For line colour specifications     
  scale_color_brewer(type = "qual",palette = "Dark2")+
  
  
  theme_fivethirtyeight() +

# Extra edits  
  theme(
    text = element_text(family = "Verdana"),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.box.background  = element_blank(),
    strip.text = element_text(face = "italic"),
    plot.title = element_text(face = "bold",
                              size = 30,
                              hjust = 0.5),
    plot.subtitle = element_text(size = 14,
                                 hjust = 0.5)
    
  ) +
   
# To save the plot   
   ggsave(
   filename = "Shelter-homes.png",
   width = 37, 
   height = 22, 
   units = "cm",
   type = "cairo-png",
   dpi = 300)
