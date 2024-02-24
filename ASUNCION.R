# Script for generating the following visulizations related to SDG and Happiness
# 1. Bar plot of the top countries with highest SDG and happiness scores
# 2. Scatter plot of SDG and Happiness scores
# 3. Bivariate choropleth map of SDG and Happiness scores

# Author: Xavier Eugenio Asuncion
# Date: 26 Jan 2024

# Load required libraries
library(readxl)     # Read Excel files
library(dplyr)      # Data manipulation
library(ggplot2)    # Data visualization
library(tibble)     # Create tibbles
library(tidyr)      # Data tidying
library(ggpubr)     # Combine ggplots
library(ggalt)      # Alternative geoms
library(patchwork)  # Combine plots
library(readr)      # Read CSV files
library(ggtext)     # Text formatting in ggplot2
library(biscale)    # Bivariate scales
library(sf)         # Spatial data
library(cowplot)    # Combine ggplots

# Set working directory
setwd("/Users/ginoasuncion/Downloads/")

# Read and preprocess SDG 2023 data
sdg_2023 <- read_excel("SDR2023-data.xlsx", sheet = "SDR 2023 Data") |>
  rename('iso3' =  `Country Code ISO3`,
         "SDG Index Score" = "2023 SDG Index Score",
         'country' = Country,
         'population' = "Population in 2022") |>
  select(iso3, country, "SDG Index Score", "2023 SDG Index Rank", population, c(45:61))

# Read and preprocess World Happiness Report 2023 data
# Read and preprocess World Happiness Report 2023 data
happiness_2023 <- read_csv('WHR2023.csv') |> 
  rename(country = `Country name`,
         'Happiness Index Score' = `Ladder score`) |>
  select(country, `Happiness Index Score`) |>
  # Correct country names for consistency
  mutate(country = case_when(
    as.character(country) %in% c("Vietnam") ~ "Viet Nam",
    as.character(country) %in% c("Venezuela") ~ "Venezuela (Bolivarian Republic of)",
    as.character(country) %in% c("United States") ~ "United States of America",
    as.character(country) %in% c("United Kingdom") ~ "United Kingdom of Great Britain and Northern Ireland",
    as.character(country) %in% c("Turkiye") ~ "Turkey",
    as.character(country) %in% c("Tanzania") ~ "Tanzania, United Republic of",
    as.character(country) %in% c("Taiwan Province of China") ~ "Taiwan, Province of China",
    as.character(country) %in% c("State of Palestine") ~ "Palestine, State of",
    as.character(country) %in% c("South Korea") ~ "Korea, Republic of",
    as.character(country) %in% c("Russia") ~ "Russian Federation",
    as.character(country) %in% c("Moldova") ~ "Moldova, Republic of",
    as.character(country) %in% c("Laos") ~ "Lao People's Democratic Republic",
    as.character(country) %in% c("Iran") ~ "Iran (Islamic Republic of)",
    as.character(country) %in% c("Hong Kong S.A.R. of China") ~ "Hong Kong",
    as.character(country) %in% c("Congo (Kinshasa)") ~ "Congo, Democratic Republic of the",
    as.character(country) %in% c("Congo (Brazzaville)") ~ "Congo",
    as.character(country) %in% c("Bolivia") ~ "Bolivia (Plurinational State of)",
    TRUE ~ as.character(country)
  ))

# Read region data from CSV file
region_data <- read_csv('all.csv') |> 
  rename(country = name, iso3 = `alpha-3`) |>
  select(country, iso3, region, `sub-region`) 

# Read and transform spatial data for world map
world <- read_sf("R_S7_2024", "world")
st_crs(world)
world <- st_transform(world, 4326)

# Merge happiness data with region data
happiness_2023 <- merge(happiness_2023, region_data, all.x=TRUE, all.x.y=TRUE)

# Merge SDG and happiness data
sdg_happiness_2023 <- merge(sdg_2023, happiness_2023, by='iso3', all.x=TRUE, all.x.y=TRUE) 
sdg_happiness_2023 <- sdg_happiness_2023 |>
  # Filter out unnecessary rows
  filter(!(iso3 %in% sdg_happiness_2023$iso3[c(1:13)])) |>
  rename(country = `country.x`) |>
  select(-country.y)

# Merge SDG and happiness data with world spatial data
sdg_happiness_2023 <- merge(sdg_happiness_2023, 
                            world, by.x = "iso3", 
                            by.y="iso_a3", all.x=TRUE) |>
  st_as_sf()


# 1. Top 10 countries with highest SDG scores

# Calculate median SDG score for all countries
sdg2023_median <- median(sdg_happiness_2023$`SDG Index Score`, na.rm = TRUE)

# Create a data frame for median SDG score
median_sdg_others <- data.frame(
  "country" = "Median score",
  `SDG Score 2023` = sdg2023_median) |>
  rename(`SDG Index Score` = `SDG.Score.2023`)

# Extract top 10 SDG scoring countries and add median score
top_sdg_countries <- head(sdg_happiness_2023 |> 
                            arrange(desc(`SDG Index Score`)), 10) |> 
  select(country, `SDG Index Score`)
top_sdg_countries <- sf::st_drop_geometry(top_sdg_countries)
top_sdg_countries <- rbind(top_sdg_countries, median_sdg_others)

# Define colors for Nordic countries
nordic_countries <- c("Finland", "Sweden", "Denmark", "Norway", "Iceland", "Finland")
colors <- ifelse(top_sdg_countries$country %in% nordic_countries, "#59A14F", "#CCCCCC")

# Bar plot visualization
ggplot(top_sdg_countries, 
       aes(x = reorder(country, `SDG Index Score`), y = `SDG Index Score`)) +
  geom_bar(stat = "identity", fill = colors, width = 0.9) +  # Bar plot for SDG Index Score
  geom_text(aes(label = round(`SDG Index Score`, 1), x = country), 
            hjust = -0.2, vjust = 0.5, size = 8, color = colors, fontface = "bold") +  # Add data labels
  geom_text(aes(label = country, x = country), 
            hjust = 1.05, vjust = 0.5, size = 10, color = "white", fontface = "bold") +  # Add country labels
  labs(subtitle = "Leading 10 countries with the highest SDG Index Score in 2023",
       title = "<span style='color:#59A14F;'>Nordic countries</span> dominate the 2023 SDG rankings",
       caption = "Xavier Eugenio Asuncion\nData: Sustainable Development Report 2023") +  # Add plot titles and caption
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits
  theme(
    plot.title = element_markdown(size = 28, face = "bold"),  # Format plot titles
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),  # Format subtitle
    plot.caption = element_text(color="#696969"),  # Format caption
    panel.background = element_rect(fill = "white"),  # Set panel background color
    axis.text.y = element_text(margin = margin(r = -30), 
                               size=22, face="bold",
                               color=rev(colors)),  # Format y-axis labels
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title = element_blank(),  # Remove axis titles
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    strip.text = element_blank(),  # Remove facet title
    axis.ticks = element_blank()  # Remove axis ticks
  ) +
  scale_y_continuous(limits = c(0,  105)) +  # Set y-axis limits
  scale_x_discrete(labels = c(c(""), rev(seq_len(10)))) +  # Set x-axis labels
  coord_flip()  # Flip the coordinate system for horizontal bars

# Save the plot as an image
ggsave("SDG-top10-countries-barplot.png",
       bg = "white",
       scale = 1, height = 8,
       width = 10, dpi = 300)


# 2. Top 10 countries with highest Happiness scores

# Calculate median Happiness Score for all countries
happiness2023_median <- median(sdg_happiness_2023$`Happiness Index Score`, na.rm = TRUE)

# Create a data frame for median Happiness Score
median_happiness_others <- data.frame(
  "country" = "Median score",
  `Happiness Score 2023` = happiness2023_median) |>
  rename(`Happiness Index Score` = `Happiness.Score.2023`)

# Extract top 10 Happiness scoring countries and add median score
top_happiness_countries <- head(sdg_happiness_2023 |> 
                                  arrange(desc(`Happiness Index Score`)), 10) |> 
  select(country, `Happiness Index Score`)
top_happiness_countries <- sf::st_drop_geometry(top_happiness_countries)
top_happiness_countries <- rbind(top_happiness_countries, median_happiness_others)

# Define colors for Nordic countries
nordic_countries <- c("Finland", "Sweden", "Denmark", "Norway", "Iceland", "Finland")
colors <- ifelse(top_happiness_countries$country %in% nordic_countries, "#59A14F", "#CCCCCC")

# Top 10 Happiness countries plot
ggplot(top_happiness_countries, 
       aes(x = reorder(country, `Happiness Index Score`), y = `Happiness Index Score`)) +
  geom_bar(stat = "identity", fill = colors, width = 0.9) +  # Bar plot for Happiness Index Score
  geom_text(aes(label = round(`Happiness Index Score`, 1), x = country), 
            hjust = -0.2, vjust = 0.5, size = 8, color = colors, fontface = "bold") +  # Add data labels
  geom_text(aes(label = country, x = country), 
            hjust = 1.05, vjust = 0.5, size = 10, color = "white", fontface = "bold") +  # Add country labels
  labs(subtitle = "Top 10 countries with the highest index scores in the WHR 2023",
       title = "<span style='color:#59A14F;'>Nordic countries</span> are the world's happiest in 2023",
       caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023") +  # Add plot titles and caption
  coord_cartesian(ylim = c(0, 100)) +  # Set y-axis limits
  theme(
    plot.title = element_markdown(size = 28, face = "bold"),  # Format plot titles
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),  # Format subtitle
    plot.caption = element_text(color="#696969"),  # Format caption
    panel.background = element_rect(fill = "white"),  # Set panel background color
    axis.text.y = element_text(margin = margin(r = -30), 
                               size=22, face="bold",
                               color=rev(colors)),  # Format y-axis labels
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.title = element_blank(),  # Remove axis titles
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    strip.text = element_blank(),  # Remove facet title
    axis.ticks = element_blank()  # Remove axis ticks
  ) +
  scale_y_continuous(limits = c(0,  10)) +  # Set y-axis limits
  scale_x_discrete(labels = c(c(""), rev(seq_len(10)))) +  # Set x-axis labels
  coord_flip()  # Flip the coordinate system for horizontal bars

# Save the plot as an image
ggsave("happiness-top10-countries-barplot.png",
       bg = "white",
       scale = 1, height = 8,
       width = 10, dpi = 300)


# Relationship of SDG and Happiness: All Regions

# Define the breaks
breaks <- c(0, 1e6, 1e7, 1e8, Inf)

# Create a new column 'population_group' based on breaks
sdg_happiness_2023$population_group <- cut(sdg_happiness_2023$population, breaks, 
                                           labels = c("0 to 1 million", "1 million to 10 million", "10 million to 100 million", "100 million and above"), right = FALSE)

# Function to remove spaces from a string
remove_spaces <- function(input_string) {
  gsub(" ", "", input_string)
}

# Scatter plot showing the relationship between SDG and Happiness Index Scores
ggplot(data = sdg_happiness_2023, aes(x = `SDG Index Score`, y = `Happiness Index Score`)) +
  geom_point(aes(color=`region`, size = population_group, alpha = 0.8)) +
  scale_size_manual(values = c("0 to 1 million" = 2, "1 million to 10 million" = 4,
                               "10 million to 100 million" = 8, "100 million and above" = 16)) +
  scale_color_manual(values = c("Asia" = "#E15759", "Europe" = "#B6992D", "Africa" = "#59A14F", 
                                "Oceania" = "#4E79A7", "Americas" = "#B07AA1", "NA" = "#79706E")) +
  labs(title = "Sustainability and Happiness Connection",
       subtitle = "SDG and Happiness Index Scores of Countries in 2023",
       x = "\nSDG Index Score (0-100)\n", 
       y = "Happiness Index Score (0-10)\n",
       caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023 and Sustainable Development Report 2023",
       size="Population",
       color="Region") +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),
    plot.caption = element_text(color="#696969"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 16, color="#696969"),
    axis.title = element_text(size = 18, face="bold", color="#696969"),
    panel.grid.major = element_line(color = "gray80", linewidth=0.05),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size=12, color="#696969", face = "bold"),
    legend.text = element_text(vjust=0.5, size=12, color="#696969"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(0.1, 'lines'),
    legend.key.width = unit(0.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.margin = margin(t = 5, unit = "pt"),
    legend.position = "bottom",  # Change legend position
    legend.justification = "center",  # Center the legend
    legend.box = "horizontal"  # Arrange legend items horizontally
  ) +
  # Annotation for correlation text
  annotate(geom = "text", x = 45, y = 8,
           label = paste("Correlation:", remove_spaces(paste(0.79, "***"))),
           size = 4,
           color = "#696969",
           fontface ='bold') +
  # Annotations for specific countries
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Ukraine", "Sweden")),
            aes(label = country),
            vjust = 2.5,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Finland")),
            aes(label = country),
            vjust = -1.5,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Denmark")),
            aes(label = country),
            hjust = 1.2,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Lebanon", "Afghanistan", "Botswana", "Congo, Dem. Rep.")),
            aes(label = country),
            vjust = 2.6,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Chad", "Guatemala", "Israel")),
            aes(label = country),
            vjust = -1.6,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  coord_cartesian(xlim = c(40, 100), ylim = c(1.5, 8.5))  +
  theme(legend.position="bottom", 
        legend.box="vertical", 
        legend.margin=unit(0, "cm")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  guides(alpha = "none")  +
  guides(size = guide_legend(override.aes = list(color = "#696969")))

# Save the plot as an image
ggsave("SDG-Happiness-2023-scatterplot.png",
       bg = "white",
       scale = 1,height = 8,
       width=10, dpi = 300)


# Relationship of SDG and Happiness: Europe

ggplot(data = sdg_happiness_2023, aes(x = `SDG Index Score`, y = `Happiness Index Score`, text = paste("country:", country))) +
  geom_point(aes(color=`region`, size = population_group, alpha = region)) +
  scale_size_manual(values = c("0 to 1 million" = 2, "1 million to 10 million" = 4,
                               "10 million to 100 million" = 8, "100 million and above" = 16)) +
  scale_color_manual(values = c("Asia" = "#E15759", "Europe" = "#B6992D", "Africa" = "#59A14F", 
                                "Oceania" = "#4E79A7", "Americas" = "#B07AA1", "NA" = "#79706E")) +
  scale_alpha_manual(values = c("Asia" = 0.1, "Europe" = 0.8, "Africa" = 0.1, 
                                "Oceania" = 0.1, "Americas" = 0.1, "NA" = 0.1)) +
  
  # Titles and labels
  labs(title = "Sustainability and Happiness Connection",
       subtitle = "SDG and Happiness Index Scores of Countries in 2023",
       x = "\nSDG Index Score (0-100)\n", 
       y = "Happiness Index Score (0-10)\n",
       caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023 and Sustainable Development Report 2023",
       size="Population",
       color="Region") +
  
  # Theme settings
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),
    plot.caption = element_text(color="#696969"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 16, color="#696969"),
    axis.title = element_text(size = 18, face="bold", color="#696969"),
    panel.grid.major = element_line(color = "gray80", linewidth=0.05),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size=12, color="#696969", face = "bold"),
    legend.text = element_text(vjust=0.5, size=12, color="#696969"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(0.1, 'lines'),
    legend.key.width = unit(0.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.margin = margin(t = 5, unit = "pt"),
    legend.position = "bottom",  # Change legend position
    legend.justification = "center",  # Center the legend
    legend.box = "horizontal"  # Arrange legend items horizontally
  ) +
  
  # Correlation annotation
  annotate(geom = "text", x = 45, y = 8,
           label=paste("Correlation:", remove_spaces(paste(0.79, "***"))),
           size = 4,
           color="#696969",
           alpha=0.1,
           fontface ='bold') +
  
  # Country labels
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Ukraine", "Sweden")),
            aes(label = country),
            vjust = 2.5,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Russian Federation")),
            aes(label = country),
            vjust = -3.1,
            hjust = 0.65,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Finland")),
            aes(label = country),
            vjust = -1.5,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Denmark")),
            aes(label = country),
            hjust = 1.2,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Lebanon", "Afghanistan", "Botswana", "Congo, Dem. Rep.")),
            aes(label = country),
            vjust = 2.6,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Chad", "Guatemala", "Israel")),
            aes(label = country),
            vjust = -1.6,
            size = 3,
            color = "#696969",
            alpha=0.1,
            show.legend = FALSE, fontface ='bold') +
  
  # Coordinate limits
  coord_cartesian(xlim = c(40, 100), ylim = c(1.5, 8.5))  +
  
  # Additional theme adjustments
  theme(legend.position="bottom", 
        legend.box="vertical", 
        legend.margin=unit(0, "cm")) + 
  guides(color = guide_legend(override.aes = list(size = 5))) +
  guides(alpha = "none")  +
  guides(size = guide_legend(override.aes = list(color = "#696969")))


# Save the plot as an image
ggsave("SDG-Happiness-2023-Europe-scatterplot.png",
       bg = "white",
       scale = 1,height = 8,
       width=10, dpi = 300)


# Relationship of SDG and Happiness: Asia

ggplot(data = sdg_happiness_2023, aes(x = `SDG Index Score`, y = `Happiness Index Score`,text = paste("country:", country))) +
  geom_point(aes(color=`region`, size = population_group, alpha = region)) +
  scale_size_manual(values = c("0 to 1 million" = 2, "1 million to 10 million" = 4,
                               "10 million to 100 million" = 8, "100 million and above" = 16)) +
  scale_color_manual(values = c("Asia" = "#E15759", "Europe" = "#B6992D", "Africa" = "#59A14F", 
                                "Oceania" = "#4E79A7", "Americas" = "#B07AA1", "NA" = "#79706E")) +
  scale_alpha_manual(values = c("Asia" = 0.8, "Europe" = 0.1, "Africa" = 0.1, 
                                "Oceania" = 0.1, "Americas" = 0.1, "NA" = 0.1)) +
  
  # Titles and labels
  labs(title = "Sustainability and Happiness Connection",
       subtitle = "SDG and Happiness Index Scores of Countries in 2023",
       x = "\nSDG Index Score (0-100)\n", 
       y = "Happiness Index Score (0-10)\n",
       caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023 and Sustainable Development Report 2023",
       size="Population",
       color="Region") +
  
  # Theme settings
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),
    plot.caption = element_text(color="#696969"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 16, color="#696969"),
    axis.title = element_text(size = 18, face="bold", color="#696969"),
    panel.grid.major = element_line(color = "gray80", linewidth=0.05),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size=12, color="#696969", face = "bold"),
    legend.text = element_text(vjust=0.5, size=12, color="#696969"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(0.1, 'lines'),
    legend.key.width = unit(0.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.margin = margin(t = 5, unit = "pt"),
    legend.position = "bottom",  # Change legend position
    legend.justification = "center",  # Center the legend
    legend.box = "horizontal"  # Arrange legend items horizontally
  ) +
  
  # Correlation Annotation
  annotate(geom = "text", x = 45, y = 8,
           label=paste("Correlation:", remove_spaces(paste(0.79, "***"))),
           size = 4,
           alpha=0.1,
           color="#696969",
           fontface ='bold') +
  
  # Country labels
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Ukraine", "Sweden")),
            aes(label = country),
            vjust = 2.5,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Finland")),
            aes(label = country),
            vjust = -1.5,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Denmark")),
            aes(label = country),
            hjust = 1.2,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Botswana", "Congo, Dem. Rep.")),
            aes(label = country),
            vjust = 2.6,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Lebanon", "Afghanistan", "Jordan",  "Korea, Rep.")),
            aes(label = country),
            vjust = 2.6,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("India")),
          aes(label = country),
          vjust = 4,
          size = 3,
          color = "#696969",
          show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Chad")),
            aes(label = country),
            vjust = -1.6,
            #hjust = 1.5,
            size = 3,
            color = "#696969",
            alpha=0.1,
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Israel", "Bahrain", "Singapore", "Lao PDR")),
            aes(label = country),
            vjust = -1.8,
            #hjust = 1.5,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Japan", "Pakistan")),
            aes(label = country),
            vjust = -3,
            #hjust = 1.5,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Thailand")),
            aes(label = country),
            vjust = -2,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  # Coordinate limits
  coord_cartesian(xlim = c(40, 100), ylim = c(1.5, 8.5))  +
  
  # Additional theme adjustments
  theme(legend.position="bottom", 
        legend.box="vertical", 
        legend.margin=unit(0, "cm")) + 
  
  guides(color = guide_legend(override.aes = list(size = 5))) +
  guides(alpha = "none") +  # To hide the alpha legend
  guides(size = guide_legend(override.aes = list(color = "#696969"))) 

# Save the plot as an image
ggsave("SDG-Happiness-2023-Asia-scatterplot.png",
       bg = "white",
       scale = 1,height = 8,
       width=10, dpi = 300)


# Relationship of SDG and Happiness: Africa
ggplot(data = sdg_happiness_2023, aes(x = `SDG Index Score`, y = `Happiness Index Score`, text = paste("country:", country))) +
  geom_point(aes(color=`region`, size = population_group, alpha = region)) +
  scale_size_manual(values = c("0 to 1 million" = 2, "1 million to 10 million" = 4,
                               "10 million to 100 million" = 8, "100 million and above" = 16)) +
  scale_color_manual(values = c("Asia" = "#E15759", "Europe" = "#B6992D", "Africa" = "#59A14F", 
                                "Oceania" = "#4E79A7", "Americas" = "#B07AA1", "NA" = "#79706E")) +
  scale_alpha_manual(values = c("Asia" = 0.1, "Europe" = 0.1, "Africa" = 0.8, 
                                "Oceania" = 0.1, "Americas" = 0.1, "NA" = 0.1)) +
  
  labs(title = "Sustainability and Happiness Connection",
       subtitle = "SDG and Happiness Index Scores of Countries in 2023",
       x = "\nSDG Index Score (0-100)\n", 
       y = "Happiness Index Score (0-10)\n",
       caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023 and Sustainable Development Report 2023",
       size="Population",
       color="Region") +
  
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 18, face = "bold", color="#696969"),
    plot.caption = element_text(color="#696969"),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(size = 16, color="#696969"),
    axis.title = element_text(size = 18, face="bold", color="#696969"),
    panel.grid.major = element_line(color = "gray80", linewidth=0.05),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 16, face = "bold"),
    legend.title = element_text(size=12, color="#696969", face = "bold"),
    legend.text = element_text(vjust=0.5, size=12, color="#696969"),
    legend.background = element_rect(fill = NA, colour = NA),
    legend.key.size = unit(0.1, 'lines'),
    legend.key.width = unit(0.5, 'lines'),
    legend.key = element_rect(colour = NA, fill = NA),
    legend.box.margin = margin(t = 5, unit = "pt"),
    legend.position = "bottom",  # Change legend position
    legend.justification = "center",  # Center the legend
    legend.box = "horizontal"  # Arrange legend items horizontally
  ) +
  
  annotate(geom = "text", x = 45, y = 8,
           label=paste("Correlation:", remove_spaces(paste(0.79, "***"))),
           size = 4,
           color="#696969",
           alpha=0.1,
           fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Ukraine", "Sweden")),
            aes(label = country),
            vjust = 2.5,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Finland")),
            aes(label = country),
            vjust = -1.5,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Denmark")),
            aes(label = country),
            hjust = 1.2,
            size = 3,
            alpha=0.1,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Botswana", "Congo, Dem. Rep.")),
            aes(label = country),
            vjust = 2.6,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +

  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Lebanon", "Afghanistan", "Jordan",  "Korea, Rep.")),
            aes(label = country),
            vjust = 2.6,
            alpha=0.1,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Jordan")),
            aes(label = "Egypt, Arab Rep."),
            vjust = 3.8,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("India")),
            aes(label = country),
            vjust = 4,
            alpha=0.1,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Chad",  "Mauritius", "Niger", "Congo, Rep.", "South Africa")),
            aes(label = country),
            vjust = -2,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Algeria")),
            aes(label = country),
            vjust = -2,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Israel", "Bahrain", "Singapore", "Lao PDR")),
            aes(label = country),
            vjust = -1.8,
            alpha=0.1,
            size = 3,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Japan", "Pakistan")),
            aes(label = country),
            vjust = -3,
            alpha=0.1,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Morocco")),
            aes(label = country),
            hjust = -0.3,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Tunisia")),
            aes(label = country),
            hjust = -0.4,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  geom_text(data = sdg_happiness_2023 |> filter(country %in% c("Thailand")),
            aes(label = country),
            vjust = -2,
            alpha=0.1,
            size = 3.2,
            color = "#696969",
            show.legend = FALSE, fontface ='bold') +
  
  coord_cartesian(xlim = c(40, 100), ylim = c(1.5, 8.5))  +
  
  theme(legend.position="bottom", 
        legend.box="vertical", 
        legend.margin=unit(0, "cm")) + 
  
  guides(color = guide_legend(override.aes = list(size = 5))) +
  guides(alpha = "none")   +  # To hide the alpha legend
  guides(size = guide_legend(override.aes = list(color = "#696969"))) 


# Save the plot as an image
ggsave("SDG-Happiness-2023-Africa-scatterplot.png",
       bg = "white",
       scale = 1,height = 8,
       width=10, dpi = 300)


# Relationship of SDG and Happiness: Bivariate Choropleth Map

# Bivariate Choropleth Map for the relationship between SDG Index Score and Happiness Index Score in 2023

# Data preparation
data <- bi_class(sdg_happiness_2023, x = "SDG Index Score", y = "Happiness Index Score", style = "quantile", dim = 3)

# Create map
map <- ggplot() +
  geom_sf(data = data, mapping = aes(fill = bi_class), 
          color = "black", size = 0.5, linewidth = 0.15, show.legend = FALSE) +  # Specify na.value
  bi_scale_fill(pal = "BlueYl", dim = 3) +
  labs(
    title = "Sustainability and Happiness Connection",
    subtitle = "SDG and Happiness Index Scores of Countries in 2023\n",
    caption = "Xavier Eugenio Asuncion\nData: World Happiness Report 2023 and Sustainable Development Report 2023",
  ) +
  bi_theme() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.margin = unit(c(0, 0, 0, 0), "mm"),
    plot.subtitle = element_text(size = 18, face = "bold", color = "#696969"),
    plot.caption = element_text(size = 12, color = "#696969")
  )

# Create legend
legend <- bi_legend(pal = "BlueYl", dim = 3, xlab = "SDG Score ", ylab = "Happiness Score ") +
  theme(
    axis.title = element_text(size = 11.5, face = "bold")  # Adjust the size and face as needed
  )

# Combine map with legend
ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, -0.20, -0.28, 0.2, 0.2,
            width = 1.2,
            height = 1.2) +
  theme(plot.margin = unit(c(-1, -1, -1, -1), "mm"))

# Save the plot as an image
ggsave("sdg-happiness-bivariate-choropleth.png",
       bg = "white",
       scale = 1,height = 8,
       width=12, dpi = 300)