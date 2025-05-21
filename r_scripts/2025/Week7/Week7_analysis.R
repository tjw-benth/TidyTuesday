############################################################
# Title: Tidy Tuesday FBI Crime Data
# Author: Tom Williams
# Date: 2025-02-21
# Description: tidy tuesday code on FBI crime data 
# Version: 4.4.1
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly") 

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}

invisible(lapply(pkgs, library, character.only = TRUE))

# =========================================================
# Install and load tidytuesday data ####
# =========================================================

install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-02-18')

agencies <- tuesdata$agencies


# =========================================================
# Explore the data ####
# =========================================================

str(agencies)
glimpse(agencies)
# Rows: 19,166
# Columns: 10


# counties, lat long, states, agencies, and dates

agencies %>%
  group_by(agency_name) %>%
  summarise(count = n())

agencies %>%
  group_by(agency_type) %>%
  summarise(count = n())

agencies %>%
  group_by(state) %>%
  summarise(count = n())

# Split date column into year, month, day and week day
agencies <- agencies %>%
  mutate(
    year = year(nibrs_start_date),
    month = month(nibrs_start_date, label = TRUE, abbr = TRUE),
    day = day(nibrs_start_date),
    weekday = wday(nibrs_start_date, label = TRUE, abbr = TRUE)
  )


# =========================================================
# Visualisation 1: Agencies over time ####
# =========================================================

# Step 1: Remove NAs in `nibrs_start_date`
agencies_na <- agencies %>% filter(is.na(nibrs_start_date))  # Store NA rows separately
agencies_clean <- agencies %>% filter(!is.na(nibrs_start_date))  # Remove NAs

# Step 2: Summarize data by Year and State
agencies_summary <- agencies_clean %>%
  group_by(year, state) %>%
  summarise(count = n(), .groups = "drop")

# Step 3: Plot using ggplot2
ggplot(agencies_summary, aes(x = year, y = count, fill = state)) +
  geom_area(alpha = 0.8, size = 0.2) +  # Creates the area (river) plot
  scale_fill_manual(values = met.brewer("Archambault", 52)) +  # One color per state
  labs(
    title = "Agencies Reporting by Year and State",
    x = "Year",
    y = "Count",
    fill = "State"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

# Step 4: Identify Top 10 States with the Highest Total Counts
top_states <- agencies_summary %>%
  group_by(state) %>%
  summarise(total_count = sum(count)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(state)  # Extract top 10 state names

# Step 5: Add a new column to distinguish top 10 vs. others
agencies_summary <- agencies_summary %>%
  mutate(
    is_top = ifelse(state %in% top_states, "Top 10", "Other")
  )

# Step 6: Plot top 10 states using ggplot
ggplot(agencies_summary, aes(x = year, y = count, group = state)) +
  # Background: Grey, transparent lines for all other states
  geom_line(data = filter(agencies_summary, is_top == "Other"),
            aes(group = state), color = "grey60", alpha = 0.5, size = 0.8) +
  
  # Foreground: Colored dashed lines for the top 10 states
  geom_line(data = filter(agencies_summary, is_top == "Top 10"),
            aes(color = state, linetype = state), size = 1) +
  
  scale_color_manual(values = met.brewer("Archambault", 10)) +  # Custom colors for top 10
  scale_linetype_manual(values = rep("dashed", 10)) +  # Dashed lines for top states
  labs(
    title = "Agency Reporting Over Time",
    x = "Year",
    y = "Number of Agencies",
    color = "Top 10 States",
    linetype = "Top 10 States"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

# =========================================================
# Viz 2: Improve temporal resolution, cumulative reporting ####
# =========================================================

# Step 1: Remove NAs
agencies_clean <- agencies %>% filter(!is.na(nibrs_start_date))

# Step 2: Extract Year, Month, and Create a Year-Month Column
agencies_clean <- agencies_clean %>%
  mutate(
    year = year(nibrs_start_date),
    month = month(nibrs_start_date, label = TRUE, abbr = TRUE),
    year_month = floor_date(nibrs_start_date, "month")  # Converts to Year-Month Date
  )

# Step 3: Summarize by Year-Month and State
agencies_summary <- agencies_clean %>%
  count(state, year_month) %>%
  arrange(state, year_month) %>%  # Ensure proper ordering before cumulative sum
  group_by(state) %>%
  mutate(cumulative_count = cumsum(n)) %>%
  ungroup()

# Step 4: Identify Top 10 States with the Highest Total Reports
top_states <- agencies_summary %>%
  group_by(state) %>%
  summarise(total_count = max(cumulative_count, na.rm = TRUE)) %>%
  arrange(desc(total_count)) %>%
  slice_head(n = 10) %>%
  pull(state)  # Extracts top 10 state names

# Step 5: Add a column to distinguish top 10 vs. others
agencies_summary <- agencies_summary %>%
  mutate(
    is_top = ifelse(state %in% top_states, "Top 10", "Other")
  ) 

# Step 6: Plot using ggplot
agencies_overtime_top10 <- ggplot(agencies_summary, aes(x = year_month, y = cumulative_count, group = state)) +
  # Background: Grey, transparent lines for all other states
  geom_line(data = filter(agencies_summary, is_top == "Other"),
            aes(group = state), color = "grey60", alpha = 0.5, size = 0.6) +
  
  # Foreground: Colored dashed lines for the top 10 states
  geom_line(data = filter(agencies_summary, is_top == "Top 10"),
            aes(color = state), size = 1) +
  
  ggrepel::geom_text_repel(
    data = agencies_summary %>%
      filter(is_top == "Top 10") %>%
      slice_max(order_by = year_month, n = 1, by = state),
    aes(label = state, color = state),
    direction = "y",      # Only allow vertical movement
    nudge_y = 50,        # Move overlapping labels slightly up/down
    nudge_x = 1500,
    min.segment.length = 0, # Always show connecting lines if moved
    box.padding = 0.3,    # More space around text
    point.padding = 0.2,  # Space between text and point
    size = 4,
    show.legend = FALSE
  ) +

  scale_color_manual(values = MetBrewer::met.brewer("Archambault", 10)) +  # Archambault colors
  scale_x_date(
    limits = as.Date(c("1985-01-01", "2030-01-01")),  # Data range
    breaks = seq(as.Date("1985-01-01"), as.Date("2026-01-01"), by = "5 years"),  # Visible tick marks
    labels = format(seq(as.Date("1985-01-01"), as.Date("2026-01-01"), by = "5 years"), "%Y")  # Formatting labels
  ) +
  labs(
    title = "Cumulative Agency Reports Over Time",
    x = "Date",
    y = "No. of Reports",
    color = "Top 10 States",
    linetype = "Top 10 States"
  ) +
  theme_classic() +  # Classic theme instead of minimal
  theme(legend.position = "none")

# =========================================================
# Viz 3: Top 10 states in a map ####
# =========================================================

library(sf)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(patchwork)
library(MetBrewer)

# Load USA state geometries
us_states <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf") %>%
  select(name, geometry) %>%
  rename(state = name) %>%
  mutate(state = toupper(state))  # Convert to uppercase

# Get the top 10 states from the line plot
top_10_states <- agencies_summary %>%
  filter(is_top == "Top 10") %>%
  distinct(state) %>%
  mutate(state = toupper(state))

# Create a fill variable: top 10 get their state name; all others become "Other"
us_states <- us_states %>%
  mutate(fill_color = ifelse(state %in% top_10_states$state, state, "Other"))

# Define color mapping (Top 10 = colored, Others = white)
colors <- MetBrewer::met.brewer("Archambault", 10)
fill_values <- c(setNames(colors, top_10_states$state), Other = "lightgrey")

# Identify Alaska, Hawaii, and American Samoa
special_states <- c("ALASKA", "HAWAII")

# Extract the special states
alaska <- us_states %>% filter(state == "ALASKA")
hawaii <- us_states %>% filter(state == "HAWAII")

# Extract the mainland states (excluding the special states)
mainland <- us_states %>% filter(!state %in% special_states)

### **Fixing Alaska (Antimeridian Issue)**
# Reproject Alaska to an Albers Equal Area projection
alaska <- st_transform(alaska, crs = "+proj=aea +lat_1=55 +lat_2=65 +lon_0=-154")

# Scale & move Alaska (enlarged slightly)
alaska <- alaska %>% mutate(geometry = geometry * 0.5 + c(-30, -15))

# Adjust Hawaii 
hawaii_transformed <- hawaii %>%
  st_transform(crs = st_crs(mainland)) %>%  # Ensure same CRS as mainland
  mutate(geometry = geometry * 1.2 + c(60, -5))  # Scale and shift correctly

# Plot Mainland USA
mainland_plot <- ggplot(mainland) +
  geom_sf(aes(fill = fill_color), color = "black", size = 0.2) +
  scale_fill_manual(values = fill_values, drop = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void() +
  theme(legend.position = "none")

# Plot Alaska
alaska_plot <- ggplot(alaska) +
  geom_sf(aes(fill = fill_color), color = "black", size = 0.2) +
  scale_fill_manual(values = fill_values, drop = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()  +
  theme(legend.position = "none")

# Plot Hawaii
hawaii_plot <- ggplot(hawaii_transformed) +
  geom_sf(aes(fill = fill_color), color = "black", size = 0.2) +
  scale_fill_manual(values = fill_values, drop = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void()  +
  theme(legend.position = "none")


# Convert Alaska & Hawaii to grobs (prevents scaling issues)
alaska_grob <- ggplotGrob(alaska_plot)
hawaii_grob <- ggplotGrob(hawaii_plot)

# Combine all plots using patchwork
final_map <- mainland_plot +
  annotation_custom(alaska_grob, xmin = -125, xmax = -110, ymin = 20, ymax = 33) +  
  annotation_custom(hawaii_grob, xmin = -95, xmax = -80, ymin = 18, ymax = 28) 

# Display the final map
final_map

# =========================================================
# Viz 4: Combining plots ####
# =========================================================

# Adjust the placement of the map over the line plot
final_combined_plot <- agencies_overtime_top10 +
  inset_element(final_map, left = 0.05, bottom = 0.6, right = 0.5, top = 1)

# Display the final combined plot
final_combined_plot
