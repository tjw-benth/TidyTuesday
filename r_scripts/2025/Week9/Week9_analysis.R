############################################################
# Title: Tidy Tuesday Long Beach Island Shelter
# Author: Tom Williams
# Date: 2025-03-04
# Description: tidy tuesday code on Long Beach Island Shelter data 
# Version: 4.4.1
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor",  "waffle", "showtext") 

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}

invisible(lapply(pkgs, library, character.only = TRUE))

# =========================================================
# Install and load tidytuesday data ####
# =========================================================

## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-03-04')

longbeach <- tuesdata$longbeach

# Option 2: Read directly from GitHub

longbeach <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-04/longbeach.csv')

# =========================================================
# Explore the data ####
# =========================================================

str(longbeach)
glimpse(longbeach)
# Rows: 19,166
# Columns: 10

# This dataset comprises of the intake and outcome record from Long Beach Animal Shelter.

# How has the number of pet adoptions changed over the years?
# Which type of pets are adopted most often?


# =========================================================
# 1. Pet intake/outtake over the years ####
# =========================================================

# Step 1: Prepare intake summary
intake_summary <- longbeach %>%
  mutate(intake_year = year(intake_date)) %>%
  group_by(intake_year, intake_type) %>%
  summarize(intake_count = n(), .groups = 'drop') %>%
  complete(intake_year, intake_type, fill = list(intake_count = 0)) %>%
  pivot_wider(
    names_from = intake_type, 
    values_from = intake_count, 
    values_fill = 0
  ) %>% drop_na() %>% janitor::clean_names()

# Clean up stray columns
intake_summary_cleaned <- intake_summary %>%
  mutate(
    stray = rowSums(across(c(stray, `s_ray`)), na.rm = TRUE)
  ) %>%
  # Remove the problematic stray column
  select(-`s_ray`) %>%
  rename(foster_intake = foster)

# Create a vector of intake types
intake_types <- c(
  names(intake_summary_cleaned %>%
          select(-intake_year))
)

# Verify the intake types are in the data
print(names(intake_summary_cleaned))
print(intake_types)

# Step 2: Prepare outcome summary
outcome_summary <- longbeach %>%
  mutate(outcome_year = year(outcome_date)) %>%
  group_by(outcome_year, outcome_type) %>%
  summarize(outcome_count = n(), .groups = 'drop') %>%
  complete(outcome_year, outcome_type, fill = list(outcome_count = 0)) %>%
  pivot_wider(
    names_from = outcome_type, 
    values_from = outcome_count, 
    values_fill = 0
  ) %>% drop_na() %>% janitor::clean_names() 

# Clean up na column
outcome_summary_cleaned <- outcome_summary %>%
  rename(unknown = na)

# Create a vector of outcome types
outcome_types <- c(
  names(outcome_summary_cleaned %>%
          select(-outcome_year))
)

# Verify the outcome types are in the data
print(names(outcome_summary_cleaned))
print(outcome_types)

# Step 3: Merge intake and outcome summaries
full_summary <- intake_summary_cleaned %>%
  full_join(
    outcome_summary_cleaned, 
    by = c("intake_year" = "outcome_year")
  ) %>%
  # Replace NA with 0
  mutate(across(everything(), ~replace_na(., 0))) %>%
  # Verify totals
  mutate(
    total_intakes = rowSums(across(all_of(intake_types))),
    total_outcomes = rowSums(across(all_of(outcome_types)))
  )

# Prepare intake data for plotting
intake_plot_data <- full_summary %>%
  select(intake_year, 
         c(intake_types)
  ) %>%
  pivot_longer(cols = -intake_year, names_to = "intake_type", values_to = "count") %>%
  mutate(intake_type = factor(intake_type, 
                              levels = c(intake_types)))

# Prepare outcome data for plotting
outcome_plot_data <- full_summary %>%
  select(intake_year, 
         c(outcome_types)
  ) %>%
  pivot_longer(cols = -intake_year, names_to = "outcome_type", values_to = "count") %>%
  mutate(outcome_type = factor(outcome_type, 
                               levels = c(outcome_types)))

# Add a custom font
font_add_google("Roboto", "roboto")
showtext_auto()

# Function to clean up labels
clean_labels <- function(x) {
  x %>%
    str_replace_all("_", " ") %>%
    str_to_title()
}

# Prepare intake data
intake_waffle_data <- intake_plot_data %>%
  group_by(intake_year, intake_type) %>%
  summarise(n = sum(count), .groups = 'drop') %>%
  mutate(
    clean_type = str_to_title(str_replace_all(intake_type, "_", " ")),
    # Scale so each square represents 5 units
    scaled_n = ceiling(n / 5)
  )

# Create intake waffle plot
intake_waffle_plot <- ggplot(intake_waffle_data, aes(fill = clean_type, values = scaled_n)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 10, flip = TRUE) +
  facet_wrap(~intake_year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, 
    expand = c(0,0)
  ) +
  scale_fill_manual(values = met.brewer("Egypt", n = length(unique(intake_waffle_data$clean_type))),
                    guide = guide_legend(ncol = 2)) +
  coord_equal() +
  labs(
    title = "Pet Intakes by Type",
    subtitle = "Yearly Distribution (Each square = 5 units)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.text = element_text(
      size = 13,
      face = 'plain',
      color = "grey10"
    )
  )

# Prepare outcome data
outcome_waffle_data <- outcome_plot_data %>%
  group_by(intake_year, outcome_type) %>%
  summarise(n = sum(count), .groups = 'drop') %>%
  mutate(
    clean_type = str_to_title(str_replace_all(outcome_type, "_", " ")),
    # Scale so each square represents 5 units
    scaled_n = ceiling(n / 5)
  )

# Create outcome waffle plot
outcome_waffle_plot <- ggplot(outcome_waffle_data, aes(fill = clean_type, values = scaled_n)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 10, flip = TRUE) +
  facet_wrap(~intake_year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, 
    expand = c(0,0)
  ) +
  scale_fill_manual(values = met.brewer("Signac", n = length(unique(outcome_waffle_data$clean_type))),
                    guide = guide_legend(ncol = 2)) +
  coord_equal() +
  labs(
    title = "Pet Outcomes by Type",
    subtitle = "Yearly Distribution (Each square = 5 units)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.text = element_text(
      size = 13,
      face = 'plain',
      color = "grey10"
    )
  )

# Print plots
intake_waffle_plot +
  outcome_waffle_plot + plot_layout(ncol = 1, guides = "collect")

# =========================================================
# 2. Types of pets adopted ####
# =========================================================

# Analyze adoptions by animal type - count
adoption_by_type <- longbeach %>%
  # Filter only adopted animals
  filter(outcome_type == "adoption") %>%
  # Group by year and animal type
  group_by(intake_year = year(intake_date), animal_type) %>%
  summarize(
    adoption_count = n(),
    .groups = 'drop'
  ) %>%
  # Ensure all animal types are included
  complete(intake_year, animal_type, fill = list(adoption_count = 0)) %>%
  # Scale so each square represents 5 units
  mutate(scaled_n = ceiling(adoption_count / 5))

# Waffle plot for adoption counts
adoption_count_waffle <- ggplot(adoption_by_type, aes(fill = animal_type, values = scaled_n)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 10, flip = TRUE) +
  facet_wrap(~intake_year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) x * 10, 
    expand = c(0,0)
  ) +
  scale_fill_manual(
    values = met.brewer("Archambault", n = length(unique(adoption_by_type$animal_type))),
    guide = guide_legend(ncol = 2)
  ) +
  coord_equal() +
  labs(
    title = "Adoptions by Animal Type",
    subtitle = "Yearly Distribution (Each square = 5 units)"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.text = element_text(
      size = 13,
      face = 'plain',
      color = "grey10"
    )
  )

# Percentage waffle plot (no scaling needed)
adoption_by_type_percentage <- longbeach %>%
  # Filter only adopted animals
  filter(outcome_type == "adoption") %>%
  # Group by year and animal type
  group_by(intake_year = year(intake_date), animal_type) %>%
  summarize(
    adoption_count = n(),
    .groups = 'drop'
  ) %>%
  # Calculate percentage within each year
  group_by(intake_year) %>%
  mutate(
    percentage = adoption_count / sum(adoption_count) * 100
  ) %>%
  ungroup()

# Waffle plot for adoption percentages
adoption_percentage_waffle <- ggplot(adoption_by_type_percentage, aes(fill = animal_type, values = percentage)) +
  geom_waffle(color = "white", size = 0.25, n_rows = 10, flip = TRUE) +
  facet_wrap(~intake_year, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = c(0,0)
  ) +
  scale_fill_manual(
    values = met.brewer("Archambault", n = length(unique(adoption_by_type_percentage$animal_type))),
    guide = guide_legend(ncol = 2)
  ) +
  coord_equal() +
  labs(
    title = "Adoptions by Animal Type (Percentage)",
    subtitle = "Yearly Distribution"
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "right",
    legend.title = element_blank(),
    legend.spacing = unit(0.5, 'cm'),
    legend.key.height = unit(0.5, 'cm'),
    legend.key.width = unit(0.7, 'cm'),
    legend.text = element_text(
      size = 13,
      face = 'plain',
      color = "grey10"
    )
  )

# Calculate total adoptions by animal type
total_adoptions_by_type <- longbeach %>%
  filter(outcome_type == "adoption") %>%
  group_by(animal_type) %>%
  summarize(
    total_adoptions = n(),
    percentage = n() / nrow(longbeach[longbeach$outcome_type == "adoption",]) * 100
  ) %>%
  arrange(desc(total_adoptions))

# Print the plots and summaries
print(adoption_plot)
print(total_adoptions_by_type)

# Print plots
patched_plots <- intake_waffle_plot + theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
                    plot.title = element_blank()) +
  outcome_waffle_plot + theme(plot.title = element_blank(), plot.subtitle = element_blank()) +
  adoption_count_waffle + theme(plot.title = element_blank(), plot.subtitle = element_blank()) +
  plot_layout(ncol = 1)

patched_plots <- intake_waffle_plot + 
  outcome_waffle_plot + 
  plot_layout(nrow = 1)

ggsave(paste0(here::here("outputs", "2025", "Week9_plot1.png")),
       plot = patched_plots,
       height = 20,
       width = 40,
       units = "cm",
       scale = 1.2)
