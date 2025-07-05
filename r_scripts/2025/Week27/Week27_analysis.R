############################################################
# Title: Tidy Tuesday - XKCD color Survey
# Author: Tom Williams
# Date: July 8, 2025
# Description: TidyTuesday analysis of XKCD color Survey data exploring color perception and naming patterns
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggbeeswarm", "ggtext", "ggalluvial", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "treemap", "treemapify", "janitor", "waffle", 
          "showtext", "fmsb", "scales", "ggrepel", "viridis", "here", "wordcloud2")

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Running on R version:", R.version.string, "\n")

# =========================================================
# Load TidyTuesday data ####
# =========================================================
# Load data from TidyTuesday Week 27 2025
tuesdata <- tidytuesdayR::tt_load('2025-07-08')
answers <- tuesdata$answers
color_ranks <- tuesdata$color_ranks
users <- tuesdata$users

# Alternatively, read directly from GitHub
# answers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/answers.csv')
# color_ranks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/color_ranks.csv')
# users <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-08/users.csv')

# =========================================================
# Explore the data ####
# =========================================================
# Quick structure overview for all datasets
str(answers)
str(color_ranks)
str(users)

glimpse(answers)
glimpse(color_ranks)
glimpse(users)

# Check data dimensions
cat("Dataset dimensions:\n")
cat("Answers:", nrow(answers), "rows\n")
cat("color ranks:", nrow(color_ranks), "rows\n")
cat("Users:", nrow(users), "rows\n")

# Initial summary for key dataset
summary(answers)
summary(users)

# Check unique values for key categorical variables
color_ranks %>% 
  summarise(
    n_colors = n_distinct(color),
    n_hex_codes = n_distinct(hex),
    rank_range = paste(min(rank), "to", max(rank))
  )

users %>%
  summarise(
    n_users = n_distinct(user_id),
    n_monitors = n_distinct(monitor, na.rm = TRUE),
    pct_y_chromosome = mean(y_chromosome, na.rm = TRUE) * 100,
    pct_colorblind = mean(colorblind, na.rm = TRUE) * 100,
    mean_spam_prob = mean(spam_prob, na.rm = TRUE)
  )

# =========================================================
# Data cleaning & transformation ####
# =========================================================

# Create main analysis dataset by joining all tables
main_df <- answers %>%
  janitor::clean_names() %>%
  left_join(color_ranks %>% select(rank, color, canonical_hex = hex), by = "rank") %>%
  left_join(users %>% 
              mutate(
                user_type = case_when(
                  spam_prob > 0.5 ~ "Likely Spam",
                  spam_prob > 0.2 ~ "Possible Spam",
                  TRUE ~ "Regular User"
                ),
                monitor_clean = case_when(
                  is.na(monitor) ~ "Unknown",
                  str_detect(monitor, "LCD") ~ "LCD",
                  str_detect(monitor, "CRT") ~ "CRT",
                  str_detect(monitor, "laptop") ~ "Laptop",
                  TRUE ~ "Other"
                ),
                vision_profile = case_when(
                  colorblind == 1 ~ "colorblind",
                  y_chromosome == 1 & colorblind == 0 ~ "Male, Not colorblind",
                  y_chromosome == 0 & colorblind == 0 ~ "Female, Not colorblind",
                  TRUE ~ "Unknown"
                )
              ), 
            by = "user_id") %>%
  filter(!is.na(color)) %>%  # Remove rows where color name couldn't be matched
  rename(actual_hex = hex) %>%  # Rename to be clear this is the actual color shown
  mutate(
    # color category groupings
    color_category = case_when(
      str_detect(color, "red|pink|crimson|scarlet|maroon") ~ "Red Family",
      str_detect(color, "blue|navy|teal|cyan|azure") ~ "Blue Family",
      str_detect(color, "green|lime|olive|forest") ~ "Green Family",
      str_detect(color, "yellow|gold|orange|amber") ~ "Yellow/Orange Family",
      str_detect(color, "purple|violet|magenta|lavender") ~ "Purple Family",
      str_detect(color, "brown|tan|beige|khaki") ~ "Brown Family",
      str_detect(color, "black|white|grey|gray|silver") ~ "Neutral Family",
      TRUE ~ "Other"
    ),
    
    # Accuracy measure (lower rank = more accurate)
    accuracy_tier = case_when(
      rank <= 1 ~ "Perfect Match",
      rank <= 3 ~ "Very Good",
      rank <= 5 ~ "Good",
      TRUE ~ "Poor"
    ),
    
    # Clean user categories
    user_category = case_when(
      user_type == "Likely Spam" ~ "Spam",
      colorblind == 1 ~ "colorblind",
      y_chromosome == 1 ~ "Male",
      y_chromosome == 0 ~ "Female",
      TRUE ~ "Unknown"
    )
  )

# Create summary datasets for specific analyses
color_popularity <- main_df %>%
  filter(user_type != "Likely Spam") %>%  # Exclude spam users
  count(color, color_category, sort = TRUE) %>%
  slice_head(n = 20) %>%
  # Get a representative hex color for each color name from the canonical colors
  left_join(color_ranks %>% select(color, canonical_hex = hex), by = "color") %>%
  mutate(
    color_short = case_when(
      str_length(color) > 12 ~ str_trunc(color, 12),
      TRUE ~ color
    ),
    # Use the canonical hex color for the color name
    display_hex = canonical_hex
  )

# User accuracy analysis
user_accuracy <- main_df %>%
  filter(user_type != "Likely Spam") %>%
  group_by(user_category, vision_profile) %>%
  summarise(
    n_responses = n(),
    avg_rank = mean(rank, na.rm = TRUE),
    pct_perfect = mean(rank == 1, na.rm = TRUE) * 100,
    pct_good = mean(rank <= 3, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  filter(n_responses >= 10) %>%  # Only include groups with sufficient responses
  mutate(
    accuracy_score = 6 - avg_rank,  # Higher score = better accuracy
    category_short = case_when(
      str_length(user_category) > 10 ~ str_trunc(user_category, 10),
      TRUE ~ user_category
    )
  )

# Monitor type analysis
monitor_analysis <- main_df %>%
  filter(user_type != "Likely Spam") %>%
  group_by(monitor_clean) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  count(monitor_clean, rank) %>%
  group_by(monitor_clean) %>%
  mutate(
    total = sum(n),
    prop = n / total,
    pct = prop * 100
  ) %>%
  ungroup() %>%
  mutate(
    monitor_clean = fct_reorder(monitor_clean, rank, .fun = function(x) mean(as.numeric(x)), .desc = FALSE),
    rank_factor = factor(rank, levels = 1:5)
  )


# =========================================================
# Analysis & Visualization ####
# =========================================================

# Add fonts
showtext::showtext_auto()

# Custom theme with improved sizing for dashboard layouts
theme_custom <- function() {
  theme_classic(base_family = "sans") +
    theme(
      plot.title = element_text(size = 28, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 20, color = "grey40"),
      plot.caption = element_text(size = 14, color = "grey50"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# 1. Most Popular Colour Names
plot1 <- color_popularity %>%
  slice_head(n = 15) %>%
  ggplot(aes(area = n, fill = display_hex, label = color_short)) +
  geom_treemap(color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "centre", size = 20, fontface = "bold") +
  scale_fill_identity() +
  labs(title = "Most Popular Colour Names", 
       subtitle = "Size = frequency") +
  theme_custom()



# 2. User Accuracy by Demographics
plot2 <- user_accuracy %>%
  filter(user_category %in% c("Male", "Female", "colorblind")) %>%
  ggplot(aes(x = user_category, y = pct_perfect, fill = user_category)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = c("grey", MetBrewer::met.brewer("Benedictus", 2))) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(
    title = "Colour Naming Accuracy by User Type",
    subtitle = "Percentage of perfect matches (rank 1 answers)",
    x = "User Category",
    y = "Perfect Match Rate (%)"
  ) +
  theme_custom()

# 3. Monitor Type Performance
plot3 <- monitor_analysis %>%
  ggplot(aes(x = rank_factor, y = monitor_clean, fill = pct)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_gradient2(
    low = "white",
    high = "darkblue",
    name = "% of\nResponses",
    labels = function(x) paste0(x, "%")
  ) +
  geom_text(
    aes(label = sprintf("%.1f%%", pct)),
    color = "white",
    size = 6
  ) +
  scale_x_discrete(
    labels = c("1\n(Best)", "2", "3", "4", "5\n(Worst)")
  ) +
  labs(
    title = "Colour Naming Performance by Monitor Type",
    subtitle = "Percentage of responses (colour) at each rank level",
    x = "Rank",
    y = "Monitor Type"
  ) +
  theme_custom() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.grid = element_blank(),
    legend.position = "none"
  )

# 4. Colour Family Distribution
color_family <- main_df %>%
  filter(user_type != "Likely Spam") %>%
  count(color_category, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    hex_color = case_when(
      color_category == "Green Family" ~ "#228B22",
      color_category == "Blue Family" ~ "#4169E1", 
      color_category == "Purple Family" ~ "#8A2BE2",
      color_category == "Red Family" ~ "#DC143C",
      color_category == "Brown Family" ~ "#8B4513",
      color_category == "Yellow/Orange Family" ~ "#FF8C00",
      color_category == "Neutral Family" ~ "#696969",
      color_category == "Other" ~ "#2F4F4F",
      TRUE ~ "#000000"  # fallback
    ))

color_mapping <- setNames(color_family$hex_color, color_family$color_category)

plot4 <- color_family %>%
  ggplot(aes(x = 2, y = pct, fill = color_category)) +
  geom_col(width = 1, color = "white", size = 1) +
  coord_polar(theta = "y", start = 0) +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = color_mapping, name = "Colour Family") +
  guides(fill = guide_legend(ncol = 1, keywidth = 1, keyheight = 1)) +
  labs(title = "Colour Family Distribution") +
  theme_custom() +
  theme_void() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20, color = "grey40"),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20, "pt")
  )

# 5. Spam User Analysis
plot5 <- users %>%
  mutate(spam_category = case_when(
    spam_prob > 0.8 ~ "Very Likely Spam",
    spam_prob > 0.5 ~ "Likely Spam", 
    spam_prob > 0.2 ~ "Possible Spam",
    TRUE ~ "Regular User"
  )) %>%
  count(spam_category) %>%
  mutate(pct = round(n / sum(n) * 100)) %>%
  ggplot(aes(fill = spam_category, values = pct)) +
  geom_waffle(n_rows = 10, size = 0.33, color = "white") +
  scale_fill_manual(values = MetBrewer::met.brewer("VanGogh3", 4), 
                    name = "Category") +
  coord_equal() +
  labs(title = "User Spam Classification",
       subtitle = "Each square = 1% of users") +
  theme_custom() +
  theme_void() +
  theme(
    plot.title = element_text(size = 28, face = "bold"),
    plot.subtitle = element_text(size = 20, color = "grey40"),
    legend.position = "right",
    plot.margin = margin(20, 20, 20, 20, "pt")
  )

# 6. Accuracy vs Demographics Network
# Define colours for demographics and accuracy
demo_colors <- c("Colorblind" = "grey", "Male" = MetBrewer::met.brewer("Benedictus", 2)[2], "Female" = MetBrewer::met.brewer("Benedictus", 2)[1])

accuracy_colors <- c(
  "Perfect Match" = "#2E8B57",    # Sea Green (excellent)
  "Very Good" = "#4682B4",        # Steel Blue (good)
  "Good" = "#CD853F"              # Peru (satisfactory)
)

plot6 <- main_df %>%
  filter(user_type != "Likely Spam", !is.na(y_chromosome)) %>%
  mutate(demo_group = case_when(
    colorblind == 1 ~ "Colorblind",
    y_chromosome == 1 ~ "Male", 
    y_chromosome == 0 ~ "Female"
  )) %>%
  count(demo_group, accuracy_tier) %>%
  mutate(
    # Set the factor levels to match the order in accuracy_colors
    accuracy_tier = factor(accuracy_tier, levels = names(accuracy_colors))
  ) %>%
  ggplot(aes(y = n, axis1 = demo_group, axis2 = accuracy_tier)) +
  geom_alluvium(aes(fill = demo_group), width = 1/12) +
  geom_stratum(width = 1/12, color = "white", size = 0.5) +
  geom_stratum(aes(fill = after_stat(stratum)), width = 1/12, alpha = .4) +  
  geom_label(stat = "stratum", aes(label = after_stat(stratum)), 
             fill = "white", color = "black", size = 4) +
  scale_fill_manual(values = c(demo_colors, accuracy_colors)) +
  labs(title = "Flow from Demographics to Accuracy") +
  theme_custom() +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(size = 28, face = "bold"))

# =========================================================
# Create Dashboard Layout ####
# =========================================================

# Create a comprehensive dashboard with improved layout
dashboard <- (plot1 + plot4) / (plot3 + plot5) / (plot6) +
  plot_layout(
    heights = c(1, 1, 1)
  ) +
  plot_annotation(
    title = "XKCD Colour Survey Analysis Dashboard",
    subtitle = "Exploring colour perception and naming patterns across different user groups",
    caption = "Data: XKCD colour survey via TidyTuesday Week 27 2025 | Analysis: @tjw-benth",
    theme = theme(
      plot.title = element_text(size = 40, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 24, color = "grey40"),
      plot.caption = element_text(size = 20, color = "grey50")
    )
  )

# =========================================================
# Save final plots ####
# =========================================================

# Create output directory structure
if (!dir.exists(here::here("outputs"))) dir.create(here::here("outputs"))
if (!dir.exists(here::here("outputs", "2025"))) dir.create(here::here("outputs", "2025"))

# Save main dashboard with optimized settings
ggsave(paste0(here::here("outputs", "2025", "Week27_XKCD_color_Survey_Dashboard.png")),
       plot = dashboard,
       height = 18,
       width = 14,  
       units = "cm",
       dpi = 300,
       scale = 1.2)

# Save individual plots with consistent sizing
plot_names <- c("popular_colours", "user_accuracy", "monitor_performance", "colour_families", "spam_analysis", "accuracy_breakdown")
plot_list <- list(plot1, plot2, plot3, plot4, plot5, plot6)

for(i in seq_along(plot_list)) {
  ggsave(paste0(here::here("outputs", "2025", ""), "Week27_", plot_names[i], ".png"),
         plot = plot_list[[i]], 
         height = if(i == 6) 12 else 10,
         width = if(i == 6) 18 else 15,
         units = "cm",
         dpi = 300,
         scale = 1.2)
}

# =========================================================
# Summary Statistics ####
# =========================================================

cat("\n=== XKCD COLOUR SURVEY ANALYSIS SUMMARY ===\n")
cat("Total Responses:", nrow(main_df), "\n")
cat("Unique Colours:", n_distinct(main_df$color), "\n")
cat("Total Users:", n_distinct(main_df$user_id), "\n")
cat("Regular Users:", sum(users$spam_prob <= 0.2), "\n")
cat("Likely Spam Users:", sum(users$spam_prob > 0.5), "\n")

# Most popular colour
if(nrow(color_popularity) > 0) {
  top_color <- color_popularity %>% slice(1)
  cat("Most Popular Colour:", top_color$color, "(", top_color$n, "responses)\n")
}

# Best performing demographic
if(nrow(user_accuracy) > 0) {
  best_group <- user_accuracy %>% arrange(desc(pct_perfect)) %>% slice(1)
  cat("Most Accurate Group:", best_group$user_category, "(", round(best_group$pct_perfect, 1), "% perfect matches)\n")
}

cat("\n=== KEY FINDINGS ===\n")

# Colour perception insights
colorblind_accuracy <- user_accuracy %>% filter(str_detect(user_category, "colorblind"))
if(nrow(colorblind_accuracy) > 0) {
  cat("colorblind users perfect match rate:", round(colorblind_accuracy$pct_perfect, 1), "%\n")
}

# Gender differences
gender_comparison <- user_accuracy %>% 
  filter(user_category %in% c("Male", "Female")) %>%
  select(user_category, pct_perfect) %>%
  arrange(desc(pct_perfect))

if(nrow(gender_comparison) >= 2) {
  cat("Gender accuracy difference:", 
      round(gender_comparison$pct_perfect[1] - gender_comparison$pct_perfect[2], 1), 
      "percentage points\n")
}

# Show top colour families
cat("\nTop 5 Colour Families:\n")
top_families <- main_df %>%
  filter(user_type != "Likely Spam") %>%
  count(color_category, sort = TRUE) %>%
  slice_head(n = 5) %>%
  mutate(pct = round(n / sum(main_df$user_type != "Likely Spam") * 100, 1)) %>%
  select(color_category, n, pct)
print(top_families)

# =========================================================
# Additional Analysis Ideas ####
# =========================================================

# Colour difficulty analysis - which colours are hardest to name correctly?
# difficult_colors <- main_df %>%
#   filter(user_type != "Likely Spam") %>%
#   group_by(color, canonical_hex) %>%
#   summarise(
#     n_responses = n(),
#     avg_rank = mean(rank),
#     pct_perfect = mean(rank == 1) * 100,
#     .groups = "drop"
#   ) %>%
#   filter(n_responses >= 5) %>%
#   arrange(desc(avg_rank))

# Consensus analysis - colors where users agree vs disagree
# color_consensus <- main_df %>%
#   filter(user_type != "Likely Spam") %>%
#   group_by(canonical_hex) %>%
#   summarise(
#     n_responses = n(),
#     n_unique_colors = n_distinct(color),
#     most_common_color = names(sort(table(color), decreasing = TRUE))[1],
#     consensus_pct = max(table(color)) / n_responses * 100,
#     .groups = "drop"
#   ) %>%
#   filter(n_responses >= 5) %>%
#   arrange(desc(consensus_pct))
