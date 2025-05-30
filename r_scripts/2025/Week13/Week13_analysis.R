############################################################
# Title: Tidy Tuesday - Pokemon Dataset
# Author: Tom Williams
# Date: 2025-05-30
# Description: TidyTuesday analysis on Pokemon
# Version: R version auto-detected below
############################################################
# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor", "waffle", 
          "showtext", "scales", "viridis", "corrplot", "ggrepel") 
new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Running on R version:", R.version.string, "\n")

# =========================================================
# Load TidyTuesday data ####
# =========================================================
tuesdata <- tidytuesdayR::tt_load('2025-04-01')
## OR
tuesdata <- tidytuesdayR::tt_load(2025, week = 13)
pokemon_df <- tuesdata$pokemon_df

# =========================================================
# Explore the data ####
# =========================================================
str(pokemon_df)
glimpse(pokemon_df)

# =========================================================
# Data cleaning & transformation ####
# =========================================================
clean_df <- pokemon_df %>%
  janitor::clean_names() %>%
  # Create battle effectiveness metrics
  mutate(
    total_stats = hp + attack + defense + special_attack + special_defense + speed,
    offensive_power = attack + special_attack,
    defensive_power = defense + special_defense + hp,
    battle_score = (offensive_power + defensive_power + speed) / 3,
    # Create bulk index (Pokemon BMI equivalent)
    bulk_index = case_when(
      height > 0 ~ weight / (height^2),
      TRUE ~ NA_real_
    ),
    # Clean generation for plotting
    generation = paste("Gen", generation_id),
    # Create dual type indicator
    is_dual_type = !is.na(type_2),
    # Combined type for dual types
    type_combo = case_when(
      is_dual_type ~ paste(type_1, type_2, sep = "/"),
      TRUE ~ type_1
    )
  ) %>%
  # Remove any Pokemon with missing critical stats
  filter(!is.na(hp), !is.na(attack), !is.na(defense))

# =========================================================
# Analysis 1: Type Distribution and Power ####
# =========================================================

# Most common primary types
type_power_summary <- clean_df %>%
  group_by(type_1) %>%
  summarise(
    count = n(),
    avg_total_stats = mean(total_stats, na.rm = TRUE),
    avg_battle_score = mean(battle_score, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(count >= 5) %>%  # Only types with at least 5 Pokemon
  arrange(desc(avg_battle_score))

# =========================================================
# Analysis 2: Physical Characteristics ####
# =========================================================

# Height vs Weight analysis
height_weight_stats <- clean_df %>%
  filter(!is.na(height), !is.na(weight), height > 0, weight > 0) %>%
  mutate(
    size_category = case_when(
      height <= 5 & weight <= 50 ~ "Small",
      height <= 15 & weight <= 500 ~ "Medium", 
      TRUE ~ "Large"
    )
  )

# =========================================================
# Analysis 3: Generation Evolution ####
# =========================================================

generation_trends <- clean_df %>%
  group_by(generation_id) %>%
  filter(!is.na(generation_id)) %>%
  summarise(
    count = n(),
    avg_total_stats = mean(total_stats, na.rm = TRUE),
    avg_base_exp = mean(base_experience, na.rm = TRUE),
    dual_type_pct = mean(is_dual_type, na.rm = TRUE) * 100,
    .groups = 'drop'
  )

# =========================================================
# Visualization 1: Type Power Analysis ####
# =========================================================

# Add font
showtext::font_add_google("Roboto", "roboto")
showtext::showtext_auto()

type_power_plot <- type_power_summary %>%
  mutate(type_1 = fct_reorder(type_1, avg_battle_score)) %>%
  ggplot(aes(x = type_1, y = avg_battle_score)) +
  geom_col(aes(fill = avg_battle_score), width = 0.8) +
  geom_text(aes(label = paste0("n=", count)), 
            hjust = -0.1, size = 3, family = "roboto") +
  coord_flip() +
  scale_fill_gradientn(colors = MetBrewer::met.brewer("Hokusai1", direction = -1)) +
  labs(
    title = "Pokemon Battle Effectiveness by Type",
    subtitle = "Average battle score combining offensive, defensive, and speed stats\nOnly types with 5+ Pokemon shown",
    x = "Primary Type",
    y = "Average Battle Score",
    fill = "Battle\nScore"
  ) +
  theme_classic(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size = 16, color = "grey40"), 
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.position = "right"
  )

# =========================================================
# Visualization 2: Generation Evolution ####
# =========================================================

generation_plot <- generation_trends %>%
  ggplot(aes(x = factor(generation_id))) +
  geom_col(aes(y = count), fill = MetBrewer::met.brewer("Signac")[12], 
           alpha = 0.7, width = 0.6) +
  geom_line(aes(y = avg_total_stats/8, group = 1), 
            color = MetBrewer::met.brewer("Signac")[3], size = 1.5) +
  geom_point(aes(y = avg_total_stats/8), 
             color = MetBrewer::met.brewer("Signac")[3], size = 4) +
  scale_y_continuous(
    name = "Pokemon Count",
    sec.axis = sec_axis(~.*8, name = "Average Total Stats",
                        labels = scales::number_format())
  ) +
  labs(
    title = "Pokemon Count and Power Across Generations",
    subtitle = "Blue bars show count, orange line shows average total stats",
    x = "Generation"
  ) +
  theme_classic(base_family = "roboto") +
  theme(
  plot.title = element_text(size = 20, face = "bold"),  
  plot.subtitle = element_text(size = 16, color = "grey40"),  
  axis.title = element_text(size = 14),  
  axis.text = element_text(size = 12),  
  axis.title.y.right = element_text(color = MetBrewer::met.brewer("Signac")[3], size = 14),
  axis.text.y.right = element_text(color = MetBrewer::met.brewer("Signac")[3], size = 12),
  legend.title = element_text(size = 14), 
  legend.text = element_text(size = 12), 
  legend.position = "right"
)

# =========================================================
# Visualization 3: Height vs Weight with Type Colors ####
# =========================================================

# Define Pokemon type colors based on official Pokemon color scheme
pokemon_type_colors <- c(
  "normal" = "#A8A878",
  "fire" = "#F08030", 
  "water" = "#6890F0",
  "electric" = "#F8D030",
  "grass" = "#78C850",
  "ice" = "#98D8D8",
  "fighting" = "#C03028",
  "poison" = "#A040A0",
  "ground" = "#E0C068",
  "flying" = "#A890F0",
  "psychic" = "#F85888",
  "bug" = "#A8B820",
  "rock" = "#B8A038",
  "ghost" = "#705898",
  "dragon" = "#7038F8",
  "dark" = "#705848",
  "steel" = "#B8B8D0",
  "fairy" = "#EE99AC",
  "other" = "#68A090"
)

# Get top 10 most common types for legend
top_10_types <- height_weight_stats %>%
  count(type_1, sort = TRUE) %>%
  head(10) %>%
  pull(type_1)

# Identify outliers for labeling
size_outliers <- height_weight_stats %>%
  mutate(
    log_height = log10(height),
    log_weight = log10(weight)
  ) %>%
  filter(
    # Top 3 heaviest, top 3 tallest, top 3 lightest for their height, top 3 shortest for their weight
    rank(-weight) <= 3 | 
      rank(-height) <= 3 |
      rank(weight/height) <= 3 |
      rank(height/weight) <= 3
  ) %>%
  distinct(pokemon, .keep_all = TRUE) %>%
  head(8)  # Limit to 8 labels to avoid overcrowding

size_scatter_plot <- height_weight_stats %>%
  mutate(
    type_display = case_when(
      type_1 %in% top_10_types ~ type_1,
      TRUE ~ "Other"
    ),
    type_display = factor(type_display, levels = c(top_10_types, "Other"))
  ) %>%
  ggplot(aes(x = height, y = weight)) +
  geom_point(aes(color = type_display), alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "black", 
              linetype = "dashed", linewidth = 0.8) +
  # Add smart labels with arrows for outliers
  ggrepel::geom_label_repel(data = size_outliers,
                            aes(x = height, y = weight, label = pokemon),
                            arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                            box.padding = 0.5, point.padding = 0.3,
                            segment.color = "black", segment.alpha = 0.7,
                            family = "roboto", fontface = "bold",
                            size = 5, fill = "white", alpha = 0.9) +
  scale_x_log10(labels = scales::number_format(suffix = "m")) +
  scale_y_log10(labels = scales::number_format(suffix = "kg")) +
  scale_color_manual(values = pokemon_type_colors, 
                     breaks = c(top_10_types, "Other")[c(top_10_types, "Other") %in% names(pokemon_type_colors)]) +
  labs(
    title = "Pokemon Size Relationships",
    subtitle = "Height vs Weight on log scale, with extreme size outliers labeled",
    x = "Height (meters, log scale)",
    y = "Weight (kg, log scale)",
    color = "Primary Type"
  ) +
  theme_classic(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size = 16, color = "grey40"), 
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.position = "right"
  )


# =========================================================
# Visualization 4: Dual Type Analysis ####
# =========================================================

dual_type_summary <- clean_df %>%
  filter(is_dual_type) %>%
  count(type_1, type_2, sort = TRUE) %>%
  head(12) %>%
  mutate(
    type_combo = paste(type_1, type_2, sep = " + "),
    type_combo = fct_reorder(type_combo, n)
  )

dual_type_plot <- ggplot(dual_type_summary, aes(x = type_combo, y = n)) +
  geom_col(fill = MetBrewer::met.brewer("Hokusai1")[4], width = 0.7) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.5, family = "roboto") +
  coord_flip() +
  labs(
    title = "Most Common Dual-Type Combinations",
    subtitle = "Top 12 combinations in the dataset",
    x = "Type Combination",
    y = "Number of Pokemon"
  ) +
  theme_classic(base_family = "roboto") +
  theme(
    plot.title = element_text(size = 20, face = "bold"), 
    plot.subtitle = element_text(size = 16, color = "grey40"), 
    axis.title = element_text(size = 14),  
    axis.text = element_text(size = 12),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12),  
    legend.position = "right"
  )

# =========================================================
# Create Combined Dashboard ####
# =========================================================

combined_plot <- (type_power_plot + generation_plot) / 
  (size_scatter_plot + dual_type_plot) +
  plot_annotation(
    title = "Pokemon Dataset Analysis Dashboard",
    subtitle = "TidyTuesday Week 13 2025 | Analysis by Tom",
    theme = theme(
      plot.title = element_text(size = 20, face = "bold", family = "roboto"),
      plot.subtitle = element_text(size = 14, color = "grey40", family = "roboto")
    )
  )

# =========================================================
# Save final plot ####
# =========================================================

ggsave(paste0(here::here("outputs", "2025", "Week13_pokemon_dashboard.png")),
       plot = combined_plot,
       height = 15,
       width = 20,
       units = "cm",
       scale = 1.2,
       dpi = 300)

# Save individual plots as well
ggsave(paste0(here::here("outputs", "2025", "Week13_type_power.png")),
       plot = type_power_plot,
       height = 5,
       width = 7,
       units = "cm",
       scale = 1.2,
       dpi = 300)

ggsave(paste0(here::here("outputs", "2025", "Week13_generation_trends.png")),
       plot = generation_plot,
       height = 5,
       width = 7,
       units = "cm",
       scale = 1.2,
       dpi = 300)

# =========================================================
# Additional Analysis: Summary Statistics ####
# =========================================================

cat("\n=== POKEMON DATASET SUMMARY ===\n")
cat("Total Pokemon:", nrow(clean_df), "\n")
cat("Unique Types:", length(unique(clean_df$type_1)), "\n")
cat("Dual-Type Pokemon:", sum(clean_df$is_dual_type), 
    paste0("(", round(mean(clean_df$is_dual_type)*100, 1), "%)"), "\n")
cat("Generations:", min(clean_df$generation_id), "to", max(clean_df$generation_id), "\n")

# Top 5 strongest Pokemon overall
top_pokemon <- clean_df %>%
  select(pokemon, type_1, total_stats, battle_score, generation_id) %>%
  arrange(desc(total_stats)) %>%
  head(5)

cat("\nTop 5 Pokemon by Total Stats:\n")
print(top_pokemon)

# Most balanced Pokemon (smallest difference between max and min stats)
balanced_pokemon <- clean_df %>%
  rowwise() %>%
  mutate(
    stat_range = max(c(hp, attack, defense, special_attack, special_defense, speed), na.rm = TRUE) - 
      min(c(hp, attack, defense, special_attack, special_defense, speed), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(total_stats >= 400) %>%  # Only reasonably strong Pokemon
  arrange(stat_range) %>%
  select(pokemon, type_1, total_stats, stat_range) %>%
  head(5)

cat("\nTop 5 Most Balanced Pokemon (high stats, low range):\n")
print(balanced_pokemon)

