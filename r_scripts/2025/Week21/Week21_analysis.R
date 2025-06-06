############################################################
# Title: Tidy Tuesday - D&D Monsters Dataset
# Author: Tom Williams  
# Date: 2025-06-06
# Description: TidyTuesday analysis of D&D Monsters
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor", "waffle", 
          "showtext", "fmsb", "scales", "ggrepel", "viridis", "here")

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Running on R version:", R.version.string, "\n")

# =========================================================
# Load TidyTuesday data ####
# =========================================================
# Load D&D Monsters data from Week 21 2025
tuesdata <- tidytuesdayR::tt_load('2025-05-27')
monsters <- tuesdata$monsters

# Alternatively, read directly from GitHub
# monsters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-27/monsters.csv')

# =========================================================
# Explore the data ####
# =========================================================
str(monsters)
glimpse(monsters)
summary(monsters)

# Check unique values for key categorical variables
monsters %>% 
  summarise(
    n_monsters = n(),
    n_categories = n_distinct(category),
    n_types = n_distinct(type),
    n_sizes = n_distinct(size),
    n_alignments = n_distinct(alignment),
    cr_range = paste(min(cr, na.rm = TRUE), "to", max(cr, na.rm = TRUE))
  )

# =========================================================
# Data cleaning & transformation ####
# =========================================================
clean_monsters <- monsters %>%
  janitor::clean_names() %>%
  # Create challenge rating categories
  mutate(
    cr_category = case_when(
      cr <= 0.25 ~ "Trivial (≤0.25)",
      cr <= 1 ~ "Easy (0.5-1)",
      cr <= 5 ~ "Medium (2-5)", 
      cr <= 10 ~ "Hard (6-10)",
      cr <= 15 ~ "Deadly (11-15)",
      cr > 15 ~ "Legendary (16+)",
      TRUE ~ "Unknown"
    ),
    cr_category = factor(cr_category, levels = c("Trivial (≤0.25)", "Easy (0.5-1)", 
                                                 "Medium (2-5)", "Hard (6-10)", 
                                                 "Deadly (11-15)", "Legendary (16+)")),
    # Simplify alignment into broader categories
    alignment_simple = case_when(
      str_detect(alignment, "Good") ~ "Good",
      str_detect(alignment, "Evil") ~ "Evil", 
      str_detect(alignment, "Neutral") & !str_detect(alignment, "Good|Evil") ~ "Neutral",
      alignment == "Unaligned" ~ "Unaligned",
      TRUE ~ "Other"
    ),
    # Create size categories with order
    size_ordered = factor(size, levels = c("Tiny", "Small", "Medium or Small", "Medium", "Large", "Huge", "Gargantuan")),
    # Calculate ability score modifiers (D&D 5e formula)
    str_mod = floor((str - 10) / 2),
    dex_mod = floor((dex - 10) / 2),
    con_mod = floor((con - 10) / 2),
    int_mod = floor((int - 10) / 2),
    wis_mod = floor((wis - 10) / 2),
    cha_mod = floor((cha - 10) / 2),
    # Total ability scores
    total_abilities = str + dex + con + int + wis + cha,
    # Create HP per CR ratio to find efficient monsters
    hp_per_cr = ifelse(cr > 0, hp_number / cr, hp_number)
  ) %>%
  # Remove monsters with missing key data
  filter(!is.na(cr), !is.na(hp_number), !is.na(size))

# =========================================================
# Analysis & Visualization ####
# =========================================================

# Add fonts
showtext::showtext_auto()

# Custom theme
theme_dnd <- function() {
  theme_classic(base_family = "sans") +
    theme(
      plot.title = element_text(size = 32, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 24, color = "grey40"),
      plot.caption = element_text(size = 18, color = "grey50"),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 13),
      legend.text = element_text(size = 13),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# 1. Challenge Rating Distribution by Monster Type
plot1 <- clean_monsters %>%
  count(type, cr_category) %>%
  group_by(type) %>%
  mutate(
    total = sum(n),
    pct = n / total
  ) %>%
  filter(total >= 5) %>%  # Only show types with 5+ monsters
  ggplot(aes(x = reorder(type, total), y = n, fill = cr_category)) +
  geom_col(position = "stack") +
  coord_flip() +
  scale_fill_manual(values = MetBrewer::met.brewer("Signac", 6)) +
  labs(
    title = "Monster Types by Challenge Rating",
    subtitle = "Distribution of difficulty levels across different monster types",
    x = "Monster Type",
    y = "Number of Monsters",
    fill = "Challenge Rating"
  ) +
  theme_dnd() +
  theme(
    legend.position = c(0.98, 0.02),  # Bottom right corner (x=0.98, y=0.02)
    legend.justification = c(1, 0),   # Anchor legend at bottom-right of legend box
    legend.background = element_rect(fill = "white", color = "grey80", linewidth = 0.5),
    legend.margin = margin(t = 5, r = 5, b = 5, l = 5)
  )


# 2. Ability Scores Spider Plot
# Prepare data for spider plot - average ability scores by monster type
spider_data <- clean_monsters %>%
  filter(type %in% c("Humanoid", "Beast", "Dragon", "Fiend", "Undead", "Celestial")) %>%
  group_by(type) %>%
  summarise(
    STR = mean(str, na.rm = TRUE),
    DEX = mean(dex, na.rm = TRUE), 
    CON = mean(con, na.rm = TRUE),
    INT = mean(int, na.rm = TRUE),
    WIS = mean(wis, na.rm = TRUE),
    CHA = mean(cha, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Scale to 0-20 range for spider plot
  mutate(across(STR:CHA, ~pmax(0, pmin(20, .))))

# Convert to long format for ggplot
spider_long <- spider_data %>%
  pivot_longer(cols = STR:CHA, names_to = "ability", values_to = "score") %>%
  mutate(
    ability = factor(ability, levels = c("STR", "DEX", "CON", "INT", "WIS", "CHA")),
    # Convert to polar coordinates
    angle = as.numeric(ability) * 2 * pi / 6,
    x = score * cos(angle - pi/2),
    y = score * sin(angle - pi/2)
  )

# Create spider plot using ggplot2
plot2 <- spider_long %>%
  ggplot(aes(x = x, y = y, color = type, group = type)) +
  # Add grid lines
  geom_polygon(data = data.frame(
    x = rep(c(5, 10, 15, 20), each = 6) * cos(seq(0, 5) * 2 * pi / 6 - pi/2),
    y = rep(c(5, 10, 15, 20), each = 6) * sin(seq(0, 5) * 2 * pi / 6 - pi/2),
    grid = rep(c(5, 10, 15, 20), each = 6)
  ), aes(x = x, y = y, group = grid), 
  fill = NA, color = "grey80", alpha = 0.5, inherit.aes = FALSE) +
  # Add axis lines
  geom_segment(data = data.frame(
    x = 0, y = 0,
    xend = 20 * cos(seq(0, 5) * 2 * pi / 6 - pi/2),
    yend = 20 * sin(seq(0, 5) * 2 * pi / 6 - pi/2)
  ), aes(x = x, y = y, xend = xend, yend = yend),
  color = "grey60", inherit.aes = FALSE) +
  # Add the actual data
  geom_polygon(aes(fill = type), alpha = 0.05, size = 1) +
  geom_point(size = 2) +
  # Add ability labels
  geom_text(data = data.frame(
    x = 22 * cos(seq(0, 5) * 2 * pi / 6 - pi/2),
    y = 22 * sin(seq(0, 5) * 2 * pi / 6 - pi/2),
    label = c("STR", "DEX", "CON", "INT", "WIS", "CHA")
  ), aes(x = x, y = y, label = label), 
  inherit.aes = FALSE, fontface = "bold", size = 5.5) +
  scale_color_manual(values = MetBrewer::met.brewer("Signac", 6)) +
  scale_fill_manual(values = MetBrewer::met.brewer("Signac", 6)) +
  coord_fixed() +
  theme_void() +
  theme(
    # Move legend to right margin
    legend.position = c(0.98, 0.50),
    legend.justification = c(0, 0.5),
    legend.margin = margin(l = 20),  # Add left margin to legend
    plot.title = element_text(hjust = 0.5,  size = 32, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 24, color = "grey40"),
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    # Add plot margins to prevent text cutoff
    plot.margin = margin(t = 10, r = 40, b = 10, l = 10)  # Extra right margin for legend
  ) +
  labs(
    title = "Average Ability Scores by Monster Type",
    subtitle = "Radar chart showing the 'personality' of different monster types"
  )

# 3. Size vs Hit Points relationship
plot3 <- clean_monsters %>%
  ggplot(aes(x = size_ordered, y = hp_number, fill = size_ordered)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Archambault", 7)) +
  scale_y_log10(labels = comma_format()) +
  labs(
    title = "Monster Size vs Hit Points",
    subtitle = "Larger monsters tend to have more hit points (log scale)",
    x = "Monster Size",
    y = "Hit Points (log scale)",
    fill = "Size"
  ) +
  theme_dnd() +
  theme(legend.position = "none")

# 4. Alignment distribution
plot4 <- clean_monsters %>%
  count(alignment_simple, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    alignment_simple = fct_reorder(alignment_simple, n)
  ) %>%
  ggplot(aes(x = alignment_simple, y = pct, fill = alignment_simple)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Benedictus", 5)) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Monster Alignment Distribution", 
    subtitle = "Most D&D monsters, if aligned, tend toward evil",
    x = "Alignment",
    y = "Percentage of Monsters",
    fill = "Alignment"
  ) +
  theme_dnd() +
  theme(legend.position = "none")

# 5. Challenge Rating vs Total Ability Scores
plot5 <- clean_monsters %>%
  filter(cr <= 20) %>%  # Focus on more common CRs
  ggplot(aes(x = cr, y = total_abilities)) +
  geom_point(alpha = 0.6, color = MetBrewer::met.brewer("Signac")[4]) +
  geom_smooth(method = "loess", se = TRUE, color = MetBrewer::met.brewer("Signac")[2], 
              fill = MetBrewer::met.brewer("Signac")[2], alpha = 0.2) +
  labs(
    title = "Challenge Rating vs Total Ability Scores",
    subtitle = "Higher CR monsters generally have higher ability scores",
    x = "Challenge Rating",
    y = "Total Ability Scores (Sum of all 6 abilities)"
  ) +
  theme_dnd()

# 6. Top monster categories by count
plot6 <- clean_monsters %>%
  count(type, sort = TRUE) %>%
  slice_head(n = 12) %>%
  mutate(type = fct_reorder(type, n)) %>%
  ggplot(aes(x = type, y = n, fill = type)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Johnson", 12)) +
  coord_flip() +
  labs(
    title = "Most Common Monster Categories",
    subtitle = "Top 12 monster categories in the D&D SRD",
    x = "Monster Category", 
    y = "Number of Monsters",
    fill = "Category"
  ) +
  theme_dnd() +
  theme(legend.position = "none")

# =========================================================
# Create Dashboard Layout ####
# =========================================================

# Create the main dashboard
dashboard <- plot1 + plot2 + plot3 + plot4 + plot5 + plot6 +
  plot_layout(
    ncol = 2, 
    heights = c(1, 1, 1),
    # Add more space between plots
    widths = c(1, 1.2)  # Give plot2 column slightly more width
  ) +
  plot_annotation(
    title = "Dungeons & Dragons Monster Analysis Dashboard",
    subtitle = "Exploring the creatures of the System Reference Document 5.2.1",
    caption = "Data: D&D SRD 5.2.1 via TidyTuesday Week 21 2025 | Analysis: @tjw-benth",
    theme = theme(
      plot.title = element_text(size = 40, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 24, color = "grey40"),
      plot.caption = element_text(size = 20, color = "grey50")
    )
  )

# =========================================================
# Save final plots ####
# =========================================================

# Create output directory if it doesn't exist
if (!dir.exists(here::here("outputs"))) dir.create(here::here("outputs"))
if (!dir.exists(here::here("outputs", "2025"))) dir.create(here::here("outputs", "2025"))

# Save main dashboard (following template format)
ggsave(paste0(here::here("outputs", "2025", "Week21_DnD_Dashboard.png")),
       plot = dashboard,
       height = 16,
       width = 20,  
       units = "cm",
       dpi = 300,
       scale = 1.4)

# Save individual plots
ggsave(paste0(here::here("outputs", "2025", "Week21_CR_Distribution.png")),
       plot = plot1, 
       height = 10,
       width = 15,
       units = "cm",
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week21_Size_HP.png")),
       plot = plot3,
       height = 10,
       width = 15,
       units = "cm",
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week21_Alignment.png")),
       plot = plot4,
       height = 10,
       width = 10,
       units = "cm",
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week21_CR_Abilities.png")),
       plot = plot5,
       height = 10,
       width = 15,
       units = "cm",
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week21_CR_Spiderplot.png")),
       plot = plot2,
       height = 10,
       width = 15,
       units = "cm",
       scale = 1.2)



# =========================================================
# Summary Statistics ####
# =========================================================

cat("\n=== D&D MONSTERS ANALYSIS SUMMARY ===\n")
cat("Total Monsters Analyzed:", nrow(clean_monsters), "\n")
cat("Challenge Rating Range:", min(clean_monsters$cr), "to", max(clean_monsters$cr), "\n")
cat("Most Common Monster Type:", clean_monsters %>% count(type, sort = TRUE) %>% slice(1) %>% pull(type), "\n")
cat("Average Hit Points:", round(mean(clean_monsters$hp_number, na.rm = TRUE), 1), "\n")
cat("Most Dangerous Alignment:", clean_monsters %>% count(alignment_simple, sort = TRUE) %>% slice(1) %>% pull(alignment_simple), "\n")

# Show some interesting findings
cat("\n=== INTERESTING FINDINGS ===\n")

# Highest HP per CR monsters (most efficient)
efficient_monsters <- clean_monsters %>%
  filter(cr >= 1, cr <= 10) %>%  # Focus on mid-range
  arrange(desc(hp_per_cr)) %>%
  slice_head(n = 5) %>%
  select(name, type, cr, hp_number, hp_per_cr)

cat("Most HP-efficient monsters (HP per CR ratio):\n")
print(efficient_monsters)

# Extreme ability score monsters
cat("\nMonsters with extreme ability scores:\n")
extreme_monsters <- clean_monsters %>%
  filter(str >= 25 | dex >= 25 | con >= 25 | int >= 25 | wis >= 25 | cha >= 25) %>%
  select(name, type, cr, str, dex, con, int, wis, cha) %>%
  arrange(desc(cr))

print(extreme_monsters)


