############################################################
# Title: Tidy Tuesday - Sydney Water Quality Analysis
# Author: Tom Williams
# Date: 2025-05-21
# Description: TidyTuesday analysis 
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "janitor", "showtext", "scico", "viridis") 

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Running on R version:", R.version.string, "\n")

# =========================================================
# Data loading ####
# =========================================================
tuesdata <- tidytuesdayR::tt_load('2025-05-20')
water_quality <- tuesdata$water_quality
weather <- tuesdata$weather

# Alternative: Read directly from GitHub
# water_quality <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/water_quality.csv')
# weather <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-05-20/weather.csv')

# =========================================================
# Explore the data ####
# =========================================================
str(water_quality)
glimpse(water_quality)
str(weather)
glimpse(weather)

# Check spatial extent and data distribution
summary(water_quality$latitude)
summary(water_quality$longitude)
summary(water_quality$enterococci_cfu_100ml)

# =========================================================
# Data cleaning & transformation ####
# =========================================================
clean_water <- water_quality %>%
  janitor::clean_names() %>%
  filter(!is.na(latitude), !is.na(longitude), !is.na(enterococci_cfu_100ml)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      month %in% c("Mar", "Apr", "May") ~ "Autumn",
      month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      TRUE ~ "Spring"
    ),
    # Create log-transformed enterococci for better visualization
    log_enterococci = log10(enterococci_cfu_100ml + 1),
    # Create NHMRC microbial assessment categories based on 95th percentile enterococci levels
    # Note: Full water quality classification requires sanitary inspection data (NHMRC 2008)
    nhmrc_category = case_when(
      enterococci_cfu_100ml <= 40 ~ "A",
      enterococci_cfu_100ml <= 200 ~ "B", 
      enterococci_cfu_100ml <= 500 ~ "C",
      TRUE ~ "D"
    ) %>% factor(levels = c("A", "B", "C", "D")),
    # Simplified quality interpretation (assuming moderate sanitary inspection risk)
    quality_interpretation = case_when(
      nhmrc_category == "A" ~ "Good",
      nhmrc_category == "B" ~ "Good",
      nhmrc_category == "C" ~ "Poor",
      nhmrc_category == "D" ~ "Very Poor"
    ) %>% factor(levels = c("Good", "Poor", "Very Poor"))
  )

# Clean and prepare weather data
clean_weather <- weather %>%
  janitor::clean_names() %>%
  filter(!is.na(date), !is.na(precipitation_mm)) %>%
  mutate(
    year = lubridate::year(date),
    month = lubridate::month(date, label = TRUE),
    season = case_when(
      month %in% c("Dec", "Jan", "Feb") ~ "Summer",
      month %in% c("Mar", "Apr", "May") ~ "Autumn", 
      month %in% c("Jun", "Jul", "Aug") ~ "Winter",
      TRUE ~ "Spring"
    )
  )

# Calculate rainfall metrics for different time periods
weather_with_cumulative <- clean_weather %>%
  group_by(latitude, longitude) %>%
  arrange(date) %>%
  mutate(
    rain_1day = precipitation_mm,
    rain_3day = zoo::rollsum(precipitation_mm, k = 3, fill = NA, align = "right"),
    rain_7day = zoo::rollsum(precipitation_mm, k = 7, fill = NA, align = "right"),
    rain_14day = zoo::rollsum(precipitation_mm, k = 14, fill = NA, align = "right"),
    # Temperature difference (daily range)
    temp_range = max_temp_c - min_temp_c
  ) %>%
  ungroup()

# Merge weather data with water quality data
merged_data <- clean_water %>%
  left_join(
    weather_with_cumulative %>%
      select(date, latitude, longitude, rain_1day, rain_3day, rain_7day, rain_14day,
             max_temp_c, min_temp_c, temp_range),
    by = "date",
    suffix = c("_water", "_weather")
  ) %>%
  # Calculate distance between water site and weather station
  mutate(
    distance_km = sqrt((latitude_water - latitude_weather)^2 + 
                         (longitude_water - longitude_weather)^2) * 111.32
  ) %>%
  # Keep only reasonable matches (within ~50km)
  filter(distance_km <= 50 | is.na(distance_km)) %>%
  # For each water sample, keep the closest weather station
  group_by(swim_site, date) %>%
  slice_min(distance_km, n = 1) %>%
  ungroup()

# Create spatial summary
spatial_summary <- clean_water %>%
  group_by(swim_site, latitude, longitude) %>%
  summarise(
    mean_enterococci = mean(enterococci_cfu_100ml, na.rm = TRUE),
    median_enterococci = median(enterococci_cfu_100ml, na.rm = TRUE),
    max_enterococci = max(enterococci_cfu_100ml, na.rm = TRUE),
    n_samples = n(),
    mean_temp = mean(water_temperature_c, na.rm = TRUE),
    .groups = "drop"
  )

# =========================================================
# Create visualization plots ####
# =========================================================

# Add font
showtext::showtext_auto()

# 1. Heatmap of enterococci levels by quarter
heatmap_plot <- clean_water %>%
  mutate(
    quarter_year = paste0(year, "-", "Q", quarter(date))
  ) %>%
  group_by(swim_site, quarter_year) %>%
  summarise(mean_enterococci = mean(enterococci_cfu_100ml, na.rm = TRUE), .groups = "drop") %>%
  # Only keep sites with sufficient data across quarter-years
  group_by(swim_site) %>%
  filter(n() >= 4) %>%  # At least 4 quarter-year combinations
  ungroup() %>%
  # Add small constant to handle zeros for log transformation
  mutate(mean_enterococci_adj = mean_enterococci + 1) %>%
  ggplot(aes(x = quarter_year, y = fct_reorder(swim_site, mean_enterococci), 
             fill = mean_enterococci_adj)) +
  geom_tile(color = "white", size = 0.3) +
  scale_fill_viridis_c(name = "Mean\nEnterococci + 1\n(CFU/100ml)", 
                       trans = "log10",
                       labels = scales::comma,
                       option = "plasma") +
  labs(
    title = "Sydney Water Quality: Enterococci Levels (log scale) by Year-Quarter",
    subtitle = "Higher values indicate poorer water quality (NHMRC 2008 Standards)",
    x = "Year-Quarter",
    y = "Swimming Site"
  ) +
  theme_classic(base_size = 18) +
  theme(
    axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    panel.grid = element_blank(),
    legend.title = element_text(lineheight = 0.8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 17, face = "bold"),
    plot.subtitle = element_text(size = 15)
  )

# 2. Spatial distribution plot
spatial_plot <- ggplot(spatial_summary, aes(x = longitude, y = latitude)) +
  geom_point(aes(size = mean_temp, color = mean_enterococci), alpha = 0.8) +
  scale_size_continuous(name = "Mean\nTemp (°C)", range = c(2, 6)) +
  scale_color_viridis_c(name = "Mean\nEnterococci\n(CFU/100ml)", 
                        trans = "log10",
                        labels = scales::comma,
                        option = "plasma") +
  guides(size = guide_legend(order = 1),
         color = guide_colorbar(order = 2)) +  
  labs(
    title = "Sydney Swimming Sites: Water Quality Distribution",
    subtitle = "Size represents temperature, color represents mean enterococci levels (log scale)",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "right",
    legend.title = element_text(lineheight = 0.8),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    panel.grid = element_line(color = "grey90", size = 0.3)
  )

# 3. Seasonal variation plot
seasonal_plot <- clean_water %>%
  ggplot(aes(x = season, y = enterococci_cfu_100ml, fill = season)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  scale_y_log10(labels = scales::comma) +
  scale_fill_manual(values = MetBrewer::met.brewer("Signac", 4)) +
  labs(
    title = "Seasonal Variation in Water Quality",
    subtitle = "Enterococci levels by season (log scale)",
    x = "Season",
    y = "Enterococci (CFU/100ml)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    panel.grid.minor = element_blank()
  )

# 4. Water temperature vs enterococci correlation
temp_correlation_plot <- clean_water %>%
  filter(!is.na(water_temperature_c)) %>%
  filter(water_temperature_c < 100) %>% # avoid unrealistic temperatures
  ggplot(aes(x = water_temperature_c, y = enterococci_cfu_100ml+1)) +
  geom_point(alpha = 0.4, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "loess", color = "#F24236", size = 1.3, se = F) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Water Temperature vs Bacterial Contamination",
    subtitle = "Relationship between water temperature and enterococci levels (log scale, +1 transformation)",
    x = "Water Temperature (°C)",
    y = "Enterococci + 1 (CFU/100ml)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    panel.grid.minor = element_blank()
  )

# 5. NHMRC category distribution plot
nhmrc_category_plot <- clean_water %>%
  count(nhmrc_category, quality_interpretation) %>%
  mutate(pct = n/sum(n)*100) %>%
  ggplot(aes(x = nhmrc_category, y = n, fill = quality_interpretation)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(aes(label = paste0(round(pct, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            color = "black", fontface = "bold", size = 10) +
  scale_fill_manual(values = c("Good" = "#2E8B57", "Poor" = "#FF6347", "Very Poor" = "#8B0000"),
                    name = "Quality\nInterpretation") +
  labs(
    title = "NHMRC Water Quality Categories",
    subtitle = "Distribution based on 95th percentile enterococci levels (NHMRC 2008)",
    x = "NHMRC Microbial Assessment Category",
    y = "Number of Samples",
    caption = "Note: Full classification requires sanitary inspection data"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    plot.caption = element_text(size = 11, face = "italic"),
    legend.position = "right",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# 6. Site comparison plot - top 10 worst sites
site_comparison_plot <- spatial_summary %>%
  arrange(desc(mean_enterococci)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(swim_site, mean_enterococci), y = mean_enterococci, 
             fill = mean_enterococci)) +
  geom_col(alpha = 0.8) +
  coord_flip() +
  scale_fill_viridis_c(name = "Mean\nEnterococci", trans = "log10", option = "plasma") +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Top 10 Sites by Mean Enterococci Levels",
    subtitle = "Sites with highest bacterial contamination",
    x = "Swimming Site",
    y = "Mean Enterococci (CFU/100ml)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11)
  )

# =========================================================
# Weather-Bacteria Relationship Analysis ####
# =========================================================

# 7. Rainfall vs Enterococci - Multiple time periods
rainfall_comparison_plot <- merged_data %>%
  filter(!is.na(rain_7day), !is.na(enterococci_cfu_100ml)) %>%
  pivot_longer(cols = c(rain_1day, rain_3day, rain_7day, rain_14day),
               names_to = "rainfall_period", values_to = "rainfall_mm") %>%
  mutate(
    rainfall_period = factor(rainfall_period, 
                             levels = c("rain_1day", "rain_3day", "rain_7day", "rain_14day"),
                             labels = c("1 Day", "3 Days", "7 Days", "14 Days")),
    rainfall_category = case_when(
      rainfall_mm == 0 ~ "No Rain",
      rainfall_mm <= 5 ~ "Light (0-5mm)",
      rainfall_mm <= 20 ~ "Moderate (5-20mm)",
      rainfall_mm <= 50 ~ "Heavy (20-50mm)",
      TRUE ~ "Very Heavy (>50mm)"
    ) %>% factor(levels = c("No Rain", "Light (0-5mm)", "Moderate (5-20mm)", 
                            "Heavy (20-50mm)", "Very Heavy (>50mm)"))
  ) %>%
  ggplot(aes(x = rainfall_category, y = enterococci_cfu_100ml, fill = rainfall_category)) +
  geom_violin(alpha = 0.7, scale = "width") +
  geom_boxplot(width = 0.2, fill = "white", alpha = 0.8) +
  facet_wrap(~ rainfall_period, scales = "free_x") +
  scale_y_log10(labels = scales::comma) +
  scale_fill_viridis_d(option = "viridis", name = "Rainfall\nCategory") +
  labs(
    title = "Impact of Recent Rainfall on Bacterial Contamination",
    subtitle = "Enterococci levels by cumulative rainfall over different time periods",
    x = "Rainfall Category",
    y = "Enterococci (CFU/100ml)",
    caption = "Higher rainfall often leads to runoff and increased bacterial contamination"
  ) +
  theme_classic(base_size = 17) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    plot.caption = element_text(size = 11, face = "italic"),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "grey90", color = NA),
    panel.grid.minor = element_blank()
  )

# 8. Scatter plot: 7-day rainfall vs enterococci with season colors
rainfall_scatter_plot <- merged_data %>%
  filter(!is.na(rain_7day), !is.na(enterococci_cfu_100ml), rain_7day <= 200) %>%
  ggplot(aes(x = rain_7day, y = enterococci_cfu_100ml+1, color = season)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "loess", se = F, color = "black", size = 1.3) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = MetBrewer::met.brewer("Signac", 4), name = "Season") +
  labs(
    title = "7-Day Cumulative Rainfall vs Bacterial Contamination",
    subtitle = "Relationship between recent rainfall and enterococci levels by season (log scale, +1 transformation)",
    x = "7-Day Cumulative Rainfall (mm)",
    y = "Enterococci + 1 (CFU/100ml)",
    caption = "Trend line shows overall relationship across all seasons"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    plot.caption = element_text(size = 11, face = "italic"),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# 9. Air temperature vs water quality (using max daily temperature)
air_temp_plot <- merged_data %>%
  filter(!is.na(max_temp_c), !is.na(enterococci_cfu_100ml)) %>%
  ggplot(aes(x = max_temp_c, y = enterococci_cfu_100ml+1)) +
  geom_point(alpha = 0.4, color = "#2E86AB", size = 1.5) +
  geom_smooth(method = "loess", color = "#F24236", size = 1.3, se = F) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title = "Daily Maximum Air Temperature vs Bacterial Contamination",
    subtitle = "Relationship between ambient temperature and enterococci levels (log scale, +1 transformation)",
    x = "Maximum Air Temperature (°C)",
    y = "Enterococci + 1 (CFU/100ml)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    panel.grid.minor = element_blank()
  )

# 10. Combined weather effects plot
weather_effects_plot <- merged_data %>%
  filter(!is.na(rain_7day), !is.na(max_temp_c), !is.na(enterococci_cfu_100ml)) %>%
  mutate(
    rain_category = case_when(
      rain_7day == 0 ~ "No Rain (0mm)",
      rain_7day <= 10 ~ "Light Rain (0-10mm)",
      rain_7day <= 30 ~ "Moderate Rain (10-30mm)",
      TRUE ~ "Heavy Rain (>30mm)"
    ) %>% factor(levels = c("No Rain (0mm)", "Light Rain (0-10mm)", 
                            "Moderate Rain (10-30mm)", "Heavy Rain (>30mm)"))
  ) %>%
  ggplot(aes(x = max_temp_c, y = enterococci_cfu_100ml+1, color = rain_category)) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "loess", se = FALSE, size = 1.3) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"),
                     name = "7-Day Rainfall") +
  labs(
    title = "Combined Weather Effects on Water Quality",
    subtitle = "Air temperature and rainfall interaction effects on enterococci levels (log scale, +1 transformation)",
    x = "Maximum Air Temperature (°C)",
    y = "Enterococci + 1 (CFU/100ml)"
  ) +
  theme_classic(base_size = 15) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 17),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

# =========================================================
# Create comprehensive dashboards ####
# =========================================================

# Main water quality dashboard
dashboard <- (spatial_plot | seasonal_plot) /
  (temp_correlation_plot | nhmrc_category_plot) +
  plot_annotation(
    title = "Sydney Swimming Sites Water Quality Analysis",
    subtitle = "Comprehensive analysis of enterococci levels across Sydney's swimming locations (NHMRC 2008 Standards)",
    caption = "Data: TidyTuesday Week 20, 2025 | NHMRC categories based on 95th percentile enterococci levels",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 18, hjust = 0.5)
    )
  ) + plot_layout(guides = "collect")

# Alternative dashboard with heatmap and site comparison
dashboard_alt <- (heatmap_plot) /
  plot_spacer() /
  (site_comparison_plot | spatial_plot + theme(legend.position = "bottom")) +
  plot_layout(heights = c(3, 0.1, 2)) +
  plot_annotation(
    title = "Sydney Swimming Sites Water Quality Dashboard",
    subtitle = "Temporal and spatial patterns of bacterial contamination (NHMRC 2008 Standards)",
    caption = "Data: TidyTuesday Week 20, 2025 | Higher enterococci levels indicate poorer water quality",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 18, hjust = 0.5)
    )
  )

# Weather analysis dashboard
weather_dashboard <- (rainfall_comparison_plot | rainfall_scatter_plot) /
  (air_temp_plot | weather_effects_plot) +
  plot_annotation(
    title = "Weather Impact on Sydney Swimming Site Water Quality",
    subtitle = "Analysis of rainfall and temperature effects on bacterial contamination levels",
    caption = "Data: TidyTuesday Week 20, 2025 | Weather data matched to nearest station within 50km",
    theme = theme(
      plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 18, hjust = 0.5),
      plot.caption = element_text(size = 18, hjust = 0.5)
    )
  )

# =========================================================
# Save all plots ####
# =========================================================

# Create output directory if it doesn't exist
if(!dir.exists("outputs")) dir.create("outputs")

if(!dir.exists("outputs/2025")) dir.create("outputs/2025")

# Save main dashboards
ggsave("outputs/2025/Week20_dashboard.png",
       plot = dashboard,
       height = 4,
       width = 6,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_dashboard_alt.png",
       plot = dashboard_alt,
       height = 4,
       width = 6,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_weather_dashboard.png",
       plot = weather_dashboard,
       height = 4,
       width = 6,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

# Save individual plots
ggsave("outputs/2025/Week20_heatmap.png",
       plot = heatmap_plot,
       height = 5,
       width = 12,
       units = "in",
       dpi = 100,        # Lower DPI
       scale = 3,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_spatial.png",
       plot = spatial_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_seasonal.png",
       plot = seasonal_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_temperature.png",
       plot = temp_correlation_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_sites.png",
       plot = site_comparison_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_nhmrc_categories.png",
       plot = nhmrc_category_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_rainfall_effects.png",
       plot = rainfall_comparison_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_rainfall_scatter.png",
       plot = rainfall_scatter_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_air_temperature.png",
       plot = air_temp_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

ggsave("outputs/2025/Week20_weather_effects.png",
       plot = weather_effects_plot,
       height = 2,
       width = 3,
       units = "in",
       dpi = 200,        # Lower DPI
       scale = 2.5,        # Scale up everything by 2x
       bg = "white")

# =========================================================
# Print comprehensive summary statistics ####
# =========================================================

cat("\n=== COMPREHENSIVE ANALYSIS SUMMARY ===\n")
cat("Total water quality samples:", nrow(clean_water), "\n")
cat("Number of swimming sites:", length(unique(clean_water$swim_site)), "\n")
cat("Date range:", min(clean_water$date), "to", max(clean_water$date), "\n")
cat("Mean enterococci level:", round(mean(clean_water$enterococci_cfu_100ml, na.rm = TRUE), 2), "CFU/100ml\n")
cat("Median enterococci level:", round(median(clean_water$enterococci_cfu_100ml, na.rm = TRUE), 2), "CFU/100ml\n")

# NHMRC category distribution
cat("\nNHMRC Water Quality Categories (based on NHMRC 2008 guidelines):\n")
nhmrc_dist <- clean_water %>% count(nhmrc_category) %>% mutate(pct = round(n/sum(n)*100, 1))
for(i in 1:nrow(nhmrc_dist)) {
  cat("- Category", nhmrc_dist$nhmrc_category[i], ":", nhmrc_dist$pct[i], "%\n")
}

# Quality interpretation distribution
cat("\nWater Quality Interpretation (assuming moderate sanitary risk):\n")
quality_dist <- clean_water %>% count(quality_interpretation) %>% mutate(pct = round(n/sum(n)*100, 1))
for(i in 1:nrow(quality_dist)) {
  cat("-", quality_dist$quality_interpretation[i], ":", quality_dist$pct[i], "%\n")
}

cat("\n=== WEATHER-BACTERIA CORRELATION ANALYSIS ===\n")

# Calculate correlations
correlations <- merged_data %>%
  filter(!is.na(rain_7day), !is.na(max_temp_c), !is.na(enterococci_cfu_100ml)) %>%
  summarise(
    cor_rain_1day = cor(rain_1day, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_rain_3day = cor(rain_3day, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_rain_7day = cor(rain_7day, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_rain_14day = cor(rain_14day, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_max_temp = cor(max_temp_c, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_water_temp = cor(water_temperature_c, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_min_temp = cor(min_temp_c, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_temp_range = cor(temp_range, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    cor_conductivity = cor(conductivity_ms_cm, log10(enterococci_cfu_100ml + 1), use = "complete.obs"),
    n_samples = n()
  )

# Display the results
print(correlations)

# More readable summary
correlation_summary <- correlations %>%
  select(-n_samples) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "correlation") %>%
  mutate(
    variable = str_replace(variable, "cor_", ""),
    variable = str_replace_all(variable, "_", " "),
    variable = str_to_title(variable)
  ) %>%
  arrange(desc(abs(correlation)))

print("Correlation Summary (ordered by absolute value):")
print(correlation_summary)
