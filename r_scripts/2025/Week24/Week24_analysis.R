############################################################
# Title: Tidy Tuesday - Web APIs from APIs.guru
# Author: Tom Williams
# Date: June 17, 2025
# Description: TidyTuesday analysis of Web APIs dataset from APIs.guru
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor", "waffle", 
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
# Load data from TidyTuesday Week 24 2025
tuesdata <- tidytuesdayR::tt_load('2025-06-17')
api_categories <- tuesdata$api_categories
api_info <- tuesdata$api_info
api_logos <- tuesdata$api_logos
api_origins <- tuesdata$api_origins
apisguru_apis <- tuesdata$apisguru_apis

# Alternatively, read directly from GitHub
# api_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
# api_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
# api_logos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
# api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
# apisguru_apis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')

# =========================================================
# Explore the data ####
# =========================================================
str(api_categories)
str(api_info)
str(api_logos)
str(api_origins)
str(apisguru_apis)

glimpse(api_categories)
glimpse(api_info)
glimpse(apisguru_apis)

# Check data dimensions
cat("Dataset dimensions:\n")
cat("API Categories:", nrow(api_categories), "rows\n")
cat("API Info:", nrow(api_info), "rows\n")
cat("API Logos:", nrow(api_logos), "rows\n")
cat("API Origins:", nrow(api_origins), "rows\n")
cat("APIs Guru Main:", nrow(apisguru_apis), "rows\n")

# =========================================================
# Data cleaning & transformation ####
# =========================================================

# Create main analysis dataset by joining key tables
main_df <- apisguru_apis %>%
  janitor::clean_names() %>%
  left_join(api_info %>% select(name, provider_name, service_name, description, title), 
            by = "name") %>%
  left_join(api_categories %>% 
              group_by(name) %>% 
              summarise(categories = paste(apisguru_category, collapse = ", "),
                        n_categories = n(), .groups = "drop"),
            by = "name") %>%
  mutate(
    year_added = lubridate::year(added),
    year_updated = lubridate::year(updated),
    months_since_update = as.numeric(difftime(Sys.Date(), updated, units = "days")) / 30.44,
    has_external_docs = !is.na(external_docs_url),
    openapi_version_clean = case_when(
      str_detect(openapi_ver, "^3\\.") ~ "OpenAPI 3.x",
      str_detect(openapi_ver, "^2\\.") ~ "Swagger 2.x",
      TRUE ~ "Other/Unknown"
    ),
    provider_clean = case_when(
      is.na(provider_name) ~ "Unknown Provider",
      str_length(provider_name) > 20 ~ str_trunc(provider_name, 20),
      TRUE ~ provider_name
    )
  ) %>%
  filter(!is.na(added))

# Get category analysis
category_summary <- api_categories %>%
  count(apisguru_category, sort = TRUE, name = "n_apis") %>%
  mutate(
    pct = n_apis / sum(n_apis),
    category_clean = case_when(
      str_length(apisguru_category) > 15 ~ str_trunc(apisguru_category, 15),
      TRUE ~ apisguru_category
    )
  )

# Provider analysis
provider_summary <- main_df %>%
  filter(!is.na(provider_name)) %>%
  count(provider_name, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(
    provider_short = case_when(
      str_length(provider_name) > 12 ~ str_trunc(provider_name, 12),
      TRUE ~ provider_name
    ),
    provider_short = fct_reorder(provider_short, n)
  )

# =========================================================
# Analysis & Visualization ####
# =========================================================

# Add fonts
showtext::showtext_auto()

# Custom theme
theme_custom <- function() {
  theme_classic(base_family = "sans") +
    theme(
      plot.title = element_text(size = 32, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 24, color = "grey40"),
      plot.caption = element_text(size = 18, color = "grey50"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# 1. API Categories Distribution
plot1 <- category_summary %>%
  slice_head(n = 12) %>%
  mutate(category_clean = fct_reorder(category_clean, n_apis)) %>%
  ggplot(aes(x = category_clean, y = n_apis, fill = category_clean)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Signac", 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Most Popular API Categories",
    subtitle = "Number of APIs registered in each category on APIs.guru",
    x = "API Category",
    y = "Number of APIs",
  ) +
  theme_custom()

# 2. API Growth Over Time
plot2 <- main_df %>%
  filter(year_added >= 2014) %>%
  count(year_added, name = "apis_added") %>%
  mutate(cumulative_apis = cumsum(apis_added)) %>%
  ggplot(aes(x = year_added)) +
  geom_col(aes(y = apis_added), fill = MetBrewer::met.brewer("Signac")[4], alpha = 0.7) +
  geom_line(aes(y = cumulative_apis / 10), color = MetBrewer::met.brewer("Signac")[2], size = 2) +
  scale_y_continuous(
    name = "APIs Added per Year",
    sec.axis = sec_axis(~ . * 10, name = "Cumulative APIs")
  ) +
  scale_x_continuous(breaks = seq(2014, 2025, 2)) +
  labs(
    title = "API Growth on APIs.guru",
    subtitle = "Bars show annual additions, line shows cumulative total",
    x = "Year",
  ) +
  theme_custom()

# 3. Top API Providers
plot3 <- provider_summary %>%
  ggplot(aes(x = provider_short, y = n, fill = provider_short)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Archambault", nrow(provider_summary))) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Top API Providers",
    subtitle = "Organizations with the most APIs in the directory",
    x = "Provider",
    y = "Number of APIs",
  ) +
  theme_custom()

# 4. OpenAPI Version Distribution
plot4 <- main_df %>%
  filter(!is.na(openapi_ver)) %>%
  count(openapi_version_clean, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    openapi_version_clean = fct_reorder(openapi_version_clean, n)
  ) %>%
  ggplot(aes(x = openapi_version_clean, y = pct, fill = openapi_version_clean)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hokusai2", 3)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "API Specification Versions",
    subtitle = "Distribution of OpenAPI/Swagger versions",
    x = "Specification Version",
    y = "Percentage of APIs",
  ) +
  theme_custom()

# 5. API Maintenance Patterns
plot5 <- main_df %>%
  filter(!is.na(months_since_update), months_since_update >= 0) %>%
  mutate(
    update_category = case_when(
      months_since_update <= 3 ~ "Recently Updated (≤3 months)",
      months_since_update <= 12 ~ "Moderately Updated (3-12 months)",
      months_since_update <= 36 ~ "Older Updates (1-3 years)",
      TRUE ~ "Very Old (>3 years)"
    ),
    update_category = factor(update_category, levels = c(
      "Recently Updated (≤3 months)",
      "Moderately Updated (3-12 months)", 
      "Older Updates (1-3 years)",
      "Very Old (>3 years)"
    ))
  ) %>%
  count(update_category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = update_category, y = pct, fill = update_category)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Moreau", 4)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(
    title = "API Maintenance Patterns",
    subtitle = "How recently APIs were last updated",
    x = "Time Since Last Update",
    y = "Percentage of APIs",
  ) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# =========================================================
# Create Dashboard Layout ####
# =========================================================

############################################################
# Title: Tidy Tuesday - Web APIs from APIs.guru
# Author: Tom Williams
# Date: June 17, 2025
# Description: TidyTuesday analysis of Web APIs dataset from APIs.guru
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggtext", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor", "waffle", 
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
# Load data from TidyTuesday Week 24 2025
tuesdata <- tidytuesdayR::tt_load('2025-06-17')
api_categories <- tuesdata$api_categories
api_info <- tuesdata$api_info
api_logos <- tuesdata$api_logos
api_origins <- tuesdata$api_origins
apisguru_apis <- tuesdata$apisguru_apis

# Alternatively, read directly from GitHub
# api_categories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_categories.csv')
# api_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_info.csv')
# api_logos <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_logos.csv')
# api_origins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/api_origins.csv')
# apisguru_apis <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-17/apisguru_apis.csv')

# =========================================================
# Explore the data ####
# =========================================================
str(api_categories)
str(api_info)
str(api_logos)
str(api_origins)
str(apisguru_apis)

glimpse(api_categories)
glimpse(api_info)
glimpse(apisguru_apis)

# Check data dimensions
cat("Dataset dimensions:\n")
cat("API Categories:", nrow(api_categories), "rows\n")
cat("API Info:", nrow(api_info), "rows\n")
cat("API Logos:", nrow(api_logos), "rows\n")
cat("API Origins:", nrow(api_origins), "rows\n")
cat("APIs Guru Main:", nrow(apisguru_apis), "rows\n")

# =========================================================
# Data cleaning & transformation ####
# =========================================================

# Create main analysis dataset by joining key tables
main_df <- apisguru_apis %>%
  janitor::clean_names() %>%
  left_join(api_info %>% select(name, provider_name, service_name, description, title), 
            by = "name") %>%
  left_join(api_categories %>% 
              group_by(name) %>% 
              summarise(categories = paste(apisguru_category, collapse = ", "),
                        n_categories = n(), .groups = "drop"),
            by = "name") %>%
  mutate(
    year_added = lubridate::year(added),
    year_updated = lubridate::year(updated),
    months_since_update = as.numeric(difftime(Sys.Date(), updated, units = "days")) / 30.44,
    has_external_docs = !is.na(external_docs_url),
    openapi_version_clean = case_when(
      str_detect(openapi_ver, "^3\\.") ~ "OpenAPI 3.x",
      str_detect(openapi_ver, "^2\\.") ~ "Swagger 2.x",
      TRUE ~ "Other/Unknown"
    ),
    provider_clean = case_when(
      is.na(provider_name) ~ "Unknown Provider",
      str_length(provider_name) > 20 ~ str_trunc(provider_name, 20),
      TRUE ~ provider_name
    )
  ) %>%
  filter(!is.na(added))

# Get category analysis
category_summary <- api_categories %>%
  count(apisguru_category, sort = TRUE, name = "n_apis") %>%
  mutate(
    pct = n_apis / sum(n_apis),
    category_clean = case_when(
      str_length(apisguru_category) > 15 ~ str_trunc(apisguru_category, 15),
      TRUE ~ apisguru_category
    )
  )

# Provider analysis
provider_summary <- main_df %>%
  filter(!is.na(provider_name)) %>%
  count(provider_name, sort = TRUE) %>%
  slice_head(n = 15) %>%
  mutate(
    provider_short = case_when(
      str_length(provider_name) > 12 ~ str_trunc(provider_name, 12),
      TRUE ~ provider_name
    ),
    provider_short = fct_reorder(provider_short, n)
  )

# =========================================================
# Analysis & Visualization ####
# =========================================================

# Add fonts
showtext::showtext_auto()

# Custom theme
theme_custom <- function() {
  theme_classic(base_family = "sans") +
    theme(
      plot.title = element_text(size = 32, face = "bold", family = "sans"),
      plot.subtitle = element_text(size = 24, color = "grey40"),
      plot.caption = element_text(size = 18, color = "grey50"),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 18),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# 1. API Categories Distribution
plot1 <- category_summary %>%
  slice_head(n = 12) %>%
  mutate(category_clean = fct_reorder(category_clean, n_apis)) %>%
  ggplot(aes(x = category_clean, y = n_apis, fill = category_clean)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Signac", 12)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Most Popular API Categories",
    subtitle = "Number of APIs registered in each category on APIs.guru",
    x = "API Category",
    y = "Number of APIs",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025"
  ) +
  theme_custom()

# 2. API Growth Over Time
plot2 <- main_df %>%
  filter(year_added >= 2014) %>%
  count(year_added, name = "apis_added") %>%
  mutate(cumulative_apis = cumsum(apis_added)) %>%
  ggplot(aes(x = year_added)) +
  geom_col(aes(y = apis_added), fill = MetBrewer::met.brewer("Signac")[4], alpha = 0.7) +
  geom_line(aes(y = cumulative_apis / 10), color = MetBrewer::met.brewer("Signac")[2], size = 2) +
  scale_y_continuous(
    name = "APIs Added per Year",
    sec.axis = sec_axis(~ . * 10, name = "Cumulative APIs")
  ) +
  scale_x_continuous(breaks = seq(2014, 2025, 2)) +
  labs(
    title = "API Growth on APIs.guru",
    subtitle = "Bars show annual additions, line shows cumulative total",
    x = "Year",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025"
  ) +
  theme_custom()

# 3. Top API Providers
plot3 <- provider_summary %>%
  ggplot(aes(x = provider_short, y = n, fill = provider_short)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Benedictus", nrow(provider_summary))) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "Top API Providers",
    subtitle = "Organizations with the most APIs in the directory",
    x = "Provider",
    y = "Number of APIs",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025"
  ) +
  theme_custom()

# 4. OpenAPI Version Distribution
plot4 <- main_df %>%
  filter(!is.na(openapi_ver)) %>%
  count(openapi_version_clean, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    openapi_version_clean = fct_reorder(openapi_version_clean, n)
  ) %>%
  ggplot(aes(x = openapi_version_clean, y = pct, fill = openapi_version_clean)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hokusai2", 3)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  coord_flip() +
  labs(
    title = "API Specification Versions",
    subtitle = "Distribution of OpenAPI/Swagger versions",
    x = "Specification Version",
    y = "Percentage of APIs",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025"
  ) +
  theme_custom()

# 5. API Maintenance Patterns
plot5 <- main_df %>%
  filter(!is.na(months_since_update), months_since_update >= 0) %>%
  mutate(
    update_category = case_when(
      months_since_update <= 3 ~ "Recently Updated (≤3 months)",
      months_since_update <= 12 ~ "Moderately Updated (3-12 months)",
      months_since_update <= 36 ~ "Older Updates (1-3 years)",
      TRUE ~ "Very Old (>3 years)"
    ),
    update_category = factor(update_category, levels = c(
      "Recently Updated (≤3 months)",
      "Moderately Updated (3-12 months)", 
      "Older Updates (1-3 years)",
      "Very Old (>3 years)"
    ))
  ) %>%
  count(update_category) %>%
  mutate(pct = n / sum(n)) %>%
  ggplot(aes(x = update_category, y = pct, fill = update_category)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  scale_fill_manual(values = MetBrewer::met.brewer("Moreau", 4)) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_x_discrete(labels = function(x) str_wrap(x, 15)) +
  labs(
    title = "API Maintenance Patterns",
    subtitle = "How recently APIs were last updated",
    x = "Time Since Last Update",
    y = "Percentage of APIs",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025"
  ) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 6. Provider-Category Network Analysis
network_data <- api_categories %>%
  left_join(api_info %>% select(name, provider_name), by = "name") %>%
  filter(!is.na(provider_name)) %>%
  count(provider_name, apisguru_category, name = "connection_strength") %>%
  filter(connection_strength >= 2) %>%  # Only show providers with 2+ APIs in a category
  # Focus on top providers and categories for readability
  group_by(provider_name) %>%
  filter(sum(connection_strength) >= 3) %>%  # Providers with 3+ total APIs
  ungroup() %>%
  group_by(apisguru_category) %>%
  filter(sum(connection_strength) >= 4) %>%  # Categories with 4+ APIs from these providers
  ungroup()

# Check if we have data and create the network plot
if(nrow(network_data) > 0) {
  
  # Create separate dataframes for nodes
  providers_df <- network_data %>%
    distinct(provider_name) %>%
    mutate(
      provider_short = case_when(
        str_length(provider_name) > 15 ~ str_trunc(provider_name, 15),
        TRUE ~ provider_name
      ),
      x = 1,
      y = row_number() * 2,  # Space them out more
      node_type = "provider"
    )
  
  categories_df <- network_data %>%
    distinct(apisguru_category) %>%
    mutate(
      category_short = case_when(
        str_length(apisguru_category) > 15 ~ str_trunc(apisguru_category, 15),
        TRUE ~ apisguru_category
      ),
      x = 4,
      y = row_number() * 2,  # Space them out more
      node_type = "category"
    )
  
  # Create edges dataframe with coordinates
  edges_df <- network_data %>%
    left_join(providers_df %>% select(provider_name, provider_x = x, provider_y = y), 
              by = "provider_name") %>%
    left_join(categories_df %>% select(apisguru_category, category_x = x, category_y = y), 
              by = "apisguru_category") %>%
    mutate(
      line_width = case_when(
        connection_strength >= 5 ~ 1.5,
        connection_strength >= 3 ~ 1.0,
        TRUE ~ 0.5
      ),
      connection_color = case_when(
        connection_strength >= 5 ~ MetBrewer::met.brewer("Signac")[1],  # Strong connections
        connection_strength >= 3 ~ MetBrewer::met.brewer("Signac")[3],  # Medium connections  
        TRUE ~ MetBrewer::met.brewer("Signac")[4]  # Weak connections
      )
    )
  
  # Create the plot
  plot6 <- ggplot() +
    # Draw connections first (so they appear behind nodes)
    geom_segment(data = edges_df,
                 aes(x = provider_x, y = provider_y,
                     xend = category_x, yend = category_y,
                     color = connection_color, size = line_width),
                 alpha = 0.7) +
    # Add provider nodes
    geom_point(data = providers_df,
               aes(x = x, y = y),
               color = MetBrewer::met.brewer("Benedictus")[1],
               size = 2, alpha = 0.8) +
    # Add category nodes  
    geom_point(data = categories_df,
               aes(x = x, y = y),
               color = MetBrewer::met.brewer("Benedictus")[5],
               size = 2, alpha = 0.8) +
    # Add provider labels
    geom_text(data = providers_df,
              aes(x = x - 0.15, y = y, label = provider_short),
              hjust = 1, size = 3.5, family = "sans", fontface = "bold") +
    # Add category labels
    geom_text(data = categories_df,
              aes(x = x + 0.15, y = y, label = category_short),
              hjust = 0, size = 3.5, family = "sans", fontface = "bold") +
    # Legend for connection strength
    annotate("text", x = 2.5, y = max(c(max(providers_df$y), max(categories_df$y))) + 3,
             label = "Connection Strength", fontface = "bold", size = 10) +
    annotate("segment", x = 2.2, y = max(c(max(providers_df$y), max(categories_df$y))) ,
             xend = 2.4, yend = max(c(max(providers_df$y), max(categories_df$y))),
             size = 1.5, color = MetBrewer::met.brewer("Signac")[1]) +
    annotate("text", x = 2.45, y = max(c(max(providers_df$y), max(categories_df$y))),
             label = "5+ APIs", hjust = 0, size = 7) +
    annotate("segment", x = 2.2, y = max(c(max(providers_df$y), max(categories_df$y))) - 3,
             xend = 2.4, yend = max(c(max(providers_df$y), max(categories_df$y))) - 3,
             size = 1.0, color = MetBrewer::met.brewer("Signac")[3]) +
    annotate("text", x = 2.45, y = max(c(max(providers_df$y), max(categories_df$y))) - 3,
             label = "3-4 APIs", hjust = 0, size = 7) +
    annotate("segment", x = 2.2, y = max(c(max(providers_df$y), max(categories_df$y))) -6,
             xend = 2.4, yend = max(c(max(providers_df$y), max(categories_df$y))) - 6,
             size = 0.5, color = MetBrewer::met.brewer("Signac")[4]) +
    annotate("text", x = 2.45, y = max(c(max(providers_df$y), max(categories_df$y))) - 6,
             label = "2 APIs", hjust = 0, size = 7) +
    scale_color_identity() +
    scale_size_identity() +
    xlim(0, 5.5) +
    ylim(0, max(c(max(providers_df$y), max(categories_df$y))) + 4) +
    labs(
      title = "Provider-Category Network",
      subtitle = paste0("Major API ", 
                        "<span style='color:", MetBrewer::met.brewer("Benedictus")[1], "'>**providers**</span>", 
                        " and their ", 
                        "<span style='color:", MetBrewer::met.brewer("Benedictus")[5], "'>**category**</span>", 
                        " specializations"),
      caption = "Line color & thickness = number of APIs"
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 32, face = "bold", family = "sans"),
      plot.subtitle = ggtext::element_markdown(size = 24, color = "grey40"),
      plot.caption = element_text(size = 18, color = "grey50"),
      legend.position = "none"
    )
  
} else {
  # Fallback if no network data
  plot6 <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, 
             label = "No network connections found\nwith current filtering criteria", 
             size = 8, hjust = 0.5) +
    theme_void() +
    labs(title = "Provider-Category Network", subtitle = "No data available")
}


# =========================================================
# Create Dashboard Layout ####
# =========================================================

# Create a comprehensive dashboard
dashboard <- (plot1 + plot2) / (plot3 + (plot4 + plot5)) / plot6 +
  plot_layout(
    heights = c(1, 1, 1)
  ) +
  plot_annotation(
    title = "Web APIs Landscape Analysis",
    subtitle = "Exploring the APIs.guru directory - powering Jon Harmon's Ghana R Conference 2025 talk",
    caption = "Data: APIs.guru via TidyTuesday Week 24 2025 | Analysis: @tjw-benth",
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

# Save main dashboard
ggsave(paste0(here::here("outputs", "2025", "Week24_WebAPIs_Dashboard.png")),
       plot = dashboard,
       height = 20,
       width = 24,  
       units = "cm",
       dpi = 300,
       scale = 1.4)

# Save individual plots
ggsave(paste0(here::here("outputs", "2025", "Week24_api_categories.png")),
       plot = plot1, 
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week24_api_growth.png")),
       plot = plot2,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week24_top_providers.png")),
       plot = plot3,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week24_openapi_versions.png")),
       plot = plot4,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week24_maintenance_patterns.png")),
       plot = plot5,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week24_provider_network.png")),
       plot = plot6,
       height = 12,
       width = 18,
       units = "cm",
       dpi = 300,
       scale = 1.2)

# =========================================================
# Summary Statistics ####
# =========================================================

cat("\n=== WEB APIs ANALYSIS SUMMARY ===\n")
cat("Total APIs:", nrow(main_df), "\n")
cat("Total Categories:", nrow(category_summary), "\n")
cat("Total Providers:", n_distinct(main_df$provider_name, na.rm = TRUE), "\n")
cat("Date Range:", min(main_df$added, na.rm = TRUE), "to", max(main_df$added, na.rm = TRUE), "\n")

# Most common category
top_category <- category_summary %>% slice(1)
cat("Most Common Category:", top_category$apisguru_category, "(", top_category$n_apis, "APIs)\n")

# Most prolific provider
top_provider <- provider_summary %>% arrange(desc(n)) %>% slice(1)
cat("Most Prolific Provider:", top_provider$provider_name, "(", top_provider$n, "APIs)\n")

cat("\n=== INTERESTING FINDINGS ===\n")

# APIs with external documentation
external_docs_pct <- mean(main_df$has_external_docs, na.rm = TRUE) * 100
cat("APIs with External Documentation:", round(external_docs_pct, 1), "%\n")

# OpenAPI version adoption
openapi3_pct <- main_df %>% 
  filter(!is.na(openapi_ver)) %>% 
  summarise(pct = mean(str_detect(openapi_ver, "^3\\."), na.rm = TRUE) * 100) %>% 
  pull(pct)
cat("APIs using OpenAPI 3.x:", round(openapi3_pct, 1), "%\n")

# Recently updated APIs
recent_update_pct <- main_df %>%
  filter(!is.na(months_since_update)) %>%
  summarise(pct = mean(months_since_update <= 12, na.rm = TRUE) * 100) %>%
  pull(pct)
cat("APIs updated in last 12 months:", round(recent_update_pct, 1), "%\n")

# Peak year for API additions
peak_year <- main_df %>%
  filter(!is.na(year_added)) %>%
  count(year_added, sort = TRUE) %>%
  slice(1)
cat("Peak year for API additions:", peak_year$year_added, "(", peak_year$n, "APIs added)\n")

# Show top categories with counts
cat("\nTop 5 API Categories:\n")
top_categories <- category_summary %>%
  slice_head(n = 5) %>%
  select(apisguru_category, n_apis)
print(top_categories)

# =========================================================
# Additional Analysis Ideas ####
# =========================================================

# Category co-occurrence analysis
# multi_category_apis <- api_categories %>%
#   group_by(name) %>%
#   summarise(categories = paste(apisguru_category, collapse = " & "), .groups = "drop") %>%
#   filter(str_detect(categories, " & ")) %>%
#   count(categories, sort = TRUE)

# Geographic analysis of API origins (if country data available)
# country_analysis <- api_origins %>%
#   count(country, sort = TRUE) %>%
#   slice_head(n = 10)

# License analysis
# license_summary <- api_info %>%
#   filter(!is.na(license_name)) %>%
#   count(license_name, sort = TRUE) %>%
#   slice_head(n = 10)

# Interactive exploration of API ecosystem
# interactive_scatter <- main_df %>%
#   filter(!is.na(year_added), !is.na(months_since_update)) %>%
#   ggplot(aes(x = year_added, y = months_since_update, 
#              color = openapi_version_clean,
#              text = paste("API:", name, "<br>Provider:", provider_name))) +
#   geom_point(alpha = 0.7) +
#   scale_color_manual(values = MetBrewer::met.brewer("Hokusai2", 3)) +
#   labs(title = "API Age vs Last Update",
#        x = "Year Added", y = "Months Since Update") +
#   theme_custom()
# 
# plotly::ggplotly(interactive_scatter, tooltip = "text")