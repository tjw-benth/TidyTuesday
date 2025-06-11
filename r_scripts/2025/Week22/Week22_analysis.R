############################################################
# Title: Tidy Tuesday - Project Gutenberg Collection
# Author: Tom Williams
# Date: June 11, 2025
# Description: TidyTuesday analysis of Project Gutenberg digital library collection
# Version: R version auto-detected below
############################################################

# =========================================================
# Load or install required packages ####
# =========================================================
pkgs <- c("tidyverse", "cli", "ggblanket", "ggbeeswarm","ggtext", "ggridges", "MetBrewer", "lubridate",
          "patchwork", "sf", "sp", "plotly", "tidytuesdayR", "janitor", "waffle", 
          "showtext", "fmsb", "scales", "ggrepel", "viridis", "here", "countrycode")

new_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

if(length(new_pkgs)) {
  install.packages(new_pkgs, dependencies = TRUE)
}
invisible(lapply(pkgs, library, character.only = TRUE))
cat("Running on R version:", R.version.string, "\n")

# =========================================================
# Load TidyTuesday data ####
# =========================================================
# Load data from TidyTuesday Week 22 2025
tuesdata <- tidytuesdayR::tt_load('2025-06-03')
gutenberg_authors <- tuesdata$gutenberg_authors
gutenberg_languages <- tuesdata$gutenberg_languages
gutenberg_metadata <- tuesdata$gutenberg_metadata
gutenberg_subjects <- tuesdata$gutenberg_subjects

# Alternatively, read directly from GitHub
# gutenberg_authors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_authors.csv')
# gutenberg_languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_languages.csv')
# gutenberg_metadata <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_metadata.csv')
# gutenberg_subjects <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-06-03/gutenberg_subjects.csv')

# =========================================================
# Explore the data ####
# =========================================================
str(gutenberg_authors)
str(gutenberg_languages)
str(gutenberg_metadata)
str(gutenberg_subjects)

glimpse(gutenberg_authors)
glimpse(gutenberg_languages)
glimpse(gutenberg_metadata)
glimpse(gutenberg_subjects)

# Check data dimensions
cat("Authors dataset:", nrow(gutenberg_authors), "rows,", ncol(gutenberg_authors), "columns\n")
cat("Languages dataset:", nrow(gutenberg_languages), "rows,", ncol(gutenberg_languages), "columns\n")
cat("Metadata dataset:", nrow(gutenberg_metadata), "rows,", ncol(gutenberg_metadata), "columns\n")
cat("Subjects dataset:", nrow(gutenberg_subjects), "rows,", ncol(gutenberg_subjects), "columns\n")

# =========================================================
# Data cleaning & transformation ####
# =========================================================

# Clean authors data
clean_authors <- gutenberg_authors %>%
  janitor::clean_names() %>%
  mutate(
    lifespan = deathdate - birthdate,
    has_wikipedia = !is.na(wikipedia),
    has_birth_year = !is.na(birthdate),
    has_death_year = !is.na(deathdate),
    century_born = case_when(
      birthdate >= 1800 & birthdate < 1900 ~ "19th Century",
      birthdate >= 1900 & birthdate < 2000 ~ "20th Century",
      birthdate >= 1700 & birthdate < 1800 ~ "18th Century",
      birthdate < 1700 ~ "Pre-18th Century",
      TRUE ~ "Unknown"
    )
  )

# Clean languages data
clean_languages <- gutenberg_languages %>%
  janitor::clean_names() %>%
  mutate(
    is_multilingual = total_languages > 1,
    language_name = case_when(
      language == "en" ~ "English",
      language == "fr" ~ "French",
      language == "de" ~ "German",
      language == "es" ~ "Spanish",
      language == "hu" ~ "Hungarian",
      language == "el" ~ "Greek",
      language == "eo" ~ "Esperanto",
      language == "it" ~ "Italian",
      language == "pt" ~ "Portuguese",
      language == "nl" ~ "Dutch",
      language == "la" ~ "Latin",
      language == "fi" ~ "Finnish",
      language == "da" ~ "Danish",
      language == "sv" ~ "Swedish",
      language == "no" ~ "Norwegian",
      language == "ru" ~ "Russian",
      language == "zh" ~ "Chinese",
      language == "ja" ~ "Japanese",
      TRUE ~ as.character(language)
    )
  )

# Clean metadata
clean_metadata <- gutenberg_metadata %>%
  janitor::clean_names() %>%
  mutate(
    has_author_id = !is.na(gutenberg_author_id),
    is_public_domain = rights == "Public domain in the USA.",
    multiple_bookshelves = str_detect(gutenberg_bookshelf, "/"),
    bookshelf_count = str_count(gutenberg_bookshelf, "/") + 1
  ) %>%
  filter(has_text == TRUE) # Focus on books with text

# Clean subjects
clean_subjects <- gutenberg_subjects %>%
  janitor::clean_names()

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

# =========================================================
# Key Research Questions ####
# =========================================================

# Q1: How many different languages are available?
languages_available <- clean_languages %>%
  distinct(language, language_name) %>%
  arrange(language_name)

cat("\n=== LANGUAGE ANALYSIS ===\n")
cat("Total unique languages available:", nrow(languages_available), "\n")

# Q2: How many books per language?
books_per_language <- clean_languages %>%
  count(language_name, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    language_name = fct_reorder(language_name, n)
  )

cat("\nTop 10 languages by number of books:\n")
print(books_per_language %>% slice_head(n = 10))

# Q3: Authors with multiple IDs
authors_multiple_ids <- clean_authors %>%
  group_by(author) %>%
  summarise(
    n_ids = n_distinct(gutenberg_author_id),
    ids = paste(gutenberg_author_id, collapse = ", "),
    .groups = "drop"
  ) %>%
  filter(n_ids > 1) %>%
  arrange(desc(n_ids))

cat("\n=== AUTHOR ID ANALYSIS ===\n")
cat("Authors appearing under multiple IDs:", nrow(authors_multiple_ids), "\n")
if(nrow(authors_multiple_ids) > 0) {
  cat("Examples of authors with multiple IDs:\n")
  print(authors_multiple_ids %>% slice_head(n = 10))
}

# 1. Language Distribution Visualization
plot1 <- books_per_language %>%
  slice_head(n = 15) %>%
  ggplot(aes(x = language_name, y = n, fill = language_name)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Archambault", 15)) +
  scale_y_log10(labels = comma_format()) +
  coord_flip() +
  labs(
    title = "Project Gutenberg Books by Language",
    subtitle = "Top 15 languages in the digital collection (log scale)",
    x = "Language",
    y = "Number of Books",
  ) +
  theme_custom() +
  theme(legend.position = "none")

# 2. Popular Bookshelves Analysis (Replacement for weak multilingual plot)
popular_shelves_data <- clean_metadata %>%
  filter(!is.na(gutenberg_bookshelf)) %>%
  separate_rows(gutenberg_bookshelf, sep = "/") %>%
  mutate(gutenberg_bookshelf = str_trim(gutenberg_bookshelf)) %>%
  count(gutenberg_bookshelf, sort = TRUE) %>%
  slice_head(n = 12) %>%
  mutate(
    gutenberg_bookshelf = fct_reorder(gutenberg_bookshelf, n),
    # Add some nice category groupings for color
    category = case_when(
      str_detect(gutenberg_bookshelf, "Fiction|Novel|Adventure|Mystery|Romance") ~ "Fiction",
      str_detect(gutenberg_bookshelf, "History|War|Biography|Politics") ~ "History & Biography",
      str_detect(gutenberg_bookshelf, "Poetry|Literature|Drama") ~ "Literature & Poetry",
      str_detect(gutenberg_bookshelf, "Science|Technology|Medicine") ~ "Science & Technology",
      str_detect(gutenberg_bookshelf, "Philosophy|Religion|Ethics") ~ "Philosophy & Religion",
      TRUE ~ "Other"
    )
  )

plot2 <- popular_shelves_data %>%
  ggplot(aes(x = gutenberg_bookshelf, y = n, fill = category)) +
  geom_col(alpha = 0.8, width = 0.7) +
  scale_fill_manual(
    values = MetBrewer::met.brewer("Signac", 6),
    name = "Category"
  ) +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(
    title = "Most Popular Project Gutenberg Bookshelves",
    subtitle = "Top 12 collections by number of works",
    x = "Bookshelf Category",
    y = "Number of Works",
    caption = "Works can appear in multiple bookshelves"
  ) +
  theme_custom() +
  theme(
    legend.position = "right"
  )

# Alternative: Combined plot showing both bookshelves AND multilingual analysis
# This creates a more interesting compound visualization

# Get multilingual stats for annotation
multilingual_stats <- clean_languages %>%
  summarise(
    total_works = n(),
    multilingual_works = sum(is_multilingual),
    multilingual_pct = round(multilingual_works / total_works * 100, 1)
  )

# Create the combined plot as an alternative
plot2_alternative <- popular_shelves_data %>%
  ggplot(aes(x = gutenberg_bookshelf, y = n, fill = category)) +
  geom_col(alpha = 0.8, width = 0.7) +
  scale_fill_manual(
    values = MetBrewer::met.brewer("Signac", 6),
    name = "Category"
  ) +
  scale_y_continuous(labels = comma_format()) +
  coord_flip() +
  labs(
    title = "Most Popular Project Gutenberg Bookshelves",
    subtitle = glue::glue("Top 12 collections by number of works | {multilingual_stats$multilingual_pct}% of works are multilingual"),
    x = "Bookshelf Category",
    y = "Number of Works"
  ) +
  theme_custom() +
  theme(
    legend.position = "right"
  ) +
  # Add a small inset plot showing multilingual distribution
  annotation_custom(
    grob = ggplotGrob(
      clean_languages %>%
        count(is_multilingual) %>%
        mutate(
          label = ifelse(is_multilingual, "Multi-\nlingual", "Single\nLanguage"),
          pct = n / sum(n)
        ) %>%
        ggplot(aes(x = "", y = pct, fill = is_multilingual)) +
        geom_col(width = 1) +
        coord_polar("y") +
        scale_fill_manual(values = c("#E8E8E8", "#FF6B6B")) +
        theme_void() +
        theme(legend.position = "none") +
        geom_text(aes(label = paste0(round(pct*100,1), "%")), 
                  position = position_stack(vjust = 0.5), size = 9)
    ),
    xmin = 4, xmax = 10, ymin = 25000, ymax = 35000
  )

# 3. Author Century Distribution
plot3 <- clean_authors %>%
  filter(century_born != "Unknown") %>%
  count(century_born, sort = TRUE) %>%
  mutate(century_born = fct_reorder(century_born, n)) %>%
  ggplot(aes(x = century_born, y = n, fill = century_born)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Degas", 4)) +
  labs(
    title = "Authors by Birth Century",
    subtitle = "Distribution of Project Gutenberg authors by birth century",
    x = "Birth Century",
    y = "Number of Authors"
  ) +
  theme_custom() +
  theme(legend.position = "none")

# 4. Rights Distribution
plot4 <- clean_metadata %>%
  count(rights, sort = TRUE) %>%
  mutate(
    pct = n / sum(n),
    # Create wrapped versions of the rights text
    rights_wrapped = case_when(
      str_detect(rights, "Public domain") ~ "Public domain<br>in the USA.",
      str_detect(rights, "Copyrighted") ~ "Copyrighted.<br>Read the copyright notice<br>inside this book for details.",
      TRUE ~ as.character(rights)
    ),
    rights_wrapped = fct_reorder(rights_wrapped, n)
  ) %>%
  ggplot(aes(x = rights_wrapped, y = pct, fill = rights_wrapped)) +
  geom_col(alpha = 0.8) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hiroshige", 3)) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(
    title = "Copyright Status Distribution",
    subtitle = "Rights status of works in Project Gutenberg",
    x = "Rights Status",
    y = "Percentage of Works"
  ) +
  theme_custom() +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown()
  )

# 5. Author Productivity Across Lifespans - Beeswarm Plot

# Prepare data for beeswarm plot: books per author across their lifespan
beeswarm_data <- clean_metadata %>%
  filter(!is.na(author), !is.na(gutenberg_author_id)) %>%
  # Count books per author
  count(author, gutenberg_author_id, name = "n_books") %>%
  # Join with author data to get birth/death years
  left_join(clean_authors, by = c("author", "gutenberg_author_id")) %>%
  filter(
    !is.na(birthdate), 
    !is.na(deathdate),
    birthdate >= 1700,  # Focus on modern era
    deathdate <= 2025,
    lifespan > 0,
    lifespan < 120,     # Remove outliers
    n_books >= 3        # Focus on more productive authors to reduce clutter
  ) %>%
  # Create age groups for beeswarm
  mutate(
    lifespan_group = case_when(
      lifespan < 50 ~ "Short Life\n(< 50 years)",
      lifespan >= 50 & lifespan < 70 ~ "Medium Life\n(50-69 years)", 
      lifespan >= 70 & lifespan < 85 ~ "Long Life\n(70-84 years)",
      lifespan >= 85 ~ "Very Long Life\n(85+ years)"
    ),
    lifespan_group = factor(lifespan_group, levels = c(
      "Short Life\n(< 50 years)",
      "Medium Life\n(50-69 years)", 
      "Long Life\n(70-84 years)",
      "Very Long Life\n(85+ years)"
    )),
    # Create productivity categories for coloring
    productivity_level = case_when(
      n_books < 10 ~ "Medium (5-9)",
      n_books >= 10 & n_books < 20 ~ "High (10-19)",
      n_books >= 20 & n_books < 40 ~ "Very High (20-39)",
      n_books >= 40 ~ "Exceptional (40+)"
    ),
    productivity_level = factor(productivity_level, levels = c(
      "Medium (5-9)",
      "High (10-19)", 
      "Very High (20-39)",
      "Exceptional (40+)"
    ))
  )

# Create the beeswarm plot
plot5 <- beeswarm_data %>%
  ggplot(aes(x = lifespan_group, y = n_books, color = productivity_level)) +
  ggbeeswarm::geom_beeswarm(
    size = 1,
    alpha = 0.8,
    method = "swarm",  # or "hex", "square"
    side = 0L,  # 0 = both sides, 1 = right, -1 = left
    priority = "random",  # adds randomness to positioning
    cex = 0.05  # Smaller cex for more spacing between groups
  ) +
  # # boxplot overlay for summary statistics
  # geom_boxplot(
  #   aes(x = lifespan_group, y = n_books),
  #   width = 0.3,
  #   alpha = 0.3,
  #   color = "black",
  #   fill = "white",
  #   outlier.shape = NA,  # Don't show outliers since we have all points
  #   linewidth = 0.5
  # ) +
  # text labels for 150+ book authors with jitter
  geom_text_repel(
    data = . %>% filter(n_books >= 150),
    aes(label = author),
    size = 5,
    color = "black",
    bg.color = "white",
    bg.r = 0.1,
    force = 2,
    max.overlaps = 10,
    min.segment.length = 0.1,
    seed = 123
  ) +
  scale_color_manual(
    values = MetBrewer::met.brewer("Hiroshige", 4),
    name = "Books Published"
  ) +
  scale_y_continuous(
    limits = c(0, max(beeswarm_data$n_books) + 20),
    breaks = c(0, 10, 20, 30, 50, 75, 100, 150, 200),
    labels = function(x) ifelse(x >= 200, paste0(x, "+"), as.character(x))
  ) +
  coord_flip() +
  labs(
    title = "Author Productivity by Lifespan",
    subtitle = paste0("Each dot represents an author with 3+ published books <br>• ", 
                      sum(beeswarm_data$n_books >= 100), " authors have 100+ books • ",
                      sum(beeswarm_data$n_books >= 150), " authors have 150+ books"),
    x = "Author Lifespan Groups",
    y = "Number of Books Published",
    caption = "Includes authors with ≥5 books and complete birth/death dates"
  ) +
  theme_custom() +
  theme(title = element_markdown(),
    legend.position = "right",
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
    axis.text.y = element_text(hjust = 0.5),
    plot.margin = margin(20, 40, 20, 20)  # Extra right margin for labels
  ) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Alternative version: Violin plot with points
plot5_alternative <- beeswarm_data %>%
  ggplot(aes(x = lifespan_group, y = n_books, fill = lifespan_group)) +
  geom_violin(alpha = 0.6, scale = "width") +
  ggbeeswarm::geom_beeswarm(
    aes(color = productivity_level),
    size = 1.5,
    alpha = 0.7,
    cex = 0.6
  ) +
  scale_fill_manual(
    values = MetBrewer::met.brewer("Hokusai1", 4),
    guide = "none"
  ) +
  scale_color_manual(
    values = MetBrewer::met.brewer("Hiroshige", 4),
    name = "Books Published"
  ) +
  scale_y_continuous(
    limits = c(0, 120),
    breaks = c(0, 10, 20, 30, 50, 75, 100)
  ) +
  labs(
    title = "Author Productivity Distribution by Lifespan",
    subtitle = "Violin plots show distribution density • Points show individual authors",
    x = "Author Lifespan Groups",
    y = "Number of Books Published"
  ) +
  theme_custom() +
  theme(legend.position = "bottom")

# Summary statistics for the beeswarm data
beeswarm_summary <- beeswarm_data %>%
  group_by(lifespan_group) %>%
  summarise(
    n_authors = n(),
    median_books = median(n_books),
    mean_books = round(mean(n_books), 1),
    q75_books = quantile(n_books, 0.75),
    max_books = max(n_books),
    median_lifespan = median(lifespan),
    .groups = "drop"
  )

cat("\n=== BEESWARM PLOT SUMMARY ===\n")
print(beeswarm_summary)

# Find notable authors for annotation potential
notable_authors <- beeswarm_data %>%
  filter(n_books >= 30) %>%
  arrange(desc(n_books)) %>%
  select(author, birthdate, deathdate, lifespan, n_books, lifespan_group) %>%
  slice_head(n = 8)

cat("\n=== MOST PRODUCTIVE AUTHORS (30+ books) ===\n")
print(notable_authors)

# =========================================================
# Create Dashboard Layout ####
# =========================================================

dashboard <- (plot1 + plot2_alternative) / (plot3 + plot5) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Project Gutenberg Digital Library Analysis",
    subtitle = "Exploring languages, authors, and copyright status in the collection",
    caption = "Data: Project Gutenberg via TidyTuesday Week 22 2025 | Analysis: @tjw-benth",
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
ggsave(paste0(here::here("outputs", "2025", "Week22_Gutenberg_Dashboard.png")),
       plot = dashboard,
       height = 16,
       width = 20,  
       units = "cm",
       dpi = 300,
       scale = 1.2)

# Save individual plots
ggsave(paste0(here::here("outputs", "2025", "Week22_languages.png")),
       plot = plot1, 
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week22_multilingual.png")),
       plot = plot2,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week22_authors_century.png")),
       plot = plot3,
       height = 10,
       width = 10,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week22_rights.png")),
       plot = plot4,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

ggsave(paste0(here::here("outputs", "2025", "Week22_productivity.png")),
       plot = plot5,
       height = 10,
       width = 15,
       units = "cm",
       dpi = 300,
       scale = 1.2)

# =========================================================
# Summary Statistics ####
# =========================================================

cat("\n=== PROJECT GUTENBERG ANALYSIS SUMMARY ===\n")
cat("Total Works with Text:", nrow(clean_metadata), "\n")
cat("Total Authors:", nrow(clean_authors), "\n")
cat("Total Languages:", nrow(languages_available), "\n")
cat("Works in Multiple Languages:", sum(clean_languages$is_multilingual), "\n")
cat("Authors with Multiple IDs:", nrow(authors_multiple_ids), "\n")

# Language statistics
cat("\n=== LANGUAGE STATISTICS ===\n")
cat("Most common language:", books_per_language$language_name[1], 
    "with", books_per_language$n[1], "books\n")
cat("Percentage of works in English:", 
    round(books_per_language$pct[books_per_language$language_name == "English"] * 100, 1), "%\n")

# Author statistics
authors_with_dates <- clean_authors %>% filter(!is.na(birthdate), !is.na(deathdate))
cat("\n=== AUTHOR STATISTICS ===\n")
cat("Authors with birth/death dates:", nrow(authors_with_dates), "\n")
if(nrow(authors_with_dates) > 0) {
  cat("Average lifespan:", round(mean(authors_with_dates$lifespan, na.rm = TRUE), 1), "years\n")
  cat("Oldest author at death:", max(authors_with_dates$deathdate, na.rm = TRUE), "\n")
  cat("Youngest author at death:", min(authors_with_dates$deathdate, na.rm = TRUE), "\n")
}

# =========================================================
# Data Quality & Refinement Suggestions ####
# =========================================================

cat("\n=== DATA REFINEMENT SUGGESTIONS ===\n")

# 1. Author name standardization issues
cat("1. AUTHOR NAME STANDARDIZATION:\n")
if(nrow(authors_multiple_ids) > 0) {
  cat("   - Found", nrow(authors_multiple_ids), "authors with multiple IDs\n")
  cat("   - Suggest creating author name matching algorithm\n")
  cat("   - Consider standardizing name formats (Last, First vs First Last)\n")
}

# 2. Missing metadata analysis
missing_analysis <- clean_metadata %>%
  summarise(
    missing_author = sum(is.na(author)),
    missing_author_id = sum(is.na(gutenberg_author_id)),
    missing_language = sum(is.na(language)),
    missing_bookshelf = sum(is.na(gutenberg_bookshelf)),
    total_works = n()
  )

cat("2. MISSING METADATA:\n")
cat("   - Works without author:", missing_analysis$missing_author, 
    "(", round(missing_analysis$missing_author/missing_analysis$total_works*100, 1), "%)\n")
cat("   - Works without author ID:", missing_analysis$missing_author_id,
    "(", round(missing_analysis$missing_author_id/missing_analysis$total_works*100, 1), "%)\n")
cat("   - Works without language:", missing_analysis$missing_language,
    "(", round(missing_analysis$missing_language/missing_analysis$total_works*100, 1), "%)\n")

# 3. Language code suggestions
unusual_languages <- clean_languages %>%
  count(language, language_name, sort = TRUE) %>%
  filter(n < 10) %>%
  nrow()

cat("3. LANGUAGE STANDARDIZATION:\n")
cat("   - Found", unusual_languages, "languages with <10 works\n")
cat("   - Consider creating language_name field with full names\n")
cat("   - Some ISO codes may need verification\n")

# 4. Subject classification analysis
subject_analysis <- clean_subjects %>%
  count(subject_type) %>%
  mutate(pct = n/sum(n))

cat("4. SUBJECT CLASSIFICATION:\n")
print(subject_analysis)
cat("   - Consider adding broader subject categories\n")
cat("   - LCC and LCSH could be supplemented with simplified tags\n")

# 5. Date standardization
date_issues <- clean_authors %>%
  filter(!is.na(birthdate) | !is.na(deathdate)) %>%
  summarise(
    impossible_dates = sum(birthdate > 2025, na.rm = TRUE) + sum(deathdate > 2025, na.rm = TRUE),
    negative_lifespan = sum(lifespan < 0, na.rm = TRUE),
    very_long_lifespan = sum(lifespan > 100, na.rm = TRUE)
  )

cat("5. DATE QUALITY ISSUES:\n")
cat("   - Impossible future dates:", date_issues$impossible_dates, "\n")
cat("   - Negative lifespans:", date_issues$negative_lifespan, "\n")
cat("   - Lifespans over 100 years:", date_issues$very_long_lifespan, "\n")

# =========================================================
# Additional Analysis Ideas ####
# =========================================================

# Most prolific authors
prolific_authors <- clean_metadata %>%
  filter(!is.na(author)) %>%
  count(author, sort = TRUE) %>%
  slice_head(n = 10)

cat("\n=== MOST PROLIFIC AUTHORS ===\n")
print(prolific_authors)

# Popular bookshelves
popular_shelves <- clean_metadata %>%
  filter(!is.na(gutenberg_bookshelf)) %>%
  separate_rows(gutenberg_bookshelf, sep = "/") %>%
  count(gutenberg_bookshelf, sort = TRUE) %>%
  slice_head(n = 10)

cat("\n=== MOST POPULAR BOOKSHELVES ===\n")
print(popular_shelves)

# Subject analysis
subject_summary <- clean_subjects %>%
  group_by(subject_type) %>%
  summarise(
    n_works = n_distinct(gutenberg_id),
    n_subjects = n_distinct(subject),
    avg_subjects_per_work = n() / n_distinct(gutenberg_id),
    .groups = "drop"
  )

cat("\n=== SUBJECT CLASSIFICATION SUMMARY ===\n")
print(subject_summary)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check the outputs folder for saved visualizations!\n")
