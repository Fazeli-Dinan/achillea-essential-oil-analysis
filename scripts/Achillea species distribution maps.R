# ============================================================
# Achillea species distribution maps
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Creates heatmap-style world maps showing the
#              number of studies per country for three Achillea
#              species (A. biebersteinii, A. millefolium, A. nobilis).
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)
library(readxl)

# -----------------------------
# 1) Define paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 2) Read Excel files and build df_long
# -----------------------------
read_species <- function(file, species_name) {
  read_excel(file.path(data_dir, file)) %>%
    mutate(species = species_name) %>%
    select(species, country = Country)
}

df_long <- bind_rows(
  read_species("A_biebersteinii.xlsx", "biebersteinii"),
  read_species("A_millefolium.xlsx",   "millefolium"),
  read_species("A_nobilis.xlsx",       "nobilis")
) %>%
  mutate(country = recode(country,
                          "Eastern Turkey"            = "Turkey",
                          "USA"                       = "United States of America",
                          "UK"                        = "United Kingdom",
                          "Czech Republic"            = "Czechia",
                          "Türkiye"                   = "Turkey",
                          "United States"             = "United States of America",
                          .default = country
  ))

cat("===== Records per species =====\n")
print(table(df_long$species))
cat("\n===== Unique countries =====\n")
print(sort(unique(df_long$country)))

# -----------------------------
# 3) World map data
# -----------------------------
world <- ne_countries(scale = "medium", returnclass = "sf")

# -----------------------------
# 4) Map theme
# -----------------------------
white_map_theme <- theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.title       = element_blank(),
    legend.position  = "right",
    legend.title     = element_text(size = 8, face = "bold"),
    legend.text      = element_text(size = 9),
    plot.title       = element_text(size = 13.5, face = "italic",
                                    hjust = 0.5, margin = margin(b = 10)),
    plot.margin      = margin(10, 10, 10, 160)
  )

# -----------------------------
# 5) Function: species map with heatmap + single-column list
# -----------------------------
plot_species_map <- function(species_name, low_color, high_color, title_text) {
  
  country_counts <- df_long %>%
    filter(species == species_name) %>%
    count(country, name = "n_studies") %>%
    arrange(desc(n_studies), country) %>%
    mutate(number = row_number())
  
  world_data <- world %>%
    left_join(country_counts, by = c("name" = "country"))
  
  centroids <- world_data %>%
    filter(!is.na(n_studies)) %>%
    st_centroid() %>%
    cbind(st_coordinates(.))
  
  legend_table <- country_counts %>%
    mutate(label = paste0(number, ": ", country, " (", n_studies, ")")) %>%
    pull(label) %>%
    paste(collapse = "\n")
  
  legend_size <- 1.4
  left_margin <- 160
  
  p <- ggplot(world_data) +
    geom_sf(aes(fill = n_studies), color = "gray40", linewidth = 0.25) +
    scale_fill_gradient(
      low = low_color, high = high_color,
      na.value = "#f0f0f0",
      name = "Number of\nstudies",
      breaks = pretty(country_counts$n_studies, n = 5)
    ) +
    white_map_theme +
    labs(title = title_text) +
    geom_text(data = centroids,
              aes(x = X, y = Y, label = number),
              size = 1.5, fontface = "bold", color = "black") +
    coord_sf(expand = FALSE) +
    annotate("text", x = -Inf, y = 0, label = legend_table,
             hjust = 0, vjust = 0.5, size = legend_size, lineheight = 0.85) +
    theme(plot.margin = margin(10, 10, 10, left_margin))
  
  return(p)
}

# -----------------------------
# 6) Build maps
# -----------------------------
map_b <- plot_species_map("biebersteinii", "#deebf7", "#08519c", "A. biebersteinii")
map_m <- plot_species_map("millefolium",   "#e5f5e0", "#006d2c", "A. millefolium")
map_n <- plot_species_map("nobilis",       "#fff5eb", "#d94801", "A. nobilis")

combined_map <- map_b / map_m / map_n

# -----------------------------
# 7) Save outputs
# -----------------------------
ggsave(file.path(output_dir, "Achillea_maps_heatmap_3x1.png"),
       combined_map, width = 9.5, height = 14.5, dpi = 350)

ggsave(file.path(output_dir, "Achillea_maps_heatmap_3x1.pdf"),
       combined_map, width = 9.5, height = 14.5, dpi = 350)

message("✅ Maps saved to: ", normalizePath(output_dir))
