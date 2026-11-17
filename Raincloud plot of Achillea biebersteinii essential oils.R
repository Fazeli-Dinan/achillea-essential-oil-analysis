# ============================================================
# Raincloud plot for essential oil meta-analysis
# Species: Achillea biebersteinii
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Builds raincloud plots showing the distribution
#              of recurrent major compounds (present in >= 5
#              studies, percentage >= 1%) across studies.
#              The "Present study" is highlighted as a red square.
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggdist)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 2) Read data
# -----------------------------
data <- read_excel(file.path(data_dir, "A_biebersteinii.xlsx"))

# -----------------------------
# 3) Split compounds & percents -> long format
# -----------------------------
df_long <- data %>%
  mutate(
    Compound_list = str_split(Compounds, ",\\s*"),
    Percent_list  = str_split(Percents,  ",\\s*")
  ) %>%
  unnest(cols = c(Compound_list, Percent_list)) %>%
  mutate(
    Compound = case_when(
      str_detect(Compound_list, regex("1-8-Cineole|1,8 Cineole|1,8-cineole|Eucalyptol|eucalyptol", ignore_case = TRUE)) ~ "1,8-Cineole",
      str_detect(Compound_list, regex("^\\.-Terpinene$|^.-Terpinene$|gamma-Terpinene|^Terpinene$|^-Terpinene$|^a-Terpinene$|^b-Terpinene$", ignore_case = TRUE)) ~ "γ-Terpinene",
      str_detect(Compound_list, regex("^E-Caryophyllene$|^e-caryophyllene$|^E-Caryophyllene", ignore_case = FALSE)) ~ "(E)-Caryophyllene",
      str_detect(Compound_list, regex("^T-Muurolene$|^t-Muurolene$|^gamma-Muurolene$|^y-Muurolene$", ignore_case = TRUE)) ~ "γ-Muurolene",
      TRUE ~ str_trim(Compound_list)
    ),
    Percent_list = str_remove(Percent_list, "%"),
    Percent_list = str_replace(Percent_list, ",", "."),
    Percentage   = as.numeric(Percent_list)
  ) %>%
  select(Study, Country, `Oil Extract`, Compound, Percentage)

# -----------------------------
# 4) Filter recurrent compounds (>= 5 studies) and remove < 1%
# -----------------------------
compound_frequency <- df_long %>%
  count(Compound, name = "Frequency") %>%
  arrange(desc(Frequency))

selected_compounds <- compound_frequency %>%
  filter(Frequency >= 5)

filtered_data <- df_long %>%
  filter(Compound %in% selected_compounds$Compound,
         Percentage >= 1)

# -----------------------------
# 5) Order compounds by mean percentage
# -----------------------------
compound_order <- filtered_data %>%
  group_by(Compound) %>%
  summarise(Mean_Percentage = mean(Percentage), .groups = "drop") %>%
  arrange(Mean_Percentage)

filtered_data$Compound <- factor(filtered_data$Compound,
                                 levels = compound_order$Compound)

# -----------------------------
# 6) Mark present study
# -----------------------------
filtered_data <- filtered_data %>%
  mutate(is_present = ifelse(Study == "Present study", TRUE, FALSE))

# -----------------------------
# 7) Function: build raincloud plot
# -----------------------------
make_raincloud <- function(data, flip = TRUE, dotsize = 0.7) {
  
  p <- ggplot(data,
              aes(x = Compound, y = Percentage, fill = Compound)) +
    
    ggdist::stat_halfeye(
      adjust = 0.5, width = 0.7, justification = -0.2,
      .width = c(0.5, 0.95), point_colour = NA, alpha = 0.7
    ) +
    
    ggdist::stat_dots(
      data = data %>% dplyr::filter(is_present == FALSE),
      aes(x = Compound, y = Percentage),
      side = "left", justification = 1.1,
      dotsize = dotsize, binwidth = 0.7
    ) +
    
    geom_point(
      data = data %>% dplyr::filter(is_present == TRUE),
      aes(x = Compound, y = Percentage),
      color = "#FF1F1F", fill = "#FF1F1F",
      shape = 22, size = 3, stroke = 1.2
    ) +
    
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.5) +
    
    labs(
      title    = "Distribution of recurrent major compounds",
      subtitle = "Compounds occurring in ≥5 studies",
      x        = ifelse(flip, "", "Compound"),
      y        = "Percentage (%)"
    ) +
    
    theme_classic(base_size = 14) +
    theme(
      legend.position = "none",
      plot.title      = element_text(face = "bold", size = 16),
      plot.subtitle   = element_text(size = 12),
      axis.title.x    = element_text(face = "bold", size = 16),
      axis.title.y    = element_text(face = "bold", size = 16),
      axis.text.x     = element_text(angle = ifelse(flip, 0, 45),
                                     hjust = 1, face = "bold", size = 12),
      axis.text.y     = element_text(face = "bold", size = 12)
    )
  
  if (flip) p <- p + coord_flip()
  
  return(p)
}

# -----------------------------
# 8) Build both versions
# -----------------------------
p_vertical   <- make_raincloud(filtered_data, flip = TRUE,  dotsize = 0.7)
p_horizontal <- make_raincloud(filtered_data, flip = FALSE, dotsize = 1.7)

print(p_vertical)
print(p_horizontal)

# -----------------------------
# 9) Save both versions
# -----------------------------
ggsave(file.path(output_dir, "Raincloud_A_biebersteinii_vertical.pdf"),
       p_vertical, width = 10, height = 8, units = "in")

ggsave(file.path(output_dir, "Raincloud_A_biebersteinii_vertical.jpg"),
       p_vertical, width = 10, height = 8, units = "in", dpi = 300)

ggsave(file.path(output_dir, "Raincloud_A_biebersteinii_horizontal.pdf"),
       p_horizontal, width = 10, height = 8, units = "in")

ggsave(file.path(output_dir, "Raincloud_A_biebersteinii_horizontal.jpg"),
       p_horizontal, width = 10, height = 8, units = "in", dpi = 300)

# -----------------------------
# End
# -----------------------------
message("✅ Raincloud plots for A. biebersteinii finished. Outputs in: ",
        normalizePath(output_dir))