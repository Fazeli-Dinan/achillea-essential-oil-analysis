# ============================================================
# Probit regression and dose-response curves for Achillea spp.
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Probit regression analysis of larval mortality
#              data for three Achillea species (A. nobilis,
#              A. millefolium, A. biebersteinii). Produces:
#              - Individual probit regression plots per species
#              - Comparative probit regression plot
#              - Comparative sigmoidal dose-response curve
# Note: Raw data are embedded below as a tribble for
#       reproducibility. For long-term use, consider moving
#       them to `data/raw/probit_data.csv`.
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 2) Raw data (embedded)
#    Note: "A.beibers" is the informal label for A. biebersteinii
# -----------------------------
raw_data <- tribble(
  ~plant,       ~dose, ~replicate, ~total, ~dead,
  # A. nobilis
  "A.nobilis", 100, 1, 25, 2,
  "A.nobilis", 120, 1, 25, 10,
  "A.nobilis", 140, 1, 25, 10,
  "A.nobilis", 160, 1, 25, 14,
  "A.nobilis", 180, 1, 25, 20,
  "A.nobilis", 200, 1, 25, 13,
  "A.nobilis", 220, 1, 25, 20,
  "A.nobilis", 100, 2, 25, 0,
  "A.nobilis", 120, 2, 25, 5,
  "A.nobilis", 140, 2, 25, 13,
  "A.nobilis", 160, 2, 25, 19,
  "A.nobilis", 180, 2, 25, 18,
  "A.nobilis", 200, 2, 25, 22,
  "A.nobilis", 220, 2, 25, 24,
  "A.nobilis", 100, 3, 25, 5,
  "A.nobilis", 120, 3, 25, 4,
  "A.nobilis", 140, 3, 25, 16,
  "A.nobilis", 160, 3, 25, 16,
  "A.nobilis", 180, 3, 25, 15,
  "A.nobilis", 200, 3, 25, 23,
  "A.nobilis", 220, 3, 25, 24,
  # A. millefolium
  "A.millefolium", 100, 1, 25, 0,
  "A.millefolium", 120, 1, 25, 0,
  "A.millefolium", 140, 1, 25, 2,
  "A.millefolium", 160, 1, 25, 12,
  "A.millefolium", 180, 1, 25, 10,
  "A.millefolium", 200, 1, 25, 23,
  "A.millefolium", 220, 1, 25, 25,
  "A.millefolium", 100, 2, 25, 0,
  "A.millefolium", 120, 2, 25, 1,
  "A.millefolium", 140, 2, 25, 2,
  "A.millefolium", 160, 2, 25, 7,
  "A.millefolium", 180, 2, 25, 19,
  "A.millefolium", 200, 2, 25, 22,
  "A.millefolium", 220, 2, 25, 24,
  "A.millefolium", 100, 3, 25, 0,
  "A.millefolium", 120, 3, 25, 0,
  "A.millefolium", 140, 3, 25, 3,
  "A.millefolium", 160, 3, 25, 8,
  "A.millefolium", 180, 3, 25, 16,
  "A.millefolium", 200, 3, 25, 17,
  "A.millefolium", 220, 3, 25, 25,
  # A. biebersteinii (no 80 dose)
  "A.beibers", 100, 1, 25, 0,
  "A.beibers", 120, 1, 25, 2,
  "A.beibers", 140, 1, 25, 4,
  "A.beibers", 160, 1, 25, 10,
  "A.beibers", 180, 1, 25, 21,
  "A.beibers", 200, 1, 25, 24,
  "A.beibers", 220, 1, 25, 25,
  "A.beibers", 100, 2, 25, 2,
  "A.beibers", 120, 2, 25, 3,
  "A.beibers", 140, 2, 25, 5,
  "A.beibers", 160, 2, 25, 11,
  "A.beibers", 180, 2, 25, 17,
  "A.beibers", 200, 2, 25, 25,
  "A.beibers", 220, 2, 25, 25,
  "A.beibers", 100, 3, 25, 0,
  "A.beibers", 120, 3, 25, 1,
  "A.beibers", 140, 3, 25, 3,
  "A.beibers", 160, 3, 25, 13,
  "A.beibers", 180, 3, 25, 21,
  "A.beibers", 200, 3, 25, 22,
  "A.beibers", 220, 3, 25, 25
)

# -----------------------------
# 3) Aggregate by plant × dose
# -----------------------------
df <- raw_data %>%
  group_by(plant, dose) %>%
  summarise(total = sum(total), dead = sum(dead), .groups = "drop") %>%
  mutate(percent = round(100 * dead / total, 1))

# -----------------------------
# 4) Probit lookup table
# -----------------------------
probit_table <- tribble(
  ~percent, ~probit,
  1, 2.67, 2, 2.95, 3, 3.12, 4, 3.25, 5, 3.36, 6, 3.45, 7, 3.52, 8, 3.59, 9, 3.66,
  10, 3.72, 11, 3.77, 12, 3.82, 13, 3.87, 14, 3.92, 15, 3.96, 16, 4.01, 17, 4.05,
  18, 4.08, 19, 4.12, 20, 4.16, 21, 4.19, 22, 4.23, 23, 4.26, 24, 4.29, 25, 4.33,
  26, 4.36, 27, 4.39, 28, 4.42, 29, 4.45, 30, 4.48, 31, 4.50, 32, 4.53, 33, 4.56,
  34, 4.59, 35, 4.61, 36, 4.64, 37, 4.67, 38, 4.69, 39, 4.72, 40, 4.75, 41, 4.77,
  42, 4.80, 43, 4.82, 44, 4.85, 45, 4.87, 46, 4.90, 47, 4.92, 48, 4.95, 49, 4.97,
  50, 5.00, 51, 5.03, 52, 5.05, 53, 5.08, 54, 5.10, 55, 5.13, 56, 5.15, 57, 5.18,
  58, 5.20, 59, 5.23, 60, 5.25, 61, 5.28, 62, 5.31, 63, 5.33, 64, 5.36, 65, 5.39,
  66, 5.41, 67, 5.44, 68, 5.47, 69, 5.50, 70, 5.52, 71, 5.55, 72, 5.58, 73, 5.61,
  74, 5.64, 75, 5.67, 76, 5.71, 77, 5.74, 78, 5.77, 79, 5.81, 80, 5.84, 81, 5.88,
  82, 5.92, 83, 5.95, 84, 5.99, 85, 6.04, 86, 6.08, 87, 6.13, 88, 6.18, 89, 6.23,
  90, 6.28, 91, 6.34, 92, 6.41, 93, 6.48, 94, 6.55, 95, 6.64, 96, 6.75, 97, 6.88,
  98, 7.05, 99, 7.33
)

find_probit <- function(pct) {
  if (pct <= 1) return(2.67)
  if (pct >= 99) return(7.33)
  probit_table$probit[which.min(abs(probit_table$percent - pct))]
}

df <- df %>%
  rowwise() %>%
  mutate(
    probit   = find_probit(percent),
    log_dose = log10(dose)
  ) %>%
  ungroup()

# -----------------------------
# 5) Individual probit plots per species
# -----------------------------
species_plots <- df %>%
  group_by(plant) %>%
  group_split() %>%
  lapply(function(df_sub) {
    plant_name <- unique(df_sub$plant)
    
    p <- ggplot(df_sub, aes(x = log_dose, y = probit)) +
      geom_point(size = 3, color = "darkred") +
      geom_smooth(method = "lm", se = TRUE,
                  color = "blue", fill = "lightblue", alpha = 0.3) +
      labs(
        title = paste("Probit Regression:", plant_name),
        x     = "Log10(Dose)",
        y     = "Probit(Mortality)"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line          = element_line(color = "black"),
        axis.ticks         = element_line(color = "black", linewidth = 1),
        axis.ticks.length  = unit(0.2, "cm"),
        axis.text          = element_text(size = 14),
        axis.title         = element_text(size = 16)
      )
    
    ggsave(file.path(output_dir, paste0(gsub(" ", "_", plant_name), "_probit.jpg")),
           p, width = 8, height = 6, units = "in", dpi = 300, bg = "white")
    
    p
  })

# -----------------------------
# 6) Comparative probit plot
# -----------------------------
p_compare <- ggplot(df, aes(x = log_dose, y = probit, color = plant)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", aes(fill = plant), se = TRUE, alpha = 0.2) +
  labs(
    title = "Comparative Probit Regression",
    x     = "Log10(Dose)",
    y     = "Probit(Mortality)"
  ) +
  scale_color_manual(values = c("A.nobilis"     = "#E31A1C",
                                "A.millefolium" = "#1F78B4",
                                "A.beibers"     = "#33A02C")) +
  scale_fill_manual(values  = c("A.nobilis"     = "#E31A1C",
                                "A.millefolium" = "#1F78B4",
                                "A.beibers"     = "#33A02C")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line         = element_line(color = "black"),
    axis.ticks        = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text         = element_text(size = 16),
    axis.title        = element_text(size = 18),
    legend.position   = c(0.8, 0.2)
  )

ggsave(file.path(output_dir, "comparative_probit.jpg"),
       p_compare, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

# -----------------------------
# 7) Comparative sigmoidal dose-response plot
# -----------------------------
p_sigmoid <- ggplot(df, aes(x = dose, y = percent, color = plant)) +
  geom_point(size = 2) +
  geom_smooth(method = "loess", aes(fill = plant), se = TRUE,
              alpha = 0.2, span = 1) +
  labs(
    title = "Dose-Response Curve (Sigmoidal)",
    x     = "Dose (mg/L)",
    y     = "Mortality (%)"
  ) +
  scale_color_manual(values = c("A.nobilis"     = "#E31A1C",
                                "A.millefolium" = "#1F78B4",
                                "A.beibers"     = "#33A02C")) +
  scale_fill_manual(values  = c("A.nobilis"     = "#E31A1C",
                                "A.millefolium" = "#1F78B4",
                                "A.beibers"     = "#33A02C")) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line         = element_line(color = "black"),
    axis.ticks        = element_line(color = "black", linewidth = 1),
    axis.ticks.length = unit(0.2, "cm"),
    axis.text         = element_text(size = 14),
    axis.title        = element_text(size = 16),
    legend.position   = c(0.8, 0.2)
  )

ggsave(file.path(output_dir, "dose_response_sigmoidal.jpg"),
       p_sigmoid, width = 8, height = 6, units = "in", dpi = 300, bg = "white")

# -----------------------------
# End
# -----------------------------
message("✅ Probit regression plots finished. Outputs in: ",
        normalizePath(output_dir))
