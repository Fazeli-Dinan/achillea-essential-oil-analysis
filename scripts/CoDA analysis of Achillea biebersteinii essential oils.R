# ============================================================
# CoDA analysis of Achillea biebersteinii essential oils
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Compositional data analysis (CoDA) of essential
#              oil compounds in A. biebersteinii: frequency,
#              descriptive stats, zero replacement (CZM),
#              CLR transformation, correlation, PCA, clustering,
#              heatmap, and chemotype assignment.
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(tidyverse)
library(purrr)
library(compositions)
library(zCompositions)
library(pheatmap)
library(corrplot)
library(colormap)   # added: used for corrplot color palette
library(readxl)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
# -----------------------------
# 2) Helper functions
# -----------------------------
standardize_names <- function(x) {
  x <- str_trim(x)
  x <- str_replace_all(
    x,
    regex("1-8-Cineole|1,8 Cineole|1,8-cineole|Eucalyptol|eucalyptol",
          ignore_case = TRUE),
    "1,8-Cineole"
  )
  return(x)
}

parse_percent <- function(x) {
  x <- trimws(x)
  x <- gsub("–", "-", x)
  if (grepl("-", x)) {
    parts <- strsplit(x, "-")[[1]]
    parts <- parts[parts != ""]
    mean(as.numeric(parts))
  } else {
    as.numeric(x)
  }
}

# -----------------------------
# 3) Read data
# -----------------------------
bieber_data <- read_excel(file.path(data_dir, "A. biebersteinii.xlsx"))

print(colnames(bieber_data))
print(head(bieber_data))

# -----------------------------
# 4) Parse percents and build long/wide matrices
# -----------------------------
bieber_data <- bieber_data %>%
  mutate(Sample = paste0("S", row_number())) %>%
  relocate(Sample)

bieber_long <- bieber_data %>%
  separate_rows(Compounds, Percents, sep = ",") %>%
  mutate(
    Compound    = standardize_names(Compounds),
    Percent_str = str_trim(Percents),
    Value       = map_dbl(Percent_str, parse_percent)
  )

bieber_wide <- bieber_long %>%
  dplyr::select(Sample, Compound, Value) %>%
  pivot_wider(names_from = Compound,
              values_from = Value,
              values_fill = 0) %>%
  column_to_rownames("Sample")

oil <- as.data.frame(bieber_wide)

# -----------------------------
# 5) Frequency of occurrence
# -----------------------------
presence         <- colSums(oil > 0)
presence_percent <- round(presence / nrow(oil) * 100, 1)

frequency <- data.frame(
  Compound         = names(oil),
  Frequency        = presence,
  Presence_Percent = presence_percent
)
print(frequency)

write.csv(frequency,
          file.path(output_dir, "biebersteinii_frequency.csv"),
          row.names = FALSE)

# -----------------------------
# 6) Descriptive statistics
# -----------------------------
desc <- data.frame(
  Compound = names(oil),
  Mean     = round(colMeans(oil), 2),
  Median   = round(apply(oil, 2, median), 2),
  Min      = apply(oil, 2, min),
  Max      = apply(oil, 2, max)
)
print(desc)

write.csv(desc,
          file.path(output_dir, "biebersteinii_descriptive_stats.csv"),
          row.names = FALSE)

# -----------------------------
# 7) Zero replacement (CZM) and CLR transformation
# -----------------------------
oil_nozero <- cmultRepl(oil, method = "CZM")
oil_clr    <- clr(acomp(oil_nozero))
oil_clr    <- as.data.frame(oil_clr)
print(round(oil_clr, 3))

write.csv(round(oil_clr, 3),
          file.path(output_dir, "biebersteinii_clr.csv"))

# -----------------------------
# 8) Distance and correlation matrices
# -----------------------------
dist_matrix <- dist(oil_clr)
print(as.matrix(dist_matrix))

cor_matrix <- cor(oil_clr)
print(round(cor_matrix, 2))

# -----------------------------
# 9) Correlation plot (saved once, in two formats)
# -----------------------------
plot_corr <- function() {
  corrplot(cor_matrix,
           method       = "color",
           col          = colormap::colormap(colormap = "jet", nshades = 200),
           tl.col       = "black",
           tl.srt       = 45,
           addCoef.col  = "black",
           number.cex   = 0.7,
           number.digits = 2)
}

pdf(file.path(output_dir, "biebersteinii_correlation_plot.pdf"),
    width = 7, height = 7)
plot_corr()
dev.off()

jpeg(file.path(output_dir, "biebersteinii_correlation_plot.jpeg"),
     width = 2000, height = 2000, res = 300)
plot_corr()
dev.off()

corrplot(cor_matrix, method = "color", col = colormap::colormap(colormap = "jet", nshades = 200), tl.col = "black", tl.srt = 45, tl.cex = 0.8, addCoef.col = "black", number.cex = 0.65, number.digits = 2)

# -----------------------------
# End
# -----------------------------
message("✅ CoDA analysis for A. biebersteinii finished. Outputs in: ",
        normalizePath(output_dir))
