# ============================================================
# CoDA analysis of Achillea millefolium essential oils
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Compositional data analysis (CoDA) of essential
#              oil compounds in A. millefolium: frequency,
#              descriptive stats, CZM zero replacement,
#              CLR transformation, correlation, PCA, clustering,
#              heatmap, and chemotype assignment (k = 2).
# ============================================================
###################################################
# CoDA analysis of Achillea millefolium essential oils
###################################################
library(tidyverse)
library(purrr)
library(compositions)
library(zCompositions)
library(pheatmap)
library(corrplot)
library(readxl)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

###################################################
# 2. خواندن داده از فایل اکسل
###################################################
mille_data <- read_excel(file.path(data_dir, "A_millefolium.xlsx"))

###################################################
# 3. Parse percents (ranges → میانگین)
###################################################
parse_percent <- function(x){
  x <- trimws(x)
  x <- gsub("–","-",x)
  if(grepl("-", x)){
    parts <- strsplit(x, "-")[[1]]
    parts <- parts[parts != ""]
    mean(as.numeric(parts))
  } else {
    as.numeric(x)
  }
}

mille_data <- mille_data %>%
  mutate(Sample = paste0("S", row_number())) %>%
  relocate(Sample)

mille_long <- mille_data %>%
  separate_rows(Compounds, Percents, sep = ",") %>%
  mutate(
    Compound = str_trim(Compounds),
    Percent_str = str_trim(Percents),
    Value = map_dbl(Percent_str, parse_percent)
  )

###################################################
# 4. Wide composition matrix
###################################################
mille_wide <- mille_long %>%
  mutate(Compound = make.names(Compound)) %>%
  dplyr::select(Sample, Compound, Value) %>%
  pivot_wider(names_from = Compound,
              values_from = Value,
              values_fill = 0) %>%
  column_to_rownames("Sample")

oil <- as.data.frame(mille_wide)

###################################################
# 5. Frequency of occurrence
###################################################
presence <- colSums(oil > 0)
presence_percent <- round(presence / nrow(oil) * 100, 1)
frequency <- data.frame(
  Compound = names(oil),
  Frequency = presence,
  Presence_Percent = presence_percent
)
print(frequency)

###################################################
# 6. Descriptive statistics
###################################################
desc <- data.frame(
  Compound = names(oil),
  Mean = round(colMeans(oil), 2),
  Median = round(apply(oil, 2, median), 2),
  Min = apply(oil, 2, min),
  Max = apply(oil, 2, max)
)
print(desc)

###################################################
# 7. Zero replacement (CZM)
###################################################
oil_nozero <- cmultRepl(oil, method = "CZM")

###################################################
# 8. CLR transformation
###################################################
oil_clr <- clr(acomp(oil_nozero))
oil_clr <- as.data.frame(oil_clr)
print(round(oil_clr, 3))

###################################################
# 9. Distance matrix
###################################################
dist_matrix <- dist(oil_clr)
print(as.matrix(dist_matrix))

###################################################
# 10. Correlation matrix
###################################################
cor_matrix <- cor(oil_clr)
print(round(cor_matrix, 2))

corrplot(cor_matrix, method = "color")

corrplot(cor_matrix,
         method = "color",
         col = colormap::colormap(colormap = "jet", nshades = 200),
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.7,
         number.digits = 2)

# -----------------------------
# 11. Save correlation plot (PDF + JPEG)
# -----------------------------
pdf(file.path(output_dir, "millefolium_correlation_plot.pdf"),
    width = 9, height = 9)
corrplot(cor_matrix,
         method = "color",
         col = colormap::colormap(colormap = "jet", nshades = 200),
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.65,
         number.digits = 2)
dev.off()

jpeg(file.path(output_dir, "millefolium_correlation_plot.jpeg"),
     width = 2400, height = 2400, res = 300)
corrplot(cor_matrix,
         method = "color",
         col = colormap::colormap(colormap = "jet", nshades = 200),
         tl.col = "black",
         tl.srt = 45,
         tl.cex = 0.8,
         addCoef.col = "black",
         number.cex = 0.65,
         number.digits = 2)
dev.off()