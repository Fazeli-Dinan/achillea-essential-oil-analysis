# ============================================================
# UpSet plot and Venn diagram for Achillea nobilis
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Visualizes the co-occurrence of frequent essential
#              oil compounds (present in >= 5 studies) using an
#              UpSet plot, and a 4-set Venn diagram for selected
#              key compounds (auto-detected via pattern matching).
# Note: The compound separator here is ", " (comma + space),
#       unlike the other two species.
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(readxl)
library(tidyverse)
library(ComplexUpset)
library(ggVennDiagram)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 2) Read data
# -----------------------------
df_raw <- read_excel(file.path(data_dir, "A_nobilis.xlsx"))

df <- df_raw %>%
  select(Study, Compounds) %>%
  rename(Text = Compounds)

# -----------------------------
# 3) Helper: extract compounds from text
#    NOTE: separator is ", " (comma + space)
# -----------------------------
extract_compounds <- function(text_vec) {
  text_vec %>%
    str_split(", ") %>%
    map(~ str_trim(.x)) %>%
    map(~ str_replace(.x, "\\s*\\(.*\\)$", "")) %>%
    map(~ .x[.x != ""])
}

# -----------------------------
# 4) Build long format
# -----------------------------
df_long <- df %>%
  mutate(Comp_list = extract_compounds(Text)) %>%
  unnest(Comp_list) %>%
  rename(Compound = Comp_list) %>%
  distinct()

# -----------------------------
# 5) Build binary matrix
# -----------------------------
binary_matrix <- df_long %>%
  mutate(value = 1L) %>%
  pivot_wider(
    names_from  = Compound,
    values_from = value,
    values_fill = 0L
  )

# -----------------------------
# 6) Frequent compounds (>= 5 studies)
# -----------------------------
freq <- df_long %>%
  count(Compound, sort = TRUE)

top_compounds <- freq %>%
  filter(n >= 5) %>%
  pull(Compound)

binary_top <- binary_matrix %>%
  select(Study, all_of(top_compounds)) %>%
  filter(rowSums(across(-Study)) > 0)

message("Frequent compounds (>= 5 studies):")
print(top_compounds)

# -----------------------------
# 7) UpSet plot
# -----------------------------
p_top <- upset(
  binary_top,
  intersect = top_compounds,
  name = "Frequent Compounds (>= 5 studies)",
  width_ratio = 0.25,
  sort_sets = "descending",
  sort_intersections_by = "cardinality",
  set_sizes = upset_set_size(
    geom = geom_bar(fill = "#2ca02c", color = "black")
  ),
  base_annotations = list(
    "Intersection Size" = intersection_size(counts = TRUE)
  )
) +
  ggtitle("Achillea nobilis - Frequent Essential Oil Compounds (>= 5 studies)") +
  theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text   = element_text(face = "bold", color = "black", size = 10),
    axis.title  = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(p_top)

ggsave(file.path(output_dir, "A_nobilis_UpSet_top5.pdf"),
       p_top, width = 14, height = 10, bg = "white")

ggsave(file.path(output_dir, "A_nobilis_UpSet_top5.png"),
       p_top, width = 14, height = 10, dpi = 600, bg = "white")

# -----------------------------
# 8) Venn diagram (4 key compounds, auto-detected)
# -----------------------------
message("Column names available in binary_top:")
print(colnames(binary_top))

venn_targets <- c(
  grep("1[,.-]?8", colnames(binary_top), value = TRUE, ignore.case = TRUE)[1],
  grep("Thujone",   colnames(binary_top), value = TRUE, ignore.case = TRUE)[1],
  grep("^Camphor$", colnames(binary_top), value = TRUE, ignore.case = TRUE)[1],
  grep("Artemisia", colnames(binary_top), value = TRUE, ignore.case = TRUE)[1]
)

message("Venn targets found:")
print(venn_targets)

venn_list <- lapply(venn_targets, function(comp) {
  if (!is.na(comp) && comp %in% colnames(binary_top)) {
    binary_top %>%
      filter(.data[[comp]] == 1) %>%
      pull(Study) %>%
      unique()
  } else {
    character(0)
  }
})
names(venn_list) <- venn_targets

# Remove empty sets (if a compound was not found)
venn_list <- venn_list[sapply(venn_list, length) > 0]

p_venn <- ggVennDiagram(
  venn_list,
  label_alpha = 0,
  edge_size   = 1,
  set_size    = 5
) +
  scale_fill_gradient(low = "#FFFFFF", high = "#0072B2") +
  ggtitle("Achillea nobilis - Venn Diagram (4 Key Compounds)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

print(p_venn)

ggsave(file.path(output_dir, "A_nobilis_ggVenn.pdf"),
       p_venn, width = 10, height = 10, bg = "white")

ggsave(file.path(output_dir, "A_nobilis_ggVenn.png"),
       p_venn, width = 10, height = 10, dpi = 600, bg = "white")

# -----------------------------
# End
# -----------------------------
message("✅ UpSet and Venn plots for A. nobilis finished. Outputs in: ",
        normalizePath(output_dir))
