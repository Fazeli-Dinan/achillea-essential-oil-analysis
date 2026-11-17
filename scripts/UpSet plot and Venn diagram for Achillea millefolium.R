# ============================================================
# UpSet plot and Venn diagram for Achillea millefolium
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Visualizes the co-occurrence of frequent essential
#              oil compounds (present in >= 5 studies, top 20)
#              using an UpSet plot (min intersection size = 3),
#              and a 4-set Venn diagram for selected key compounds.
# Note: `venn_targets` must be adjusted after inspecting the
#       printed `top_compounds` vector (see Section 5).
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
df_raw <- read_excel(file.path(data_dir, "A_millefolium.xlsx"))

df <- df_raw %>%
  select(Study = 1, Text = 4)   # column 1 = Study, column 4 = Compounds

# -----------------------------
# 3) Helper: extract compounds from text
# -----------------------------
extract_compounds <- function(text_vec) {
  text_vec %>%
    str_split(",") %>%
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
# 6) Frequent compounds (>= 5 studies, top 20)
# -----------------------------
freq <- df_long %>%
  count(Compound, sort = TRUE)

top_compounds <- freq %>%
  filter(n >= 5) %>%
  slice_head(n = 20) %>%
  pull(Compound)

binary_top <- binary_matrix %>%
  select(Study, all_of(top_compounds)) %>%
  filter(rowSums(across(-Study)) > 0)

message("Frequent compounds (>= 5 studies, top 20):")
print(top_compounds)

# -----------------------------
# 7) UpSet plot
# -----------------------------
p_top <- upset(
  binary_top,
  intersect = top_compounds,
  name = "Frequent Compounds (>= 5 studies)",
  width_ratio = 0.25,
  min_size = 3,
  sort_sets = "descending",
  sort_intersections_by = "cardinality",
  set_sizes = upset_set_size(
    geom = geom_bar(fill = "#2ca02c", color = "black")
  ),
  base_annotations = list(
    "Intersection Size" = intersection_size(counts = TRUE)
  )
) +
  ggtitle("Achillea millefolium - Frequent Essential Oil Compounds (>= 5 studies)") +
  theme_bw() +
  theme(
    plot.title  = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text   = element_text(face = "bold", color = "black", size = 10),
    axis.title  = element_text(face = "bold", size = 12),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )

print(p_top)

ggsave(file.path(output_dir, "A_millefolium_UpSet_top5.pdf"),
       p_top, width = 14, height = 10, bg = "white")

ggsave(file.path(output_dir, "A_millefolium_UpSet_top5.png"),
       p_top, width = 14, height = 10, dpi = 600, bg = "white")

# -----------------------------
# 8) Venn diagram (4 key compounds)
# -----------------------------
# IMPORTANT: adjust `venn_targets` after inspecting `top_compounds`
venn_targets <- c("1-8-Cineole", "β-Pinene", "Sabinene", "Camphor")

message("Checking Venn targets against available compounds:")
print(venn_targets %in% colnames(binary_top))

venn_list <- lapply(venn_targets, function(comp) {
  if (comp %in% colnames(binary_top)) {
    binary_top %>%
      filter(.data[[comp]] == 1) %>%
      pull(Study) %>%
      unique()
  } else {
    character(0)
  }
})
names(venn_list) <- venn_targets

p_venn <- ggVennDiagram(
  venn_list,
  label_alpha = 0,
  edge_size   = 1,
  set_size    = 5
) +
  scale_fill_gradientn(
    colours = c("#fff5eb", "#fdae6b", "#fd8d3c", "#e6550d", "#a63603")
  ) +
  ggtitle("Achillea millefolium - Venn Diagram (4 Key Compounds)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

print(p_venn)

ggsave(file.path(output_dir, "A_millefolium_ggVenn.pdf"),
       p_venn, width = 10, height = 10, bg = "white")

ggsave(file.path(output_dir, "A_millefolium_ggVenn.png"),
       p_venn, width = 10, height = 10, dpi = 600, bg = "white")

# -----------------------------
# End
# -----------------------------
message("✅ UpSet and Venn plots for A. millefolium finished. Outputs in: ",
        normalizePath(output_dir))
