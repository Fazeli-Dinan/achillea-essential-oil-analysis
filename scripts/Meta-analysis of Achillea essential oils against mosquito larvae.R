# ============================================================
# Meta-analysis of Achillea essential oils against mosquito larvae
# Author: [Dr. Mahmoud Fazeli-Dinan]
# Date: 2026
# Description: Random-effects meta-analysis (REML) of log(LC50)
#              values from studies testing Achillea extracts
#              against four mosquito species in larval stage.
#              Produces a subgroup forest plot with prediction
#              interval (PDF + PNG).
# ============================================================

# -----------------------------
# 0) Load packages
# -----------------------------
library(readxl)
library(dplyr)
library(meta)
library(stringr)

# -----------------------------
# 1) Paths (relative to project root)
# -----------------------------
data_dir   <- file.path("data", "raw")
output_dir <- "output"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# -----------------------------
# 2) Read data
# -----------------------------
file_path <- file.path(data_dir, "Achillea_insecticidal_final.xlsx")
df <- read_excel(file_path, .name_repair = "minimal")

names(df) <- c(
  "Study_ID", "Year", "Plant_species", "Species", "Stage",
  "LC50_raw", "LC50_Lower", "LC50_Upper",
  "LC90_raw", "LC90_Lower", "LC90_Upper",
  "LC95_raw", "LC95_Lower", "LC95_Upper",
  "LC99_raw", "LC99_Lower", "LC99_Upper",
  "Target_group", "Major_Compounds"
)

df <- df %>%
  mutate(
    Species       = str_trim(Species),
    Plant_species = str_trim(Plant_species),
    Study_ID      = str_trim(Study_ID)
  )

# -----------------------------
# 3) Unit extraction
# -----------------------------
extract_num <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || x == "" ||
      grepl("^NA$|^N/A$|^–$|^-$|^Not reported$", x, ignore.case = TRUE)) {
    return(NA_real_)
  }
  if (grepl("%|w/v|v/v", x, ignore.case = TRUE)) return(NA_real_)
  suppressWarnings(as.numeric(str_extract(x, "[0-9]+\\.?[0-9]*")))
}

df <- df %>%
  mutate(
    LC50_num = sapply(LC50_raw, extract_num),
    LCI_num  = sapply(LC50_Lower, extract_num),
    UCI_num  = sapply(LC50_Upper, extract_num)
  )

df_clean <- df %>%
  filter(!is.na(LC50_num), LC50_num > 0,
         !is.na(LCI_num),  LCI_num  > 0,
         !is.na(UCI_num),  UCI_num  > 0,
         UCI_num > LCI_num) %>%
  mutate(
    logLC50 = log(LC50_num),
    SE      = (log(UCI_num) - log(LCI_num)) / (2 * 1.96)
  ) %>%
  filter(!is.na(SE), SE > 0)

# -----------------------------
# 4) Mosquito larvae subset
# -----------------------------
df_ma <- df_clean %>%
  filter(
    Species %in% c("Aedes aegypti", "Aedes albopictus",
                   "Culex pipiens", "Anopheles stephensi"),
    Stage == "Larvae"
  ) %>%
  mutate(
    Subgroup = factor(
      Species,
      levels = c("Anopheles stephensi", "Aedes aegypti",
                 "Aedes albopictus", "Culex pipiens")
    ),
    Study_label = paste0(Study_ID, " - ", Plant_species)
  )

# -----------------------------
# 5) Meta-analysis (random-effects, REML)
# -----------------------------
m_larv <- metagen(
  TE         = logLC50,
  seTE       = SE,
  studlab    = Study_label,
  data       = df_ma,
  sm         = "MRAW",
  method.tau = "REML",
  subgroup   = Subgroup,
  common     = FALSE,
  random     = TRUE
)

print(summary(m_larv))

# -----------------------------
# 6) Forest plot function (PDF + PNG)
# -----------------------------
draw_forest <- function(meta_obj) {
  forest(
    meta_obj,
    sortvar             = TE,
    prediction          = TRUE,
    print.byvar         = TRUE,
    print.subgroup.name = TRUE,
    col.square          = "blue",
    col.square.lines    = "black",
    col.diamond         = "red",
    col.diamond.lines   = "black",
    fontsize            = 9,
    xlab                = "Ln(LC50)",
    label.left          = "More potent",
    label.right         = "Less potent",
    leftcols            = c("studlab"),
    rightcols           = c("effect", "ci", "w.random"),
    spacing             = 1.3
  )
}

pdf(file.path(output_dir, "Fig2_Forest_MosquitoLarvae.pdf"),
    width = 11, height = 6.5)
draw_forest(m_larv)
dev.off()

png(file.path(output_dir, "Fig2_Forest_MosquitoLarvae.png"),
    width = 3300, height = 1950, res = 300)
draw_forest(m_larv)
dev.off()
forest(m_larv)

# -----------------------------
# End
# -----------------------------
message("✅ Meta-analysis forest plot finished. Outputs in: ",
        normalizePath(output_dir))
