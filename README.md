
# Code for: Essential Oil Analysis of Three Achillea Species

This repository contains the R code and data for the analysis of essential oils from three *Achillea* species (*A. biebersteinii*, *A. millefolium*, and *A. nobilis*), including distribution mapping, compositional data analysis (CoDA), raincloud plots, UpSet/Venn diagrams, meta-analysis, and probit regression.

**Associated Manuscript:**
> **[Authors]. (Year). [Manuscript Title]. Journal Name, Volume(Issue), Pages. DOI: [DOI]**


## Repository Structure

.
├── README.md
├── LICENSE
├── .gitignore
├── .zenodo.json
├── data/
 ├── README.md
 └── raw/
├── scripts/
 ├── 01_make_maps.R
 ├── 02_coda_biebersteinii.R
 ├── 03_coda_millefolium.R
 ├── 04_coda_nobilis.R
 ├── 05_raincloud_biebersteinii.R
 ├── 06_raincloud_millefolium.R
 ├── 07_raincloud_nobilis.R
 ├── 08_upset_venn_biebersteinii.R
 ├── 09_upset_venn_millefolium.R
 ├── 10_upset_venn_nobilis.R
 ├── 11_meta_analysis_forest.R
 └── 12_probit_regression.R
└── output/


## Requirements

- **R** (version 4.0 or higher)
- Required R packages:

| Package | Purpose |
| :--- | :--- |
| `tidyverse`, `readxl` | Data manipulation and reading files |
| `sf`, `rnaturalearth`, `rnaturalearthdata` | Spatial mapping |
| `patchwork` | Combining plots |
| `compositions`, `zCompositions` | Compositional data analysis |
| `pheatmap`, `corrplot`, `colormap` | Heatmaps and correlation plots |
| `ggdist` | Raincloud plots |
| `ComplexUpset`, `ggVennDiagram` | UpSet and Venn diagrams |
| `meta` | Meta-analysis |
| `viridis`, `fields`, `scico`, `colorspace` | Journal-quality color palettes |

Install the required packages using:

r
install.packages(c("tidyverse", "readxl", "sf", "rnaturalearth", 
                   "rnaturalearthdata", "patchwork", "compositions", 
                   "zCompositions", "pheatmap", "corrplot", "colormap", 
                   "ggdist", "ComplexUpset", "ggVennDiagram", "meta", 
                   "viridis", "fields", "scico", "colorspace"))
How to Run
Clone this repository:

bash
git clone https://github.com/Fazeli-Dinan/achillea-essential-oil-analysis.git
Open R or RStudio and set the working directory to the repository root.

Run the scripts sequentially:

r
source("./scripts/01_make_maps.R")
source("./scripts/02_coda_biebersteinii.R")
#  run remaining scripts in order ...
Note: The scripts use relative paths (data/raw/ and output/). Ensure the working directory is set to the repository root.

Output
Each script generates specific figures and tables in the output/ folder:

Script	Description	Key Outputs
01_make_maps.R	Global distribution heatmaps	Achillea_maps_heatmap_3x1.{png,pdf}
02, 03, 04_coda...	Compositional data analysis & PCA	[species]_correlation_plot.{pdf,jpeg}, etc.
05, 06, 07_raincloud...	Raincloud plots of major compounds	Raincloud_A_[species]_vertical.{pdf,png}
08, 09, 10_upset_venn...	UpSet & Venn diagrams	A_[species]_UpSet_top5.pdf, A_[species]_ggVenn.pdf
11_meta_analysis_forest.R	Meta-analysis of LC50	Fig2_Forest_MosquitoLarvae.{pdf,png}
12_probit_regression.R	Probit regression & dose-response	comparative_probit.jpg, dose_response_sigmoidal.jpg
Data Source
The data consist of country-level occurrence records, essential oil compound percentages, and insecticidal LC50 values against mosquito larvae. All files are located in data/raw/. See data/README.md for detailed descriptions.

License
Code: MIT License — see the LICENSE file for details.

Data: Creative Commons Attribution 4.0 International (CC-BY-4.0).

Citation
If you use this code or data in your research, please cite the associated manuscript:

[Authors]. (Year). [Manuscript Title]. Journal Name, Volume(Issue), Pages. DOI: [DOI]

Contact
For questions or requests, please contact the corresponding author:

Dr. Mahmoud Fazeli-Dinan

Associate Professor of Entomology

Email: fazelidinan@gmail.com

Department of Medical Entomology, Mazandaran University of Medical Sciences

Zenodo DOI
[DOI badge will be added here after Zenodo archiving]

Last updated: 2026
