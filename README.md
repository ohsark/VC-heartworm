# Heartworm Analysis - Code Documentation

**Version:** 1.0  

This project provides a comprehensive epidemiological analysis pipeline for studying heartworm disease (*Dirofilaria immitis*) in canine populations across Australia. The pipeline processes veterinary consultation records to:

- Identify heartworm test results (positive/negative)
- Classify disease severity and treatment protocols
- Generate spatial and temporal disease distribution analyses
- Train machine learning models for automated case classification
- Produce publication-ready statistics and visualizations

### Modules
- **Rule-based Classification:** Uses carefully curated regex patterns to extract clinical information from free-text examination notes
- **Machine Learning:** Gradient Boosting Machine (GBM) classifier for automated positive/negative prediction
- **Spatial Analysis:** Postcode and Local Government Area (LGA) mapping of disease prevalence
- **Survival Analysis:** Time-to-death and time-to-negative conversion metrics
- **Dashboard Integration:** JSON exports for interactive dashboard visualization

| Software | Minimum Version | Purpose | Citation |
|----------|-----------------|---------|----------|
| **R** | ≥ 4.0.0 | Statistical computing environment | R Core Team (2024). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/ |
| **RStudio** | ≥ 1.4.0 | Integrated development environment (recommended) | Posit team (2024). RStudio: Integrated Development Environment for R. Posit Software, PBC, Boston, MA. https://posit.co/ |

### R Package Dependencies

#### Data Manipulation & Processing

| Package | Purpose | Citation |
|---------|---------|----------|
| **tidyverse** | Data manipulation, visualization, and workflow | Wickham H et al. (2019). Welcome to the tidyverse. Journal of Open Source Software, 4(43), 1686. https://doi.org/10.21105/joss.01686 |
| **dplyr** | Data manipulation grammar (part of tidyverse) | Wickham H et al. (2023). dplyr: A Grammar of Data Manipulation. R package. https://dplyr.tidyverse.org |
| **tidyr** | Data tidying (part of tidyverse) | Wickham H et al. (2024). tidyr: Tidy Messy Data. R package. https://tidyr.tidyverse.org |
| **readr** | Fast CSV reading (part of tidyverse) | Wickham H et al. (2024). readr: Read Rectangular Text Data. R package. https://readr.tidyverse.org |
| **stringr** | String manipulation (part of tidyverse) | Wickham H (2023). stringr: Simple, Consistent Wrappers for Common String Operations. R package. https://stringr.tidyverse.org |
| **purrr** | Functional programming (part of tidyverse) | Wickham H & Henry L (2023). purrr: Functional Programming Tools. R package. https://purrr.tidyverse.org |
| **readxl** | Excel file import | Wickham H & Bryan J (2023). readxl: Read Excel Files. R package. https://readxl.tidyverse.org |
| **lubridate** | Date-time manipulation | Grolemund G & Wickham H (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. https://www.jstatsoft.org/v40/i03/ |
| **janitor** | Data cleaning utilities | Firke S (2023). janitor: Simple Tools for Examining and Cleaning Dirty Data. R package. https://sfirke.github.io/janitor/ |

#### Machine Learning & Natural Language Processing

| Package | Purpose | Citation |
|---------|---------|----------|
| **gbm** | Gradient Boosting Machine implementation | Greenwell B et al. (2024). gbm: Generalized Boosted Regression Models. R package. https://github.com/gbm-developers/gbm |
| **caret** | Classification and regression training | Kuhn M (2008). Building Predictive Models in R Using the caret Package. Journal of Statistical Software, 28(5), 1-26. https://doi.org/10.18637/jss.v028.i05 |
| **SnowballC** | Porter's word stemming algorithm | Bouchet-Valat M (2023). SnowballC: Snowball Stemmers Based on the C 'libstemmer' UTF-8 Library. R package. https://CRAN.R-project.org/package=SnowballC |
| **e1071** | Support vector machines and statistical functions | Meyer D et al. (2023). e1071: Misc Functions of the Department of Statistics, Probability Theory Group. R package. https://CRAN.R-project.org/package=e1071 |
| **pROC** | ROC curve analysis and AUC calculation | Robin X et al. (2011). pROC: an open-source package for R and S+ to analyze and compare ROC curves. BMC Bioinformatics, 12, 77. https://doi.org/10.1186/1471-2105-12-77 |

#### Spatial Analysis & Mapping

| Package | Purpose | Citation |
|---------|---------|----------|
| **sf** | Simple features for spatial data | Pebesma E (2018). Simple Features for R: Standardized Support for Spatial Vector Data. The R Journal, 10(1), 439-446. https://doi.org/10.32614/RJ-2018-009 |
| **spdep** | Spatial dependence analysis | Bivand R et al. (2024). spdep: Spatial Dependence: Weighting Schemes, Statistics. R package. https://r-spatial.github.io/spdep/ |
| **ggspatial** | Spatial visualization with ggplot2 | Dunnington D (2023). ggspatial: Spatial Data Framework for ggplot2. R package. https://paleolimbot.github.io/ggspatial/ |

#### Visualization

| Package | Purpose | Citation |
|---------|---------|----------|
| **ggplot2** | Grammar of graphics visualization (part of tidyverse) | Wickham H (2016). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. https://ggplot2.tidyverse.org |
| **gridExtra** | Arrange multiple grid-based plots | Auguie B (2017). gridExtra: Miscellaneous Functions for "Grid" Graphics. R package. https://CRAN.R-project.org/package=gridExtra |
| **scales** | Scale functions for visualization | Wickham H et al. (2023). scales: Scale Functions for Visualization. R package. https://scales.r-lib.org |
| **DT** | Interactive HTML data tables | Xie Y et al. (2024). DT: A Wrapper of the JavaScript Library 'DataTables'. R package. https://rstudio.github.io/DT/ |

#### Output Generation

| Package | Purpose | Citation |
|---------|---------|----------|
| **jsonlite** | JSON data interchange | Ooms J (2014). The jsonlite Package: A Practical and Consistent Mapping Between JSON Data and R Objects. arXiv:1403.2805. https://arxiv.org/abs/1403.2805 |
| **openxlsx** | Excel file export | Schauberger P & Walker A (2023). openxlsx: Read, Write and Edit xlsx Files. R package. https://ycphs.github.io/openxlsx/ |

### External Data Dependencies

| Data Source | Description | Citation |
|-------------|-------------|----------|
| **Australian Postcode Boundaries (2016)** | POA_2016_AUST.shp | Australian Bureau of Statistics (ABS). Australian Statistical Geography Standard (ASGS) Edition 2016. https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/1270.0.55.003July%202016 |
| **Local Government Area Boundaries (2020)** | LGA_2020_AUST.shp | Australian Bureau of Statistics (ABS). Australian Statistical Geography Standard (ASGS) Edition 2020. https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3 |
