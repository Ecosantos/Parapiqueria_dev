# *Parapiqueria cavalcantei* Population Dynamics: Inverse methods Workflow

This repository provides the R code and data processing workflow for analyzing the population dynamics of *Parapiqueria cavalcantei*. The project's primary goal is to **facilitate the reproduction of quantitative results and analyses** from an associated scientific publication.

**Author:** Gabriel Santos

<!-- # **Citing This Work**                                                                                              
If you use this code or derived methodologies, please cite the corresponding scientific article.      
**[ACTION: Update this section with the full publication details once your article is peer-reviewed and published.]** -->

## Workflow Overview

The core analysis is driven by the `0 - Framework artigo Parapiqueria.Rmd` file, which orchestrates the following steps:

1.  **Data Preparation:** Loads and standardizes *Parapiqueria cavalcantei* census data (2022-2025) from various Excel sheets.
    -   **Required Data Files:**
        -   `Data/Dados parapiqueria - Completo - Consolidado 10Jun2025.xlsx`
        -   `Data/Fecundity.xlsx` (used by `Seed experiment integration.R`)
        -   `Data/Germination.xlsx` (used by `Seed experiment integration.R`)
    -   **Note:** These `.xlsx` files are expected in a `Data/` subfolder.
2.  **Vital Rate Estimation:** Uses the Quadratic Programming inverse method (`QPmat` from the `popbio` package) to estimate survival, stasis, and maturation probabilities. Recruitment rates are calculated separately.
    -   The `ModelToyv1.r` script contains code for **validating the accuracy** of the Quadratic Programming method using hypothetical population simulations.
3.  **Matrix Population Model (MPM) Construction:** Assembles 2x2 MPMs for each plot and year.
4.  **Demographic Analyses:**
    -   Calculates population growth rates (lambda).
    -   Performs Elasticity analysis to identify key demographic sensitivities.
    -   Conducts a Life Table Response Experiment (LTRE) to decompose differences in lambda between streams and years.
5.  **Population Viability Analysis (PVA):** Simulates extinction risk and persistence probabilities under various scenarios of vital rate perturbation.
6.  **Complementary Analyses:** Integrates data from seed experiments and genetic diversity.
    -   This step is performed by sourcing the `Seed experiment integration.R` script, which processes additional experimental data (fecundity, germination rates, seed mass) and includes genetic diversity metrics.

## Running the Project

To execute the full analysis workflow and generate the comprehensive HTML report:

1.  Ensure all required R packages (`popbio`, `ggplot2`, `tidyverse`, `openxlsx`, `ggrepel`, `ggridges`, `broom`, `car`, `cowplot`, `DiagrammeR`) are installed.
2.  Place the necessary data Excel files in the `Data/` subfolder.
3.  Open `0 - Framework artigo Parapiqueria.Rmd` in your R environment (e.g., RStudio) and execute the R Markdown document.

## Custom Functions

Key custom functions facilitating the analysis include:

-   `statrep()`: For standardized statistical summaries of data.
-   `proptable_modif()`: Calculates population persistence/extinction proportions based on a user-defined threshold.
-   `Myviab_func()`: An adaptation for stochastic population projections, allowing simulation of extinction risk under specific vital rate reduction scenarios.

## License

This project is licensed under the [MIT License](LICENSE).

This code is released solely for the purpose of peer review and academic transparency. It is NOT licensed for reuse, reproduction, or publication prior to the official publication of the associated scientific article. All rights are reserved by the author.

## Disclaimer

Parts of this README were generated using artificial intelligence (Gemini 2.5 Flash) to improve clarity and structure. The content was subsequently reviewed and edited by the author.
