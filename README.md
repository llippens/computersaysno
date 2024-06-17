# Computer says 'no'
## Overview
This repository accompanies a [study](https://doi.org/10.1016/j.chbah.2024.100054) examining the potential ethnic and gender biases of ChatGPT in evaluating job applications. Through a correspondence audit approach involving 34,560 vacancy–CV combinations, the study evaluates ChatGPT's responses (i.e. ratings) to fictitious profiles across various ethnic and gender identities.

The repository provides the data, code, and figures used in the analyses, facilitating replication, further research, and discussion on mitigating bias in ChatGPT and other (generative) AI-driven applications.

## Project structure
The project is organised as follows.

- `code`: This folder contains all the source code used in the analyses, including R scripts and Quarto (.qmd) files for descriptive statistics, main regression analyses, heterogeneity analyses, sensitivity analyses. The folder also contains a `plots` subfolder with code to replicate the manuscript and appendix figures.
- `data`: This folder houses the input data fed to ChatGPT as well as the output data in .RData format.
- `figures`: This folder includes figures generated by the code in `code\plots`, stored in .png format.
- `functions`: This folder contains custom R functions used across different analyses for data processing, statistical modelling, and visualisation.

## Getting Started
To replicate the analyses, the following software is required.

### Recommended software
- **R version**: 4.3 (or higher).
- **Quarto version**: 1.3 (or higher).
- **Integrated Development Environment (IDE)**: While any IDE that supports R and Quarto can be used, I would recommend using RStudio for an optimal experience.

### Software installation
1. **Install R**: Download and install R from the [R Project website](https://www.r-project.org/).
1. **Install Quarto**: Download and install Quarto from the [Quarto website](https://quarto.org/docs/get-started/).
1. **Set up IDE**: Download and set up your preferred IDE. This can be achieved by following the instructions on the Quarto website. Ensure the IDE supports R and Quarto.
1. **Install packages**: Open your R console and install the necessary packages using the command below.
1. **Load project**: Clone or download the `Computer says 'no'` project repository from GitHub to your local machine.

```
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               tidyverse,
               janitor, broom, glue,
               knitr, kableExtra,
               brglm2, sandwich,
               effectsize, marginaleffects, performance,
               report, modelsummary,
               ggeffects, hrbrthemes, viridis, ggpubr)
```

## Usage
To use the code with the required software, follow these steps.

1. **Open R project**: Open the `computersaysno.Rproj` file with your IDE to set the working directory to the project folder automatically.
1. **Run scripts**: Execute the scripts (`.qmd` files) within the `code` folder to reproduce the analyses and figures. Note that you do *not* need to execute the code in a specific order. The headings and comments within the scripts should guide you in the analyses.

By following the above steps, you should be able to replicate the environment and analyses of this project.

## Citation
If you use or refer to the contents of the original [manuscript](https://doi.org/10.1016/j.chbah.2024.100054) in your own work, please use the following citation.

```
Lippens, L. (2024). Computer says ‘no’: Exploring systemic bias in ChatGPT using an audit approach. Computers in Human Behavior: Artificial Humans, 2(1), 100054. https://doi.org/10.1016/j.chbah.2024.100054
```

The data, code, and figures are part of an [OSF project](osf.io/vezt7). If you use any of these elements in your own work, please use the following citation. (Also consider citing the original [manuscript](https://doi.org/10.1016/j.chbah.2024.100054).)

```
Lippens, L. (2024). Computer says ‘no’. The Open Science Framework. osf.io/vezt7
```

## License
This project is made available under the Creative Commons Attribution 4.0 International License (CC BY 4.0). This license allows you to share, copy, distribute, and transmit the work, as well as to adapt the work and make commercial use of it, provided that the original author and source are credited. For any use or distribution, you must make clear to others the license terms of this work.

For more details on the CC BY 4.0 license, please visit [Creative Commons License CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).
