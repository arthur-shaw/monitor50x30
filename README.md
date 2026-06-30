# monitor50x30

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

`{monitor50x30}` is an R package that provides user-friendly, graphical interface for monitoring all the surveys in the 50x30 survey program, from CORE AG to ILP and MEA.

In particular, `{monitor50x30}` allows users to automate

- **Get data.** Download all survey data, combine data from all questionnaire versions, and construct a consolidated set of data files.
- **Check data completeness.** Generate a report on the progress of data collection and on whether all expected data have been received for each primary sampling unit (PSU).
- **Monitor data quality.** Create a report that enables survey managers to spot trends in data that may undermine key survey estimates, where the elements of the report are tailored to the survey and selected by the end user.

## Installation 🏗️

### System dependencies 💻

Because `{monitor50x30}` is an R package, users need to have the software for running R:

- R (>= 4.1)
- RTools (for Windows)
- Quarto (>= 1.6.39)
- RStudio / Positron

<details>
<summary>
Open to see more details 👁️
</summary>

Before running this program for the first time, (re)install the
following software with the minimum required versions:

- [R](#r)
- [RTools](#rtools)
- [Quarto](#quarto)
- [RStudio/Positron](#rstudiopositron)

#### R

- Follow this [link](https://cran.r-project.org/)
- Click on the appropriate link for your operating system
- Click on `base`
- Download and install

#### RTools

Necessary when run on a Windows operaterating system

- Follow this [link](https://cran.r-project.org/)
- Click on `Windows`
- Click on `RTools`
- Find the version matching the version of installed R
- Download
- Install in the default installation location (e.g., `C:\rtools45` on
  Windows)

This program allows R to compile C++ scripts used by certain packages
(e.g., `{dplyr}`).

#### RStudio/Positron

RStudio or Positron is required for a few reasons:

1. Provides a good interface for using R
2. Ships with [Quarto](https://quarto.org/), a program that this project will use for creating reports

For RStudio

- Navigate [here](https://posit.co/downloads)
- Download the RStudio Desktop installer for your operating system
- Install

For Positron:

- Navigate [here](https://positron.posit.co/)
- Download the installer for your operating system
- Install

</details>

### `{monitor50x30}` package 📦

Since monitor50x30 is not yet available on CRAN, it can be installed from GitHub by running the following code in the R console:

```r
if (!require("pak")) install.packages("pak")
pak::pak("arthur-shaw/monitor50x30")
```

## Usage 👩‍💻

To open the app's graphical interface, simply run this command in the R console:

```r
library(monitor50x30)
run_app()
```
