
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tbportals.ct.survival.aspera

<!-- badges: start -->

<!-- badges: end -->

## Background information

**Author:** Gabriel Rosenfeld

**Email:** gabriel.rosenfeld@nih.gov

**Date:** 12/1/2020

**Version:** 1.0

**License:** CC0

This repository contains the associated code for the publication
examining the CT radiologist annotation data from **[TB
Portals](https://tbportals.niaid.nih.gov/)**. The analysis leverages the
publicly shared TB Portals data which can be obtained by an external
researcher by signing the
**[DUA](https://tbportals.niaid.nih.gov/download-data)** to examine CT
radiologist annotation predictive capability. Bear in mind that while
this code allows for easing the barrier to rerunning of the analysis,
differences in local computing environment may result in differences
between reruns.

### Software requirements

**[R](https://www.r-project.org/)**

**[Rstudio](https://rstudio.com/)**

### Data requirements

Due to the program requirements to sign the DUA before access to data,
interested parties will have to complete the
**[DUA](https://tbportals.niaid.nih.gov/download-data)** first in order
to download the correct version of the data using Aspera (October 2020).
Once data download has been completed, the individual .csv files
containing the clinical data must be put into the following local
project directory **data/TB Portals Published data\_20201021** in order
for the associated drake workflow to execute correctly. Otherwise, it
will not complete.

### Code to execute the workflow

To execute the workflow, pull the project to your local repo and open
the project in Rstudio after adding the data to the correct folder. Then
execute the code below:

First install required packages and versions using the renv package. The
command below uses the renv.lock file to install the required R
packages.

``` r
install.packages(renv)
renv::restore()
```

Then, execute the drake workflow to run the pipeline.

``` r
drake::r_make()
```

After the drake workflow is executed, you can view the current pipeline
with the following:

``` r
drake::r_vis_drake_graph()
```

Any of the individual steps of the analysis can be loaded into your R
environment to
explore.

``` r
loadd(initial_df) # This is the initial df created based upon the selected cohort of cases.
```

Key files are found in the following directory

  - R/plan.R - the drake workflow plan
  - R/file\_name.R - functions executing portions of the workflow plan
    such as preprocessing, setting up the MLR3 tasks, benchmarking, etc.

Using the R/plan.R file, you can identify different drake targets to
loadd and explore after executing the drake workflow. Each of the
R/file\_name.R files generally receive upstream drake targets as part of
function calls to continue the workflow execution.
