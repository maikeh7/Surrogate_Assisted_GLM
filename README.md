## Code for "Synthesizing data products, mathematical models, and observational measurements for lake temperature forecasting"

This repository contains all code for reproducing all datasets, analyses, and figures used in the manuscript "Synthesizing data products, mathematical models, and observational measurements for lake temperature forecasting" (Holthuijzen et al. , in preparation)
Maintainer: Maike Holthuijzen (mholthuijzen@alumni.uidaho.edu)

## IMPORTANT
First, clone this repository. Next, download the zipped folder containing all associated data here: https://zenodo.org/uploads/10028017 **into the repository directory**. Unzip the Data folder and ensure that it is called "Data". 

Once you have cloned the respository and have saved the zipped data folder **in the same folder as the respository**, run these lines:
```
zipF = "Data.zip"
outDir = "Data"
unzip(zipF, exdir = outDir)
```

## Folder Organization
* **GLM**: contains code for downloaded NOAA-GEFS forecasts and generating GLM ensemble forecasts
* **Figures**: contains code for generating figures
* **Results**: contains code for analyzing results and constructing competing models
* **Validation**: contains code for running model validation

## References
```
@Preamble{ " \newcommand{\noop}[1]{} " }
@article{holthuijzen2023Synthesizing,
         title = {Synthesizing data products, mathematical models, and observational measurements for lake temperature forecasting}, 
         author = {Maike Holthuijzen Sauer and Robert B. Gramacy and R. Quinn Thomas and Cayelan C. Carey and David M. Higdon},
         journal = {Annals of Applied Statistics},
         year = "\noop{2024}in preparation"
}
```