# antarxxvii-leg2

This repository is used to manage data transformation script and output for antarxxvii-leg2 dataset

## Repo structure

```
├── LICENSE
├── README.md
├── antarxxvii-leg2.Rproj 	: R project file
├── data 					: raw data and output data
├── docs 					: documents provided alongside this dataset
├── renv 					: renv files for dependencies
├── renv.lock 				: describes the state of project's library
└── src 					: directory for source code
```

## Getting started

This project uses [renv](https://rstudio.github.io/renv/) to manage the virtual environment. If dependencies are not automatically installed by renv when you open `antarxxvii-leg2.Rproj`, please try the following command:

```{r}
renv::restore()
```
R version 4.4.2 (2024-10-31) was used when writing the script
