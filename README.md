[![DOI](https://zenodo.org/badge/117082747.svg)](https://zenodo.org/badge/latestdoi/117082747) [![DOI](https://zenodo.org/badge/72736017.svg)](https://zenodo.org/badge/latestdoi/72736017)
# Replication files to Ankargren, Unosson and Yang (2018)
This repository provides the necessary files to reproduce Ankargren, Unosson and Yang (2018).

## General instructions
There are four folders with files for different purposes:
 - `estimation`: files used for estimation
 - `plots`: files for producing plots
 - `processing`: files (and data) for processing results

### Estimation
The included files are (nesting shows calling order):
 - `<freq>_<prior>_parallel.R`: the files you run, which runs models in parallel
    * `setup.R`: includes all settings (which are common for all the `*_parallel.R` functions) and auxiliary functions
       + `prepare_data.R`: prepare the data (available in `mf_list.RData`)
       + `quarterly_models.R`: functions for estimating the quarterly models

The output is a saved file named by the convention `<freq>_<prior>_<time>.rds`, which includes the forecasts and some meta information for the model given by `<freq>` and `<prior>` made at origin `<time>`. Note that you will have to change where the file saved in the `saveRDS()` calls in `setup.R`. Also note that each file is around 80-100 mb, so it is necessary to have around 30-50 gb of free disk space if all models are to be run. 

### Plots
These files produce the plots in the paper. (To be precise: the files produce the tikz codes for the plots, which can be converted to pdf using the externalize command in tikz.) The `fcst_*.rds` files contain summaries of all the data produced from `estimation files`, i.e. instead of entire predictive distributions for each forecast origin and model they contain what is needed to compute the evaluation measures in the paper. These are needed to create the plots.

### Processing
These two files convert the output from `estimation` to the summarized `fcst_*.rds` files used by the plots.

## Package version
The version of the `mfbvar` package that was used for producing the results can be retrieved from here: [http://dx.doi.org/10.5281/zenodo.1145656](http://dx.doi.org/10.5281/zenodo.1145656).


