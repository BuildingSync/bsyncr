# bsyncr
A package for generating BuildingSync documents in R.  It is intended to be used in parallel with the [nmecr](https://github.com/kW-Labs/nmecr) package, enabling serialization of NMEC analysis into 
BuildingSync documents.

# TODO
- Stub out `auc:DerivedModels` for both the baseline and reporting scenarios
- Run NMEC analysis, as shown in nmecr vignette
  - Serialize the SLR model
  - Serialize the 5p CPM
- Add timeseries serialization into utility functions
