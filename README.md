# bsyncr

A package for generating BuildingSync documents in R. It is intended to be used in parallel with the [nmecr](https://github.com/kW-Labs/nmecr) package, enabling serialization of NMEC analysis into BuildingSync documents.

## Using as a package

Go to [NOAA's website](https://www.ncdc.noaa.gov/cdo-web/token) to request an API key. This is needed to look up weather data. Set an environment variable named `NOAA_TOKEN` with your API key.

```bash
export NOAA_TOKEN=YOUR_KEY_HERE
```

Within R, run the following to test if the package loads. There are many dependent packages that might need to be installed, check the logs as needed.

```r
remotes::install_github('BuildingSync/bsyncr', upgrade='never')

# using a specific release
remotes::install_github('BuildingSync/bsyncr@v0.1.0', upgrade='never')

# verify that the package loads
library(bsyncr)

# see the vignette PDF file or the test file for an
# example on using.
```

## Releasing new version

- Update version in bsync.RProj and DESCRIPTION to the next correct semantic version
- Create CHANGELOG in GitHub
- Paste in updates and merge release prep PR to develop
- Test
- Merge to main
- Tag on GitHub

## TODO

- Stub out `auc:DerivedModels` for both the baseline and reporting scenarios
- Run NMEC analysis, as shown in nmecr vignette
  - Serialize the SLR model
  - Serialize the 5p change point model
- Add timeseries serialization into utility functions
