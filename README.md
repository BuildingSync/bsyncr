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

- Create a branch with the prepared release change log.
- Update version in bsync.RProj and DESCRIPTION to the next correct semantic version
- Make sure the DESCRIPTION has the correct version of the dependencies
- For testing purposes, make sure the versions of NMECR and RNOAA are correct in the `setup_environment.R` script
- Create CHANGELOG in GitHub, paste in updates and
- Merge release prep PR to develop
- Test
- To release, from the command line merge latest develop into latest main: :code:`git merge --ff-only origin develop`. This will point the HEAD of main to latest develop. Then push the main branch to GitHub with :code:`git push`, which may require a developer with elevated privileges to push to main.
- Back on GitHub create a new tag in GitHub against main and copy the change log notes into the tag description.
- Tag on GitHub, copy over the correct version (format v0.1.0) and CHANGELOG content.

## TODO

- Stub out `auc:DerivedModels` for both the baseline and reporting scenarios
- Run NMEC analysis, as shown in nmecr vignette
  - Serialize the SLR model
  - Serialize the 5p change point model
- Add timeseries serialization into utility functions
