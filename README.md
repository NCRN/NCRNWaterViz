# NCRNWaterViz (branch `ncrn_refactor`)

2026-11-13 (v. 2.1.0)

## Description

The `ncrn_refactor` branch is the NCRN-specific branch of the `NCRNWaterViz` repo.

## Getting started

1.  Create a new R Studio project File -\> New Project -\> Version Control -\> Git -\> <https://github.com/NCRN/NCRNWaterViz>
2.  Switch to our `ncrn_refactor` branch. In your terminal:

```{terminal}
git checkout ncrn_refactor
```

3.  Bring your project's `renv` in sync with the provided renv.lock file. In your R console:

```{r}
install.packages('renv')
renv::activate() # tells your R project to use renv to manage packages locally instead of gloablly via install.packages()
renv::restore() # tells your R project to install all of the packages from the renv.lock file
```

4.  Create a Data/NCRN folder. In terminal:

```{terminal}
mkdir Data
cd Data
mkdir NCRN
cd ../..
```

5.  Copy the data and metadata files to your `Data/NCRN` folder from [here](https://irma.nps.gov/DataStore/Reference/Profile/2321619) wqp.csv wqp_ncrnwater_metadata.csv

6.  Confirm that you can run the shiny app.

-   open `global.R`
-   click the "Run App" button at the top of your code editor

7.  If the app runs for you, continue on. Otherwise, contact Charlie.
8.  Make your feature branch(es) from the `ncrn_refactor` branch. In terminal:

```{terminal}
git checkout -b <the_name_of_your_branch>
```

9.  Make changes app's source (probably ui.R and server.R) in your branch. Push/pull from your branch. Submit a pull request (or contact Charlie) when you're done making changes.

## News

Uses NCRNWater 1.1.1 (Nov 2026)

## Highlights

- New: 2025 NCRN photos (upstream and downstream)
- Improved: Improved exceedances figure hovertext
- New: Added citations to exceedances data table
- Bugfix: infinite loop when controls (e.g., sliders, picklists) are adjusted during calculations

## New Features

### Shiny app packaged with NCRNWater

This NCRN Water Quality Explorer is now distributed with the NCRNWater R package and can be launched directly from R.

```r
NCRNWater::run_shiny_app()
NCRNWater::run_shiny_example()
```

### Configuration-driven application

Added support for YAML-based application configuration.

New capabilities include:

- Profile-based configuration
- User-supplied YAML files
- Runtime configuration overrides
- Automatic data hydration
- Automatic year-range detection
- Optional profile-specific content

### Profile abstraction

The Shiny app has been generalized to support any WQP-formatted dataset with compatible metadata.

Profiles can now provide:

- Network name and identifiers
- Dataset download links
- About pages
- Splash screens
- Basemap definitions

### Optional splash screens

Splash screens are now optional. Applications without splash images launch normally.

### Optional About pages

Profiles can provide custom About pages through configuration. The app automatically hides the About tab when no About page is supplied.

### YAML-configurable basemaps

Map basemaps can now be defined through configuration rather than being hard-coded into the application.

Supported sources include:

- Leaflet providers
- XYZ raster tile services
- Tokenized map services

### Improved example workflows

Added helper functions for onboarding and testing:

```r
example_data()
example_paths()
example_ncrnwater()
run_shiny_example()
```

## Exceedance Framework Improvements

### Metadata-driven threshold operators

Thresholds now support configurable comparison operators:

- <
- <=
- >
- >=

using metadata fields:

- LowerPointCondition
- UpperPointCondition

### Enhanced exceedance reporting

Added:

```r
exceed(..., mode = "rows")
```

to return individual exceedance records.

### New exceedance service

Added:

```r
build_exceed_site_package()
```

which centralizes:

- Threshold evaluation
- Exceedance summaries
- Histogram generation
- Accessibility text
- Data-table generation

### Improved exceedance language

Exceedance summaries now use metadata-aware language such as:

> greater than 0.31 mg/L

instead of assuming all thresholds represent upper-limit exceedances.

### Exceedance table enhancements

Added:

- Threshold citations (`AssessmentDetails`)
- Improved tooltips
- Plotly histogram hover text
- Comparator-aware descriptions

## Photo Viewer Improvements

### Improved reactive handling

Resolved invalid-state conditions in the photo viewer when users modify filters after selecting photos.

Improvements include:

- Safer image-path validation
- Better handling of stale site visits
- Elimination of transient rendering errors
- More robust photo-selection workflows

## Time-Series Plot Improvements

### Enhanced Plotly interaction

- Added point-based hover behavior
- Improved handling of multiple measurements on the same date
- Added visual jitter to overlapping observations
- Switched from x-axis hover aggregation to nearest-point hover behavior

## Data Accessor Fixes

Resolved duplication and alignment issues affecting:

- getParkInfo()
- getSiteInfo()
- getCharInfo()
- getChars()
- exceed()

Improvements include:

- Stable deduplication behavior
- Correct threshold alignment
- Improved type stability
- More robust S4 object traversal

## Internal Refactoring

### Service Architecture

Added dedicated services for:

- Configuration management
- Data hydration
- Splash screens
- Exceedance processing
- Time utilities
- Photo handling

### Packaged App Structure

Refactored the application to run from:

```text
inst/shiny/app
```

using package-managed bootstrap state and YAML-based configuration.

## Deployment Notes

### New Package Dependencies

The Shiny application now requires:

- shiny
- leaflet
- DT
- plotly
- openair
- NADA

These dependencies are installed automatically through the package DESCRIPTION file.

### Migration Notes

Deployments should review:

- profile configuration files
- map basemap configuration
- dataset URL configuration
- optional splash-screen assets
- optional About-page assets

to take advantage of the new configuration framework.

