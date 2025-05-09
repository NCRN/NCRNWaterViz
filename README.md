# NCRNWaterViz (branch `ncrn_refactor`)

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

5.  Copy the data and metadata files to your `Data/NCRN` folder from [here](https://doimspp.sharepoint.com/:f:/r/sites/NCRNWater/Shared%20Documents/General/Annual-Data-Packages/2024?csf=1&web=1&e=fN3XjJ) wqp.csv wqp_ncrnwater_metadata.csv

6.  Confirm that you can run the shiny app.

-   open `global.R`
-   click the "Run App" button at the top of your code editor

7.  If the app runs for you, continue on. Otherwise, contact Charlie.
8.  Make your feature branch(es) from the `ncrn_refactor` branch. In terminal:

```{terminal}
git checkout -b <the_name_of_your_branch>
```

9.  Make changes app's source (probably ui.R and server.R) in your branch. Push/pull from your branch. Submit a pull request (or contact Charlie) when you're done making changes.
