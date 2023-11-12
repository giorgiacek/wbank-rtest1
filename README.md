## Repo for wbank-rtest1

Repository for the World Bank R Test 1 project. The rendered quarto project is hosted on a GitHub page at: <https://giorgiacek.github.io/wbank-rtest1/>

I use the `wbank-rtest1.R` script as a sort of testing ground for the functions, which are then moved to the `.qmd` file.

**Note**: This Quarto project utilizes `renv` for environment reproducibility. To get started, clone the repository, and ensure `renv` is installed in R (`install.packages("renv")`). Open the project in RStudio or whatever you use, which should recognize the `renv` environment, and run `renv::restore()` to install the necessary packages from the renv.lock file. Remember to use `renv::snapshot()` to update the environment if you install or update any packages. This setup isolates the project's R packages from system-wide packages.
