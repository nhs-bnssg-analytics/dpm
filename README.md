# Dynamic Population Model (DPM)

This package implements the DPM in a parameterised way, for use by decision makers looking to create scenarios and assess the projections.

## How to run

:warning: still under construction :warning:

1. Set up your `.Renviron` file with the following variables:
```
Server = "" 
Githubpat = ""
```
Where the Server is the name of the SQL server, and Githubpat is a [PAT Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) to your GitHub Account. The first is so you can access SQL database, the second is so that you can download this package, as it's in a private repo.

2. Install the package. The best way of doing this is running
```
devtools::install_github("nhs-bnssg-analytics/dpm",auth_token = Sys.getenv("Githubpat"))
```

3. Run the DPM! There are some example workflows in the folder `/inst/workflow-examples`. For a workflow with simple initial conditions see `workflow-basic.R`. For an example workflow using the SQL connections see `workflow-sql.R`
