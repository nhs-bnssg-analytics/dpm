# Dynamic Population Model (DPM)

This is the deterministic run of the DPM. As at present, the most commonly used function is `dpm::run_dpm_age_based`

## How to run

1. Set up your `.Renviron` file with the following variables:
```
Server = "" 
Githubpat = ""
```
Where the Server is the name of the SQL server, and Githubpat is a [PAT Token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens) to your GitHub Account. The first is so you can access SQL database, the second is so that you can download this package, as it's in a private repo.

2. Install required non-CRAN packages by running the following code:
```
devtools::install_github("davidsjoberg/ggsankey")
```

3. Install the package. The best way of doing this is running
```
devtools::install_github("nhs-bnssg-analytics/dpm",auth_token = Sys.getenv("Githubpat"))
```

4. Run the DPM! There are some (old) example workflows in the folder `/inst/workflow-examples`. For a more up-to-date codebase built for first-time users, see the [DPMflex](https://github.com/nhs-bnssg-analytics/DPMflex) package.
