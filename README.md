# SS_data

March 16, 2020 - Danielle Dempsey (danielle.dempsey@dfo-mpo.gc.ca)

Some notes on `SSdata`:

1. Script Extract_dataframes.R calls function compileDataframes(), which extracts fishery independent and commercial landings data and exports dataframes in a format suitable for the marindicators package. These dataframes are stored in path/output.

2. Some of the the functions called by compileDataframes() are exported so they can be called directly by the user. See help files.

3. compileDataframes() takes about 60 minutes to extract and export data from 1970 - 2018. 

4. R CMD Check returns 1 note:
This is because data are exported as .RData files (e.g., from biomassData()), and then imported by subsequent functions (e.g., stratifyBiomass()). The data is loaded into the local environment, but R doesn't know this when it runs the check.

