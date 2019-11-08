# SS_data

October 23, 2019 - Danielle Dempsey (danielle.dempsey@dfo-mpo.gc.ca)

Some notes on `SSdata`:

1. Script Extract_dataframes.R calls function compileDataframes(), which extracts fishery independent and commercial landings data and exports dataframes in a format suitable for the marindicators package. These dataframes are stored in path/output.

2. Some of the the functions called by compileDataframes() are exported so they can be called directly by the user. See help files.

3. compileDataframes() takes about 60 minutes to extract and export data from 1970 - 2015. Could be made faster by not calculating the q-adjusted abundance and biomass values before stratification (function qBiomass()), but this would require significant modifications to the rest of the code. qBiomass() is called by biomassData() and sets up the trawl survey dataframe for processing by the other functions.

3. A warning is generated for each year from stratifyBiomass():
In stratifyBiomass(path = path, s.year = s.year, e.year = e.year,  ... :
  NAs introduced by coercion
I'm not sure where this comes from, but it is also generated when running AC's code

4. R CMD Check returns 1 note:
This is because data are exported as .RData files (e.g., from biomassData()), and then imported by subsequent functions (e.g., stratifyBiomass()). The data is loaded into the local environment, but R doesn't know this when it runs the check.

5. The package can't extract RV data past 2015 because herring has not been updated.