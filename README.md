# Freshwater_Horizon_Scan_Maritimes
 Horizon scan for New Brunswick, Nova Scotia, and Prince Edward Island (i.e. the Maritimes), Canada freshwater habitat. 

 #Process
 The R scripts are separated based on which step within the watchlist generation process an assessor may be. Steps are number in the scripts. 
 After completing our own watchlist process, we would recommend pay special attention to steps 1 and 2 as moving too quickly through these steps may result in needing to return to this step and add species back to the working list. Also, paying extra ttention to accounting for when and why species are added to or removed from the working list is recommended as it facilitates easier report or manuscript write ups post-analysis. Lastly, we used the ISEIA tool for our rapid-level risk assessments, but assessors are encouraged to do their own research in tool selection because there are multiple tools that could be a better fit for your purpose. 

 1. Generate and review the working list of species with Working_list.R.
 2. Compare species working list with other watchlists through the watchlist_comparsion. R script. Add any additional species of interest to the working list. 
 3. Search for species occurrence records (required for next step) with gbif_script.R.
 4. Standardize CHELSA climate data layers with standardized_rasters.R
 5. Complete climate match analysis with Climatch.R.
 6. External to R, complete the rapid-level risk assessments for each species.
 7. Explore the reuslts further and generate plots with the ISEIA_script.R and the hot_spot_area.R scripts.



**#Limitations**
Due to file sizes, not all files were able to be uploaded to this repository, including species occurrence records and CHELSA data layers. 
To access and download CHELSA Climate data layers, please visit their website here: https://chelsa-climate.org/

Also, despite not being able to upload the csv files of combined species data, each species individual data is available in the data/species_data folder. Likewise, the results for each species under baseline and future climate scenarios is available within data/output_data.
