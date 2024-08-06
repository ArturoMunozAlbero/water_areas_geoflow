remotes::install_github("r-geoflow/geoflow")

require(sf)
require(geoflow)
#setwd("C:/Users/artur/OneDrive/Documents/FAO/R GIS/GRSF_water_areas_project")
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_geoflow")
wd = getwd()
#initiate workflow (if needed)/for analysing the data
#config = initWorkflow("config_grsf_water_areas.json")
#config$metadata$content$entities[[1]]$rights[[1]]$values[[1]]
#config$metadata$content$entities[[1]]$subjects[[1]]$keywords[[1]]

#execute workflow
jobdir = executeWorkflow("config_grsf_water_areas.json")

#area files to copy from water_areas_geoflow to water_areas_vocabulary
area_files = list.files(jobdir, pattern = "_areas.csv", recursive = TRUE, full.names = T)
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_vocabulary")
for(area_file in area_files){
	file.copy(from = area_file, to = getwd(), overwrite = TRUE)
}

#all areas?
all_areas = do.call("rbind", lapply(area_files, function(x){readr::read_csv(x)}))
readr::write_csv(all_areas, "all_areas.csv")
all_areas_publishable = all_areas[all_areas$publishable == 'yes',]
readr::write_csv(all_areas_publishable, "all_areas_publishable.csv")
#unique(all_areas$namespace)
#all_areas[is.na(all_areas$namespace),]
setwd(wd)
#Summary of publishable areas
legal_flags <- unique(all_areas[,c("namespace","system_owner_code","publishable","grsf_sourceoftruth","grsf_creation","grsf_licence","source_link","useLimitation")])
legal_flags_summary <- c(PUBLISHABLE = nrow(subset(legal_flags, publishable == "yes")), 
                         NON.PUBLISHABLE = nrow(subset(legal_flags, publishable == "no")) + nrow(subset(legal_flags, is.na(publishable))))
readr::write_csv(legal_flags,"legal_flags.csv")


#geopackages
gpkgs = list.files(jobdir, pattern = "_areas.gpkg", recursive = TRUE, full.names = T)
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow")
for(gpkg in gpkgs){
  file.copy(from = gpkg, to = getwd(), overwrite = TRUE)
}
setwd(wd)

#one single geopackage
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))
features_to_publish = all_features[all_features$publishable == 'yes',]
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles")
sf::st_write(all_features, "all_areas.gpkg")
zip(zipfile = 'all_areas.zip', files = 'all_areas.gpkg')
sf::st_write(features_to_publish, "all_areas_publishable.gpkg")
zip(zipfile = 'all_areas_publishable.zip', files = 'all_areas_publishable.gpkg')
setwd(wd)


#execute workflow for geospatialization and publication
jobdir = executeWorkflow("config_nfis_grsf_polygons.json")

#Debuf workflow
#DEBUG = debugWorkflow("config_nfis_grsf_polygons.json")
#closeWorkflow(DEBUG$config)

#EXECUTE WORKFLOW FOR PLACEMARKS
#workflow for publishing the centroids of all GRSF records