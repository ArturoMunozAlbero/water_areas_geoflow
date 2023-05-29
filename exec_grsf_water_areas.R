require(geoflow)

#execute workflow
jobdir = executeWorkflow("config_grsf_water_areas.json")

#area files to copy from water_areas_geoflow to water_areas_vocabulary
area_files = list.files(jobdir, pattern = "_areas.csv", recursive = TRUE, full.names = T)
wd = getwd()
setwd("../water_areas_vocabulary/geoflow")
for(area_file in area_files){
	file.copy(from = area_file, to = getwd(), overwrite = TRUE)
}
setwd(wd)

#all areas?
all_areas = do.call("rbind", lapply(area_files, function(x){readr::read_csv(x)}))
readr::write_csv(all_areas, "all_areas.csv")

#geopackages
gpkgs = list.files(jobdir, pattern = "_harmonized.gpkg", recursive = TRUE, full.names = T)
wd = getwd()
setwd("../water_areas_shapefiles/geoflow")
for(gpkg in gpkgs){
  file.copy(from = gpkg, to = getwd(), overwrite = TRUE)
}
setwd(wd)

#one single geopackage
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))
