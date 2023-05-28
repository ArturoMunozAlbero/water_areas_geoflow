require(geoflow)
jobdir = executeWorkflow("config_grsf_water_areas.json")
area_files = list.files(jobdir, pattern = "_areas.csv", recursive = TRUE, full.names = T)

wd = getwd()
setwd("../water_areas_vocabulary/geoflow")
for(area_file in area_files){
	file.copy(from = area_file, to = getwd())
}
setwd(wd)