require(sf)
require(readr)


if(!dir.exists("temp")) dir.create(file.path(getwd(),"temp"))
water_area_refs <- list.files("../water_areas_vocabulary", full.names = T, pattern = ".csv")

#testing
#water_area_ref <- water_area_refs[5] 


#harmonize / merging process
area_db <- do.call("rbind", lapply(water_area_refs, function(water_area_ref){
	cat(sprintf("Reading water area ref '%s'\n", water_area_ref))
	war_areas <- as.data.frame(readr::read_csv(water_area_ref))
	area_refs <- unique(war_areas[,c("namespace", "georef", "georef_code", "shape_file")])
	
	area_refs.sf <- do.call("rbind", lapply(1:nrow(area_refs), function(i){
		print(i)
		area_ref <- area_refs[i,]
		zip_shapefile <- sprintf("../water_areas_shapefiles/%s", area_ref$shape_file)
		files = utils::unzip(zip_shapefile, exdir = file.path(getwd(),"temp"), unzip = getOption("unzip"))
		area_ref.sf <- sf::st_read(sprintf("../water_areas_geoflow/temp/%s", gsub("\\.zip", ".shp", area_ref$shape_file)))
		area_ref.sf <- sf::st_transform(area_ref.sf, 4326)
		data_to_merge <- war_areas[war_areas$namespace == area_ref$namespace,]
		cols_to_keep <- colnames(area_ref.sf)[sapply(colnames(area_ref.sf), function(x){!x %in% colnames(data_to_merge)})]
		if(!area_ref$georef_code %in% cols_to_keep) cols_to_keep <- c(area_ref$georef_code, cols_to_keep)
		area_ref.sf <- area_ref.sf[,cols_to_keep]
		area_ref.sf2 <- merge(area_ref.sf, data_to_merge, by.x = area_ref$georef_code, by.y = "area_code")
		area_ref.sf2$area_code <- area_ref.sf2[[area_ref$georef_code]] 
		area_ref.sf2 <- area_ref.sf2[,colnames(war_areas)]
		return(area_ref.sf2)
	}))
	return(area_refs.sf)
}))