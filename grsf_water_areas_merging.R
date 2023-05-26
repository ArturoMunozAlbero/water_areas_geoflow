setwd("D:/Documents/CLIENTS/FAO/Projets/GRSF/water_areas_geoflow")

require(sf)
require(readr)
require(ows4R)

WFS = WFSClient$new(url = "https://www.fao.org/fishery/geoserver/fifao/wfs", serviceVersion = "1.0.0")
continent <- WFS$getFeatures("fifao:UN_CONTINENT2")

if(!dir.exists("temp")) dir.create(file.path(getwd(),"temp"))
water_area_refs <- list.files("../water_areas_vocabulary", full.names = T, pattern = ".csv")

#testing
#water_area_ref <- water_area_refs[5] 

errors <- data.frame(
	vocabulary = character(0),
	error = character(0),
	stringsAsFactors = FALSE
)

#harmonize / merging process
area_db.list <- lapply(water_area_refs, function(water_area_ref){
	cat(sprintf("Reading water area ref '%s'\n", water_area_ref))
	war_areas <- as.data.frame(readr::read_csv(water_area_ref))
	area_refs <- try(unique(war_areas[,c("namespace", "georef", "georef_code", "shape_file")]))
	
	if(is(area_refs, "try-error")){
		errors <<- rbind(errors, data.frame(vocabulary = water_area_ref, error = "Non-standard structure for vocabulary file"))
		return(NULL)
	}
	
	area_refs.sf <- do.call("rbind", lapply(1:nrow(area_refs), function(i){
		area_ref <- area_refs[i,]
		zip_shapefile <- sprintf("../water_areas_shapefiles/%s", area_ref$shape_file)
		unzipped <- try(utils::unzip(zip_shapefile, exdir = file.path(getwd(),"temp"), unzip = getOption("unzip")))
		print(unzipped)
		if(is.null(unzipped)){
			errors <<- rbind(errors, data.frame(vocabulary = water_area_ref, error = sprintf("No shapefile '%s' in 'water_areas_shapefiles' repository", area_ref$shape_file)))
			return(NULL)
		}
		shp_file <- unzipped[endsWith(unzipped,".shp")]
		area_ref.sf <- sf::st_read(shp_file)
		area_ref.sf <- sf::st_transform(area_ref.sf, 4326)
		
		layername = unlist(strsplit(basename(shp_file),".shp"))[1]
		if(layername %in% c("ICCAT_SMU", "PAC_TUNA_REP", "EEZ")){
			sf::st_crs(continent) <- sf::st_crs(area_ref.sf)
			area_ref.sf <- sf::st_difference(area_ref.sf,continent)
		}
		data_to_merge <- war_areas[war_areas$namespace == area_ref$namespace,]
		cols_to_keep <- colnames(area_ref.sf)[sapply(colnames(area_ref.sf), function(x){!x %in% colnames(data_to_merge)})]
		if(!area_ref$georef_code %in% cols_to_keep) cols_to_keep <- c(area_ref$georef_code, cols_to_keep)
		area_ref.sf <- try(area_ref.sf[,cols_to_keep])
		if(is(area_ref.sf, "try-error")){
			errors <<- rbind(errors, data.frame(vocabulary = water_area_ref, error = sprintf("No georef_code '%s' column in shapefile", area_ref$georef_code)))
			return(NULL)
		}
		area_ref.sf2 <- try(merge(area_ref.sf, data_to_merge, by.x = area_ref$georef_code, by.y = "area_code"))
		if(!is(area_ref.sf2,"try-error")){
			area_ref.sf2$area_code <- area_ref.sf2[[area_ref$georef_code]] 
			area_ref.sf2 <- area_ref.sf2[,colnames(war_areas)]
		}else{
			errors <<- rbind(errors, data.frame(vocabulary = water_area_ref, error = "Non-standard structure for vocabulary file"))
			return(NULL)
		}
		return(area_ref.sf2)
	}))
	return(area_refs.sf)
})
area_db.list <- area_db.list[!sapply(area_db.list, is.null)]
area_db <- do.call("rbind", area_db.list)

#errors
if(nrow(errors)>0) readr::write_csv(errors, "water_areas_issues.csv")

#normalized area db as geopackage
sf::write_sf(area_db, "water_areas_merge.gpkg")
#normalized area db as geopackage
area_db_withoutgeom <- as.data.frame(area_db)
area_db_withoutgeom$geometry <- NULL
readr::write_csv(area_db_withoutgeom, "water_areas_merge.csv")

#publish to Geoserver
dotenv::load_dot_env(file = ".env-sdilab")
GS = geosapi::GSManager$new(
	url = Sys.getenv("SDILAB_GEOSERVER_URL"),
	user = Sys.getenv("SDILAB_GEOSERVER_USER"),
	pwd = Sys.getenv("SDILAB_GEOSERVER_PWD")
)
