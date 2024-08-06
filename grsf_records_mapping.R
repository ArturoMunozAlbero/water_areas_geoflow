#Install packages
require(sf)
require(geoflow)
require(geojsonsf)
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting

setwd("C:/Users/artur/OneDrive/Documents/FAO/R GIS/GRSF_water_areas_project")
wd = getwd()

#Extract GRSF competency query directly from GRSF website?

#Read GSRF competency query CSV
grsf_records = read.csv("C:/Users/artur/OneDrive/Documents/FAO/R GIS/Learning/water_areas_publication_trial/trial6.csv")
View(grsf_records)

grsf_records_attributes = read.csv("C:/Users/artur/OneDrive/Documents/FAO/R GIS/Learning/water_areas_publication_trial/grsf_records_attributes.csv")
View(grsf_records_attributes)

#Separate areas and species from semantic id in new column
grsf_records$record_area <- (sub('^[^+]*\\+','',grsf_records$grsf_semantic_id))
#grsf_records$species <- sub('\\+.*$', '', grsf_records$grsf_semantic_id)

grsf_records <- merge(grsf_records, grsf_records_attributes[,c("uuid","url","traceability_flag","sdg_flag")],by = "uuid", all.x = TRUE)

#Get all areas from GitHub
all_areas = read.csv("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_vocabulary/all_areas.csv")
View(all_areas)

#Get all shapefiles from GitHub


gpkgs = list.files(jobdir, pattern = "_areas.gpkg", recursive = TRUE, full.names = T)
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow")
for(gpkg in gpkgs){
  file.copy(from = gpkg, to = getwd(), overwrite = TRUE)
}
setwd(wd)
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))

#all_features = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow/all_areas.gpkg")

#Concatenate namespace and area_code for obtaining grsf_area_code in all_features
all_features$grsf_area_code <- paste(all_features$namespace,all_features$area_code,sep=":")
View(all_features)


#Search each area of records in "all_features" and extract geometries, then join them
#Function to extract and combine polygons for given area codes
#extract_combine_polygons <- function(area_codes, all_features) {
  
  # Convert area codes to lowercase
  #area_codes_lower <- tolower(area_codes)
  #all_features$grsf_area_code_lower <- tolower(all_features$grsf_area_code)
  
  #Filter GIS data for the given area codes
  #filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  #filtered_geoms <- all_features[tolower(all_features$grsf_area_code) %in% area_codes, "geom"]
  #filtered_geoms <- all_features[all_features$grsf_area_code_lower %in% area_codes_lower, "geom"]
  
  #Combine geometries into a single multipolygon
  #combined_multipolygon <- st_union(filtered_geoms)
  
  #Return the combined multipolygon as an sf object
  #return(st_sf(geometry = combined_multipolygon))
#}

#Extraction added to grsf_records
#grsf_records$geo_polygon <- mapply(extract_combine_polygons, strsplit(grsf_records$record_area, ";"), list(all_features))


#Function to extract and combine polygons for given area codes in Well-Known Text (WKT) format
#extract_combine_polygons_wkt <- function(area_codes, all_features) {
  # Filter GIS data for the given area codes
  #filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  #combined_multipolygon <- st_union(filtered_geoms)
  
  # Convert the combined multipolygon to well-known text (WKT) format
  #combined_multipolygon_wkt <- st_as_text(combined_multipolygon)
  
  # Return the combined multipolygon in WKT format
  #return(combined_multipolygon_wkt)
#}
#Extraction added to grsf_records
#grsf_records$geo_polygon_wkt <- mapply(extract_combine_polygons_wkt, strsplit(grsf_records$record_area, ";"), list(all_features))


# Create centroids for each record's area
#grsf_records$centroid <- lapply(strsplit(grsf_records$record_area, ";"), function(area_codes) {
  # Filter GIS data for the given area codes
  #filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  #combined_multipolygon <- st_union(filtered_geoms)
  
  # Calculate centroid of the combined geometry
  #centroid <- st_point_on_surface(combined_multipolygon)
  
  # Convert centroid to WKT format
  #centroid_wkt <- st_as_text(centroid)
  
  # Return centroid in WKT format
  #return(centroid_wkt)
#})

################################################################

#TO DO ALL IN ONCE
# Function to extract, combine polygons, and generate required outputs for given area codes
extract_combine_polygons_and_outputs <- function(area_codes, all_features) {
  # Convert area codes to lowercase
  area_codes_lower <- tolower(area_codes)
  all_features$grsf_area_code_lower <- tolower(all_features$grsf_area_code)
  
  # Filter GIS data for the given area codes (case-insensitive)
  filtered_geoms <- all_features[all_features$grsf_area_code_lower %in% area_codes_lower, "geom"]
  #filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  # Convert the combined multipolygon to well-known text (WKT) format
  combined_multipolygon_wkt <- st_as_text(combined_multipolygon)
  
  # Calculate centroid of the combined geometry
  centroid <- st_point_on_surface(combined_multipolygon)
  
  # Convert centroid to WKT format
  centroid_wkt <- st_as_text(centroid)
  
  # Return a list with all required outputs
  return(list(multipolygon = st_sf(geometry = combined_multipolygon), 
              wkt = combined_multipolygon_wkt, 
              centroid_wkt = centroid_wkt))
}

# Apply the function to grsf_records and extract the required outputs
results <- mapply(extract_combine_polygons_and_outputs, strsplit(grsf_records$record_area, ";"), list(all_features), SIMPLIFY = FALSE)

# Extract individual results and add them to grsf_records
grsf_records$geo_polygon <- lapply(results, function(x) x$multipolygon)
grsf_records$geo_polygon_wkt <- sapply(results, function(x) x$wkt)
grsf_records$centroid <- sapply(results, function(x) x$centroid_wkt)

####################################################################

  
# Create SF object for grsf_records with the combined geometry (centroids)
grsf_records_sf_centroid = sf::st_sf(
  uuid = grsf_records[lengths(grsf_records$centroid) > 0, ]$uuid,
  url = grsf_records[lengths(grsf_records$centroid) > 0, ]$url,
  grsf_semantic_id = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  short_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$short_name,
  grsf_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_name,
  type = grsf_records[lengths(grsf_records$centroid) > 0, ]$type,
  traceability_flag = grsf_records[lengths(grsf_records$centroid) > 0, ]$traceability_flag,
  sdg_flag = grsf_records[lengths(grsf_records$centroid) > 0, ]$sdg_flag,
  record_area = grsf_records[lengths(grsf_records$centroid) > 0, ]$record_area,
  geom = st_as_sfc(grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid)
  #geom_wkt = grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid
  #geo_polygon = as(sfc_geojson(st_as_sfc(grsf_records$geo_polygon_wkt)),"character")
)

View(grsf_records_sf_centroid)
st_crs(grsf_records_sf_centroid)=4326
st_write(grsf_records_sf_centroid, "grsf_records_def.gpkg", driver = "GPKG")

# Create SF object for grsf_records with the combined geometry (centroids)
grsf_records_sf_polygon = sf::st_sf(
  uuid = grsf_records[lengths(grsf_records$centroid) > 0, ]$uuid,
  url = grsf_records[lengths(grsf_records$centroid) > 0, ]$url,
  grsf_semantic_id = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  short_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$short_name,
  grsf_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_name,
  type = grsf_records[lengths(grsf_records$centroid) > 0, ]$type,
  traceability_flag = grsf_records[lengths(grsf_records$centroid) > 0, ]$traceability_flag,
  sdg_flag = grsf_records[lengths(grsf_records$centroid) > 0, ]$sdg_flag,
  #record_area = grsf_records[lengths(grsf_records$centroid) > 0, ]$record_area,
  geom = st_as_sfc(grsf_records[lengths(grsf_records$centroid) > 0, ]$geo_polygon_wkt)
  #geom_wkt = grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid
  #geo_polygon = as(sfc_geojson(st_as_sfc(grsf_records$geo_polygon_wkt)),"character")
)

st_crs(grsf_records_sf_polygon)=4326
st_write(grsf_records_sf_polygon, "grsf_records_poly_def.gpkg", driver = "GPKG")


#Only approved records
# Create SF object for grsf_records with the combined geometry (centroids)
grsf_apprecords_sf_centroid = sf::st_sf(
  uuid = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$uuid,
  url = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$url,
  #grsf_semantic_id has to be with the new areas!!
  grsf_semantic_id = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  short_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$short_name,
  grsf_name = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$grsf_name,
  type = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$type,
  status = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$status,
  traceability_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$traceability_flag,
  sdg_flag = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$sdg_flag,
  #record_area = grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$record_area,
  geom = st_as_sfc(grsf_records[grsf_records$status == "approved" & lengths(grsf_records$centroid) > 0, ]$centroid)
  #geom_wkt = grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid
  #geo_polygon = as(sfc_geojson(st_as_sfc(grsf_records$geo_polygon_wkt)),"character")
)

View(grsf_apprecords_sf_centroid)
st_crs(grsf_apprecords_sf_centroid)=4326
st_write(grsf_apprecords_sf_centroid, "grsf_approved_records_def.gpkg", driver = "GPKG")