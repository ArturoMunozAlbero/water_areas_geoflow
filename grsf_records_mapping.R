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
grsf_records = read.csv("C:/Users/artur/OneDrive/Documents/FAO/R GIS/Learning/water_areas_publication_trial/sparql_2024-05-10_13-21-23Z.csv")
View(grsf_records)

#Extract areas from semantic id in new column
grsf_records$record_area <- (sub('.*[+]','',grsf_records$grsf_semantic_id))

#Get all areas from GitHub
all_areas = read.csv("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_vocabulary/all_areas.csv")
View(all_areas)

#Get all shapefiles from GitHub
#all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))
#gpkgs = list.files(jobdir, pattern = "_areas.gpkg", recursive = TRUE, full.names = T)
#setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow")
#for(gpkg in gpkgs){
#  file.copy(from = gpkg, to = getwd(), overwrite = TRUE)
#}
#setwd(wd)
#all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))

all_features = st_read("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow/all_areas.gpkg")

#Concatenate namespace and area_code for obtaining grsf_area_code in all_features
all_features$grsf_area_code <- paste(all_features$namespace,all_features$area_code,sep=":")
View(all_features)


#Search each area of records in "all_features" and extract geometries, then join them
#Function to extract and combine polygons for given area codes
extract_combine_polygons <- function(area_codes, all_features) {
  #Filter GIS data for the given area codes
  filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  #Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  #Return the combined multipolygon as an sf object
  return(st_sf(geometry = combined_multipolygon))
}
#Extraction added to grsf_records
grsf_records$geo_polygon <- mapply(extract_combine_polygons, strsplit(grsf_records$record_area, ";"), list(all_features))


#Function to extract and combine polygons for given area codes in Well-Known Text (WKT) format
extract_combine_polygons_wkt <- function(area_codes, all_features) {
  # Filter GIS data for the given area codes
  filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  # Convert the combined multipolygon to well-known text (WKT) format
  combined_multipolygon_wkt <- st_as_text(combined_multipolygon)
  
  # Return the combined multipolygon in WKT format
  return(combined_multipolygon_wkt)
}
#Extraction added to grsf_records
grsf_records$geo_polygon_wkt <- mapply(extract_combine_polygons_wkt, strsplit(grsf_records$record_area, ";"), list(all_features))


# Create centroids for each record's area
grsf_records$centroid <- lapply(strsplit(grsf_records$record_area, ";"), function(area_codes) {
  # Filter GIS data for the given area codes
  filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  # Calculate centroid of the combined geometry
  centroid <- st_point_on_surface(combined_multipolygon)
  
  # Convert centroid to WKT format
  centroid_wkt <- st_as_text(centroid)
  
  # Return centroid in WKT format
  return(centroid_wkt)
})

  
# Create SF object for grsf_records with the combined geometry (centroids)
grsf_records_sf = sf::st_sf(
  uuid = grsf_records[lengths(grsf_records$centroid) > 0, ]$uuid,
  short_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$short_name,
  grsf_name = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_name,
  grsf_semantic_id = grsf_records[lengths(grsf_records$centroid) > 0, ]$grsf_semantic_id,
  type = grsf_records[lengths(grsf_records$centroid) > 0, ]$type,
  record_area = grsf_records[lengths(grsf_records$centroid) > 0, ]$record_area,
  geom = st_as_sfc(grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid),
  #geom_wkt = grsf_records[lengths(grsf_records$centroid) > 0, ]$centroid
  geo_polygon = as(sfc_geojson(st_as_sfc(grsf_records$geo_polygon_wkt)),"character")
)

View(grsf_records_sf)
st_write(grsf_records_sf, "grsf_records.gpkg", driver = "GPKG")