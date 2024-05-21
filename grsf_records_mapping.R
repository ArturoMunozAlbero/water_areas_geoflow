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
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))
gpkgs = list.files(jobdir, pattern = "_areas.gpkg", recursive = TRUE, full.names = T)
setwd("C:/Users/artur/OneDrive/Documents/GitHub/water_areas_shapefiles/geoflow")
for(gpkg in gpkgs){
  file.copy(from = gpkg, to = getwd(), overwrite = TRUE)
}
setwd(wd)
all_features = do.call("rbind", lapply(gpkgs, function(x){sf::st_read(x)}))

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
grsf_records$combined_geom <- mapply(extract_combine_polygons, strsplit(grsf_records$record_area, ";"), list(all_features))

# Plotting using SF object (NOT WORKING)
#plot(st_geometry(grsf_records$combined_geom[9]))

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
grsf_records$combined_geom_wkt <- mapply(extract_combine_polygons_wkt, strsplit(grsf_records$record_area, ";"), list(all_features))

#PLOT AREA OF ANY RECORD
  # Convert WKT to SF object
  combined_geom_wkt <- st_as_sfc(grsf_records$combined_geom_wkt[9])
  # Plotting using WKT converted to SF object
  plot(combined_geom_wkt)


# Create centroids for each record's area
grsf_records$centroid <- lapply(strsplit(grsf_records$record_area, ";"), function(area_codes) {
  # Filter GIS data for the given area codes
  filtered_geoms <- all_features[all_features$grsf_area_code %in% area_codes, "geom"]
  
  # Combine geometries into a single multipolygon
  combined_multipolygon <- st_union(filtered_geoms)
  
  # Calculate centroid of the combined geometry
  centroid <- st_centroid(combined_multipolygon)
  
  # Convert centroid to WKT format
  centroid_wkt <- st_as_text(centroid)
  
  # Return centroid in WKT format
  return(centroid_wkt)
})

#Plot centroids
  # Convert WKT centroid to SF object
  centroid_sf <- st_as_sfc(grsf_records$centroid[9])

  # Plot the centroid using SF object
  plot(centroid_sf, add = TRUE, pch = 19, col = "red", main = "Centroid")

#Plot with FAO Coastline background
  fao_coastline <- st_combine(st_read("C:/Users/artur/OneDrive/Documents/FAO/Fish in Space/GRSF_Water_Areas_Project/FAO Areas/FAO_coastline.shp"))
  plot(fao_coastline, col = "lightblue", main = "Coastline")
  plot(combined_geom_wkt, add=TRUE, col="yellow")
  plot(centroid_sf, add = TRUE, pch = 19, col = "red", main = "Centroid")
  
  #Add more grsf records areas and centroids
  combined_geom_wkt <- st_as_sfc(grsf_records$combined_geom_wkt[17])
  plot(combined_geom_wkt, add=TRUE, col="yellow")
  centroid_sf <- st_as_sfc(grsf_records$centroid[17])
  plot(centroid_sf, add = TRUE, pch = 19, col = "red", main = "Centroid")