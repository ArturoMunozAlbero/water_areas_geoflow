function(action, entity, config){
  
  if(is.null(config$fao_major_areas)){
    WFS_FAO = ows4R::WFSClient$new(
      url = "https://www.fao.org/fishery/geoserver/fifao/wfs",
      serviceVersion = "1.0.0",
      logger = "INFO"
    )
   config$fao_major_areas = WFS_FAO$getFeatures("fifao:FAO_MAJOR")
  }
  
  #country
  country = NA
  countries = entity$subjects[sapply(entity$subjects, function(x){x$key == "country"})]
  if(length(countries)>0){
    country = tolower(countries[[1]]$keywords[[1]]$name)
  }
  
  #owner
  owner = NA
  owners = entity$contacts[sapply(entity$contacts, function(x){x$role == "owner"})]
  if(length(owners)>0) owner = owners[[1]]
  
  #code_system
  code_system = NA
  code_systems = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_code_system"})]
  if(length(code_systems)>0){
    code_system = code_systems[[1]]$keywords[[1]]$name
  }
  
  #code_system_name
  code_system_name = NA
  code_system_names = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_code_system_name"})]
  if(length(code_system_names)>0){
    code_system_name = code_system_names[[1]]$keywords[[1]]$name
  }
  
  #differentiating_code_system
  differentiating_code_system = NA
  differentiating_code_systems = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_differentiating_code_system"})]
  if(length(differentiating_code_systems)>0){
    differentiating_code_system = differentiating_code_systems[[1]]$keywords[[1]]$name
  }
  #differentiating_code_system_description
  differentiating_code_system_description = NA
  differentiating_code_system_descriptions = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_differentiating_code_system_description"})]
  if(length(differentiating_code_system_descriptions)>0){
    differentiating_code_system_description = differentiating_code_system_descriptionS[[1]]$keywords[[1]]$name
  }
  
  #area_code
  area_codes = NA
  georef_code = NA
  georef_codes = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_georef_code"})]
  if(length(georef_codes)>0){
    georef_code = georef_codes[[1]]$keywords[[1]]$name
    if(georef_code %in% colnames(entity$data$features)){
      area_codes = entity$data$features[[georef_code]]
    }
  }
  
  #area_name
  area_names = NA
  georef_name = NA
  georef_names = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_georef_name"})]
  if(length(georef_names)>0){
    georef_name = georef_names[[1]]$keywords[[1]]$name
    if(georef_name %in% colnames(entity$data$features)){
      area_names = entity$data$features[[georef_name]]
    }
  }
  
  #area_type
  area_type = NA
  area_types = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_area_type"})]
  if(length(area_types)>0){
    area_type = area_types[[1]]$keywords[[1]]$name
  }
  
  #typology
  typology = NA
  typologies = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_typology"})]
  if(length(typologies)>0){
    typology = typologies[[1]]$keywords[[1]]$name
  }
  
  #species_specific
  species_specific = NA
  species_specifics = entity$subjects[sapply(entity$subjects, function(x){x$key == "grsf_species_specific"})]
  if(length(species_specifics)>0){
    species_specific = species_specifics[[1]]$keywords[[1]]$name
  }
  
  #parent_areas
  parent_areas = sapply(1:nrow(entity$data$features), function(i){
    unique(config$fao_major_areas[sf::st_intersects(entity$data$features[i,],config$fao_major_areas, sparse = F),]$F_CODE)
  })
  
  entity_vocabulary = data.frame(
    namespace = entity$identifiers$id,
    country = country,
    system_owner_code = owner$identifiers$id,
    system_owner_name = owner$organizationName,
    code_system = code_system,
    code_system_name = code_system_name,
    differentiating_code_system = differentiating_code_system,
    differentiating_code_system_description = differentiating_code_system_description,
    area_code = area_codes, #from features (shapefiles)
    area_name = area_names, #from features (shapefiles)
    area_type = area_type,
    typology = typology,
    species_specific = species_specific,
    parent_area = parent_areas
  )
  
  #write CSV
  readr::write_csv(entity_vocabulary, file.path(getwd(), "metadata", paste0(entity$identifiers$id, "_areas.csv")))
  
  #add vocabulary as entity resource
  entity$addResource("vocabulary", entity_vocabulary)
  
}