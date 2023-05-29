function(action, entity, config){
	
	vocab = entity$resources$vocabulary
	new_features = cbind(entity$data$features, vocab)
	new_features = new_features[,colnames(vocab)]
	
	#harmonized features exported as GeoPackage
	sf::st_write(new_features, file.path(getwd(), "data", paste0(entity$identifiers$id, "_harmonized", ".gpkg")))

}