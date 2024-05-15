if (grsf_creation=='from_coordinates'){
  if (grsf_sourceoftruth=='true'){
    if (grsf_licence=='na'){
      publishable<-'yes'
    }}else{
      publishable<-'no'
    }}else
      if (grsf_creation=='none'){
        if (grsf_sourceoftruth=='true'){
          if (grsf_licence=='copyright'){
            publishable<-'no'
          }else{
            publishable<-'yes'
          }}else{
            publishable<-'no'
          }}