
#Function for map analysis
#Do be called in Household Data and Migration script

map_analysis <- function(df, pri) {
  df <- as.data.frame(df)
  
  #Merge to graph
  npl <- merge(npl_map, districtlevel,by.x="NAME_3", by.y="dist")
  
  #graph
  np_map <- fortify(npl, region="NAME_3")
  choro_dat <- data.frame(region=npl_map@data$NAME_3,
                          value=npl@data$Total_absentee,
                          stringsAsFactors=FALSE)
  
  gg <- ggplot()
  
  gg <- gg + geom_map(data=np_map, map=np_map,
                      aes(x=long, y=lat, map_id=id),
                      color="#b2b2b200",fill="#ffffff92", size=0.25)
  
  gg <- gg + geom_map(data=choro_dat, map=np_map,
                      aes(fill=value, map_id=region),
                      color="#b2b2b2", size=0.25)
  
  gg <- gg + scale_fill_viridis(name="Total_absentee",
                                alpha=0.75)
  gg <- gg + theme_map()
  gg <- gg + theme(legend.position="bottom")
  return(gg)
  
}

#find the number of household with absentee,

 