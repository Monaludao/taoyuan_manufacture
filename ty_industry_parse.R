industry_parse <- function() {
  options(stringsAsFactors = FALSE)
  
  data.root <- "./data"
  root.dir <- dir(data.root)
  file.name <- max(root.dir[grepl("data_merge",root.dir)])
  timestamp <- strftime(Sys.time(),"%Y%m%d%H%M",tz="Asia/Taipei")
  
  data.df <- read.csv(paste0(data.root,"/",file.name))
  
  library(ggplot2)
  library(ggmap)
  map.df <- data.df[!is.na(data.df$Response_X),]
  
  map.lon <- median(map.df$Response_X)-0.05
  map.lat <- median(map.df$Response_Y)

  map.cat <- c("全部","金屬","電子","機械","化學","紡織","食品")
  
  mapgilbert <- get_map(location = c(lon = map.lon, lat = map.lat), zoom = 11,
                        maptype = "toner-lite", scale = 1, language="zh-TW")
  
    
  for (i in 1:length(map.cat)){
    if (i != 1){
      map.cat.df <- map.df[grepl(map.cat[i],map.df$產業類別),]
    } else {
      map.cat.df <- map.df
    }
    mapping(i,map.cat[i],map.cat.df,mapgilbert)
  }
}

mapping <- function(i,map.cat,map.df,mapgilbert) {
  if(i == 1){
    g<- ggmap(mapgilbert) +
        coord_cartesian() +
        geom_point(data = map.df, aes(x = Response_X, y = Response_Y, colour="red",alpha = 0.01), size = .5) +
        guides(fill=FALSE, colour=FALSE, alpha=FALSE, size=FALSE)
    print(paste0("exporting 00",map.cat,"點位.png"))
    ggsave(g,filename=paste0("00",map.cat,"點位.png"),width=8,height=8)
  }
  
  g<- ggmap(mapgilbert) +
      coord_cartesian() +
      stat_binhex(data = map.df,aes(x = Response_X, y = Response_Y, alpha =.05), binwidth=c(0.006, 0.006)) +
      theme(legend.position=c(.96,.1)) + scale_alpha(guide="none") + scale_fill_gradient(low="azure2",high="red4")
  print(paste0("exporting 0",i,map.cat,"相關.png"))
  ggsave(g,filename=paste0("0",i,map.cat,"相關.png"),width=8,height=8)
}