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
  map.df <- map.df[grepl("機械設備",map.df$產業類別),]
  
  mapgilbert <- get_map(location = c(lon = median(map.df$Response_X)-0.05, lat = median(map.df$Response_Y)), zoom = 11,
                        maptype = "toner-lite", scale = 1, language="zh-TW")
  ggmap(mapgilbert) +
    geom_point(data = map.df, aes(x = Response_X, y = Response_Y, fill = "red", alpha = 0.8), size = 1, shape = 21) +
    guides(fill=FALSE, alpha=FALSE, size=FALSE)
  
    #stat_density2d(aes(x = map.df$Response_X, y = map.df$Response_Y, fill = ..level.., alpha = ..level..),
                   #size = 2, bins = 4, data = map.df$工廠登記工廠設立核准日期, geom = "polygon") +
    #scale_fill_gradient("Violent\nCrime\nDensity")
    
    
}