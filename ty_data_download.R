data_dl <- function() {
  library(RCurl)
  
  link.url <- "http://data.gov.tw/iisi/logaccess/844?dataUrl=http://www.cto.moea.gov.tw/04/factory.zip&ndctype=XML&ndcnid=6569"
  raw.root <- "./data/raw"
  dest.file <- "factory.zip"
  data.path <- paste0(raw.root,"/",dest.file)
  
  download.file(link.url,data.path,mode="wb")
  unzip(data.path,exdir=raw.root)
}

raw_parse <- function() {
  library(XML)
  raw.root <- "./data/raw"
  file.name <- dir(raw.root)[grepl(".xml",dir(raw.root))]
  data.path <- paste0(raw.root,"/",file.name)
  
  data.df <- xmlToDataFrame(data.path,stringsAsFactors = FALSE)
  colnames(data.df)<-c("工廠名稱","工廠登記編號","工廠設立許可案號","工廠地址","工廠市鎮鄉村里","工廠負責人姓名",
                       "公司（營利事業）統一編號","工廠組織型態","工廠設立核准日期","工廠登記核准日期","工廠登記狀態",
                       "產業類別","主要產品")
  write.csv(data.df,"./data/csv/full_factory.csv",row.names=FALSE)
  
  taoyuan.df<-data.df[grepl("桃園市",data.df$工廠市鎮鄉村里),]
  write.csv(taoyuan.df,"./data/csv/taoyuan_factory.csv",row.names=FALSE)
  
  yangmei.df<-taoyuan.df[grepl("楊梅區",taoyuan.df$工廠市鎮鄉村里),]
  write.csv(yangmei.df,"./data/csv/yangmei_factory.csv",row.names=FALSE)
}