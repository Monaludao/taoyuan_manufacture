export_address_query <- function() {
  options(stringsAsFactors = FALSE)
  
  data.path <- "./data/csv/taoyuan_factory.csv"
  timestamp <- strftime(Sys.time(),"%Y%m%d%H%M",tz="Asia/Taipei")
  
  data.df <- read.csv(data.path)
  data.df$Address <- fix_address(data.df$工廠地址)
  
  if(file.exists("./data/addressbook1.csv")){
    addressbook1.df <- read.csv("./data/addressbook1.csv")
    merge.df <- merge(data.df,addressbook1.df,by="Address",all.x=TRUE)
    lackaddress.df <- merge.df[is.na(merge.df$Response_Address),]
    
    if(file.exists("./data/addressbook2.csv")){
      addressbook2.df <- read.csv("./data/addressbook2.csv")
      
      lackaddress.df$Address <- gsub("*區(.*里)*","區",lackaddress.df$Address)
      lackaddress.df$Response_Address <- NULL
      lackaddress.df$Response_X <- NULL
      lackaddress.df$Response_Y <- NULL
      
      merge.nonvillage.df <- merge(lackaddress.df,addressbook2.df,by="Address",all.x=TRUE)
      
      lackaddress.df <- merge.nonvillage.df[is.na(merge.nonvillage.df$Response_Address),]
      merge.df <- rbind(merge.df,merge.nonvillage.df)
    }
    
    data.df <- lackaddress.df
    write.csv(merge.df,paste0("./data/data_merge_",timestamp,".csv"),row.names=FALSE)
  }
  
  address.vector <- unique(data.df$Address)
  
  for(i in 1:ceiling(length(address.vector)/10000)){
    output.vector <- address.vector[(1+(10000*(i-1))):(10000*i)]
    output.vector <- output.vector[!is.na(output.vector)]
    id.vector <- c(1:length(output.vector))
    output.df <- data.frame("id"=id.vector,"Address"=output.vector,"Response_Address"=NA,"Response_X"=NA,"Response_y"=NA,stringsAsFactors = FALSE)
    output.df$Address <- gsub("<U+.*>","?",output.df$Address)
    write.csv(output.df,paste0("./addressquery/lackaddress_",i,"_",timestamp,".csv"),row.names=FALSE)
    output.df$Address <- gsub("*區(.*里)*","區",output.df$Address)
    write.csv(output.df,paste0("./addressquery/lackaddress_",i,"_nonvilliage_",timestamp,".csv"),row.names=FALSE)
  }
}

fix_address <- function(address.vector) {
  address.vector <- gsub("新竹科學工業園區","",address.vector)
  address.vector <- gsub("觀音工業區","",address.vector)
  address.vector <- gsub("大園工業區","",address.vector)
  address.vector <- gsub("中壢工業區","",address.vector)
  address.vector <- gsub("平鎮工業區","",address.vector)
  address.vector <- gsub("(埔心)?幼獅工業區","",address.vector)
  address.vector <- gsub("工業區","",address.vector)
  return(address.vector)
}

read_address_response <- function() {
  source("check_address.R", encoding="UTF-8")
  
  options(stringsAsFactors = FALSE)
  
  data.root <- "./addressresponse/"
  file.list <- dir(data.root)
  file.list1 <- file.list[!grepl("nonvillage",file.list)]
  file.list2 <- file.list[grepl("nonvillage",file.list)]
  file.list <- list(file.list1,file.list2)
  
  for(k in 1:2){
    address.df <- data.frame()
    
    for(i in 1:length(file.list[[k]])){
      response.df <- read.csv(paste0(data.root,file.list[[k]][i]))
      address.df <- rbind(address.df,response.df)
    }
    address.df <- address.df[!grepl("<",address.df$id),]
    address.df <- address.df[!grepl("找不到",address.df$Response_Address),]
    address.df <- address.df[!grepl("找不到",address.df$Response_X),]
    address.df$id <- NULL
    address.df<-unique(address.df)
    
    write.csv(address.df,paste0("./data/addressbook",k,".csv"),row.names=FALSE)
  }
}