export_address_query <- function() {
  options(stringsAsFactors = FALSE)
  
  data.path <- "./data/csv/taoyuan_factory.csv"
  
  data.df <- read.csv(data.path)
  
  if(file.exists("./data/addressbook.csv")){
    addressbook.df<-read.csv("./data/addressbook.csv")
  }
  
  address.vector <- unique(data.df$工廠地址)
  
  for(i in 1:ceiling(length(address.vector)/10000)){
    output.vector <- address.vector[(1+(10000*(i-1))):(10000*i)]
    output.vector <- output.vector[!is.na(output.vector)]
    id.vector <- c(1:length(output.vector))
    output.df <- data.frame("id"=id.vector,"Address"=output.vector,"Response_Address"=NA,"Response_X"=NA,"Response_y"=NA,stringsAsFactors = FALSE)
    output.df$Address <- gsub("<U+.*>","?",output.df$Address)
    
    write.csv(output.df,paste0("./addressquery/lackaddress_",i,".csv"),row.names=FALSE)
  }
}

read_address_response <- function() {
  source("check_address.R", encoding="UTF-8")
  
  options(stringsAsFactors = FALSE)
  
  data.root <- "./addressresponse/"
  file.list <- dir(data.root)
  
  address.df <- data.frame()
  
  for(i in 1:length(file.list)){
    response.df <- read.csv(paste0(data.root,file.list[i]))
    address.df <- rbind(address.df,response.df)
  }
  address.df <- address.df[!grepl("<",address.df$id),]
  address.df <- address.df[!grepl("找不到",address.df$Response_Address),]
  
  #addressbook.df <- data.frame()
  
  #for(i in 1:nrow(address.df)){
    #address <- address.df[i,]$Response_Address
    #address.city <- gsub("(^.*市).*","\\1",address)
    #address.dist <- gsub("^.*市(.*區).*","\\1",address)
    #address.village <- gsub("^.*區(.*里).*","\\1",address)
    #address.neighbour <- gsub("^.*里(.*鄰).*","\\1",address)
    #address.vector <- check_address(address)
    #address.road <- gsub("NA","",paste(address.vector[1:2],collapse = ""))
    #address.output <- c(address.df[i,]$Response_Address,address.df[i,]$Response_X,address.df[i,]$Response_Y,
                        #address.city,address.dist,address.village,address.neighbour,address.road,
                        #address.vector[3],address.vector[4],address.vector[5])
    
    #addressbook.df <- rbind(addressbook.df,address.output)
  #}
  #colnames(addressbook.df) <- c("Response_Address","Response_X","Response_Y","city","dist","village","neighbour",
                                #"road","lane","alley","number")
  #write.csv(addressbook.df,"./data/addressbook.csv",row.names=FALSE)
  write.csv(address.df,"./data/addressbook.csv",row.names=FALSE)
}