check_address<-function(address){
  #if(grepl("新市街",address)) {
    #road<-"新市街"
  #} else {
    road<-gsub(".*(市|區|里|鄰)(.*(路|街|大道)).*","\\2",address)
  #}
  #if(road == "民大道") road = "市民大道"
  
  if(grepl("段",address)){
    section<-gsub(".*(路|街|大道)(.*段).*","\\2",address)
    lane.vector<-check_lane(strsplit(address,("段"))[[1]][2])
  } else {
    section<-NA
    lane.vector<-check_lane(gsub(".*(路|街|大道)(.*)","\\2",address))
  }
  return(c(road,section,lane.vector))
}

check_lane<-function(address){
  if(grepl("巷",address)){
    lane<-gsub("^(.*巷).*","\\1",address)
    alley.vector<-check_alley(strsplit(address,("巷"))[[1]][2])
  } else {
    lane<-NA
    alley.vector<-check_alley(address)
  }
  lane<-lapply(gsub("十","",lane),check_digit)[[1]]
  return(c(lane,alley.vector))
}

check_alley<-function(address){
  if(grepl("弄",address)){
    alley<-gsub("^(.*弄).*","\\1",address)
    number<-strsplit(address,("弄"))[[1]][2]
  } else {
    alley<-NA
    number<-address
  }
  
  if(grepl("、|\\.|，",number)) number<-paste0(strsplit(number,"、|\\.|，")[[1]][1],"號")
  alley<-lapply(gsub("十","",alley),check_digit)[[1]]
  #print(number)
  number<-lapply(gsub("十","",number),check_digit)[[1]]
  return(c(alley,number))
}

check_digit<-function(digit){
  #print(c(digit,length(digit)))
  number.df<-data.frame("en"=as.character(c(1:9)),"zh"=c("一","二","三","四","五","六","七","八","九"),
                        "cap"=c("１","２","３","４","５","６","７","８","９"),stringsAsFactors=FALSE)
  digit<-strsplit(digit,"")[[1]]
  
  if(!is.na(digit) && length(digit)>0){
    for(l in 1:length(digit)){
      if(digit[l] %in% number.df$zh){digit[l]<-number.df$en[grep(digit[l],number.df$zh)]}
      else if(digit[l] %in% number.df$cap){digit[l]<-number.df$en[grep(digit[l],number.df$cap)]}
    }
  }
  return(paste0(digit,collapse=""))
}
