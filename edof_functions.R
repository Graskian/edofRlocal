library(rebird)

ebaseFilter <- function(loc,species,back=30){
  
  dat <- ebirdregion(loc=loc,species=species,back=back,provisional=T)
  if(dim(dat)[1]==0){
    dat <- NULL
  }else{
    dat <- as.data.frame(dat[,c(6,2,7,5)])
  }
  
  return(dat)
  
}

ebaseFilterEU <- function(locs="europe",spc="all",days=30){
  
  dat <- data.frame(obsDT='N',comName='N',howMany=0,locName='N',country='N')[-1,]
  if(locs=="europe"){
    locs <- c('DK','SE','NO','FR','DE')
  }
  
  if(spc == "all"){
    spc <- c("sooshe","manshe","lcspet","yebwar3","palwar5","duswar")
  }
  
  for(i in 1:length(locs)){
    
    for(j in 1:length(spc)){
      
      datn <- ebaseFilter(locs[i],spc[j],back=days)
      if(!is.null(datn)){
        dat <- rbind(dat,cbind(datn,country=locs[i]))
      }
      
    }
    
  }
  
  dat$obsDt <- as.Date(dat$obsDt)
  dat$comName <- engtodan(dat$comName)
  dat <- dat[order(dat$obsDt,decreasing=T),]
  row.names(dat) <- NULL
  return(dat)
  
}

engtodan <- function(x){
  
  x <- gsub('Yellow-browed Warbler','Hvidbrynet Løvsanger',x)
  x <- gsub('Dusky Warbler','Brun Løvsanger',x)
  x <- gsub('Leach\'s Storm-Petrel','Stor Stormsvale',x)
  x <- gsub('Manx Shearwater','Almindelig Skråpe',x)
  x <- gsub('Sooty Shearwater','Sodfarvet Skråpe',x)
  x <- gsub('Pallas\'s Leaf Warbler','Fuglekongesanger',x)
  
}