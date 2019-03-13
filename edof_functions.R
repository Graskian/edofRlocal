# Personal eBird data extraction library Version 1.01

library(rebird)

ebaseFilter <- function(loc,species=NULL,back=30,ename=F){
  
  dat <- ebirdregion(loc=loc,species=species,back=back,provisional=T)
  if(dim(dat)[1]==0){
    dat <- NULL
  }else{
    if(ename==F){
      dat <- data.frame(obsDt=dat$obsDt,comName=dat$speciesCode,howMany=dat$howMany,locName=dat$locName)
    }else{
      dat <- data.frame(obsDt=dat$obsDt,comName=dat$comName,howMany=dat$howMany,locName=dat$locName)
    }  
  }
  
  return(dat)
  
}

ebaseFilterEU <- function(locs="europe",spc="all",days=30){
  
  dat <- data.frame(obsDT='N',comName='N',howMany=0,locName='N',country='N')[-1,]
  if(locs[1]=="europe"){
    locs <- c('DK','SE','NO','FR','DE','NL','GB','CH','LU','BE','ES','PT')
  }
  
  if(spc == "all"){
    spc <- c("sooshe","manshe","lcspet","yebwar3","palwar5","duswar",
             "refblu","radwar1","bripet","corshe","greshe","feapet1",
             "reevir1")
  }
  
  for(i in 1:length(locs)){
    
    for(j in 1:length(spc)){
      
      datn <- ebaseFilter(locs[i],spc[j],back=days)
      if(!is.null(datn)){
        dat <- rbind(dat,cbind(datn,country=locs[i]))
      }
      
    }
    
  }
  
  if(dim(dat)[1]==0){
    dat <- NULL
  }else{
    dat$obsDt <- as.Date(dat$obsDt)
    dat$comName <- codetodan(dat$comName)
    dat <- dat[order(dat$obsDt,decreasing=T),]
    row.names(dat) <- NULL
  }
  
  return(dat)
  
}

DKUfilter <- function(x){
  
  rem <- numeric(0)
  rem <- c(rem,which(is.na(x$howMany)))
  rem <- c(rem,which(x$comName=="Kuhls Skråpe" & x$country=="ES" & x$howMany < 300))
  rem <- c(rem,which(x$comName=="Almindelig Skråpe" & x$country=="ES" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Almindelig Skråpe" & x$country=="GB" & x$howMany < 3))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="ES" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="GB" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="FR" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="PT" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="NL" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="SE" & x$howMany < 2))
  rem <- c(rem,which(x$comName=="Hvidbrynet Løvsanger" & x$country=="DE" & x$howMany < 2))
  
  x <- x[-rem,]
  
  return(x)
  
}

codetodan <- function(x){
  
  x <- gsub('yebwar3','Hvidbrynet Løvsanger',x)
  x <- gsub('duswar','Brun Løvsanger',x)
  x <- gsub('lcspet','Stor Stormsvale',x)
  x <- gsub('manshe','Almindelig Skråpe',x)
  x <- gsub('sooshe','Sodfarvet Skråpe',x)
  x <- gsub('palwar5','Fuglekongesanger',x)
  x <- gsub('refblu','Blåstjert',x)
  x <- gsub('radwar1','Schwarz Løvsanger',x)
  x <- gsub('bripet','Lille Stormsvale',x)
  x <- gsub('corshe','Kuhls Skråpe',x)
  x <- gsub('greshe','Storskråpe',x)
  x <- gsub('feapet1',"Kap-Verde Petrel",x)
  x <- gsub('brnboo',"Brun Sule",x)
  x <- gsub('reevir1',"Rødøjet Vireo",x)
  
}

engtodan <- function(x){
  
  x <- gsub('Yellow-browed Warbler','Hvidbrynet Løvsanger',x)
  x <- gsub('Dusky Warbler','Brun Løvsanger',x)
  x <- gsub('Leach\'s Storm-Petrel','Stor Stormsvale',x)
  x <- gsub('Manx Shearwater','Almindelig Skråpe',x)
  x <- gsub('Sooty Shearwater','Sodfarvet Skråpe',x)
  x <- gsub('Pallas\'s Leaf Warbler','Fuglekongesanger',x)
  x <- gsub('Red-flanked Bluetail','Blåstjert',x)
  x <- gsub('Radde\'s Warbler','Schwarz Løvsanger',x)
  
}
