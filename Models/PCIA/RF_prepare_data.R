#------------------------------------------------------------------------------#
#                      Function to prepare data                                #
#------------------------------------------------------------------------------#

prepare_data <- function(ARGS, FPAR, FSL){
  
  
  # Read bat activity data
  DataCPL2  = fread(paste0(ARGS[1],".csv"))
  DataCPL2$Nuit=as.Date(DataCPL2$Nuit)
  DataCPL3= DataCPL2 %>% 
    dplyr::filter(Nuit < DateLimit)
  
  # Read predictor table
  CoordSIG=fread(paste0(ARGS[2],".csv"))
  CoordSIG = CoordSIG %>% 
    rename(longitude = CoordinateNames[1], 
           latitude = CoordinateNames[2])
  
  CoordSIG = CoordSIG %>%
    rename_all(~str_replace_all(.,"\\.x",""))
  CoordSIG <- CoordSIG %>% 
    select(-contains(".y"))
  
  # Read participation and locality data
  Particip=read_delim(FPAR,delim=";")
  SiteLoc=fread(FSL)
  
  # Identifies sites recorded near bat roosts !!! Remove these sites ???
  Gite=mapply(function(x,y)
    ((grepl(paste0(y,"="),x))|(grepl(paste0(y," ="),x)))
    ,SiteLoc$commentaire
    ,SiteLoc$localite)
  SiteLoc$SpGite=as.numeric(Gite)
  SiteLoc = SiteLoc %>% 
    mutate_at(.vars = c("longitude", "latitude"), 
              .fun = function(x) as.numeric(gsub(",", "\\.", x)))
  
  # List coordinates existing in bat activity data to help add 0 in nb_contacts later
  ListPar=levels(as.factor(DataCPL3$participation))
  SelPar=subset(Particip,Particip$participation %in% ListPar)
  SelParSL=merge(SiteLoc,SelPar,by.x=c("site","nom"),by.y=c("site","point"))
  CoordPar=aggregate(SelParSL$participation
                     ,by=c(list("longitude" = SelParSL$longitude),
                           list("latitude" = SelParSL$latitude),
                           list("participation" = SelParSL$participation))
                     ,FUN=length)
  CoordPar$x=NULL
  
  # Merge list of coordinates with environmental variables
  CoordPS=merge(CoordPar,CoordSIG,by=c("longitude","latitude"))
  CoordPS[is.na(CoordPS)]=0
  testPar=grepl(ARGS[6],names(CoordPS))
  numPar=subset(c(1:length(testPar)),testPar)
  CoordPS$participation=as.data.frame(CoordPS)[,numPar[1]]
  
  return(list(
  CoordPS, # environmental variables
  DataCPL3, # bat activity (without absence data)
  SelParSL # list of sampling sessions to know when to add absence data
  ))
  
}