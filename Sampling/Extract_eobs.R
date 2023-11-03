##############################################################################################################
# This script allows to extract at sites in Europe for given dates:
#              - raw mean temperature and the mean anomaly (in °C)
#              - or precipitations sum (in mm)
#              - or mean wind speed (in m.s-1)
#
# The  temperature anomaly is defined as the difference between the mean temperature of the site at 
# the given date and the mean of the mean temperature at this site for the given day and month from 1980 to 2010.
#
# It returns the data frame of each day and site (given in arguments) with a new column for the 
# temperature / precipitations / wind speed of the day (XX_day with XX = tg for temperature,
# XX = rr for precipitations, XX = fg for wind speed)
# and as many columns as the number of days before the survey specified in arguments 
# (e.g. temperature / precipitations / wind speed of the day before: 
# XX_day_before1, temperature / precipitations / wind speed two days before: 
# XX_day_before2, etc) and the same  pattern for anomalies 
# (tg_day_ano, tg_day_ano_before1, tg_day_ano_before2, etc.).
#
# With the help of Alejandro Sotillo
#
##############################################################################################################
#
# Meteorological data (from E-OBS) has to be download at this link : 
# https://surfobs.climate.copernicus.eu/dataaccess/access_eobs.php#datafiles
# Choose either  : Ensemble Mean TG / RR / FG (temperature / precipitations / wind speed) from the 
# 0.25 deg. regular grid or the 0.1 deg. regular grid (more precise but heavier)
#
##############################################################################################################
#
# Arguments : 
#
# path_to_meteo_nc : path to access to the meteo .nc file
# 
# tableDaysSites : a table with (minimum):
# -> a column named "Latitude" with the site latitude in WGS84
# -> a column named "Longitude" with the site longitude in WGS84
# -> a column named "Date" with the date of the survey for each sites
#       Dates should be formatted this way : "2020-05-26" if characters
#       They might also be c("POSIXct","POSIXt") objects
#
# nbrPreviousDays : number of previous days for which T° should be returned
# (for example "3", to obtain the temperature for the survey date but for also the three previous days)
#
# verbose : if FALSE messages are suppressed

##############################################################################################################

extract_eobs <- function(path_to_meteo_nc,tableDaysSites,nbrPreviousDays,France_SHP,verbose=T){

  #### Preliminary ####
  
  # Install and open required packages
  
    load <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
      install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
  } 
  packages <- c("ncdf4","sf","stringr","lubridate","data.table")
  load(packages)
  
  # creation of a site layer
  
  sites_SHP <- st_as_sf(tableDaysSites, coords = c("Longitude","Latitude"), crs = 4326)
  sites_SHP <- st_transform(sites_SHP,crs=2154)
  
  if (all(class(tableDaysSites[,which(colnames(tableDaysSites)=="Date")])!=c("POSIXct","POSIXt"))){
    tableDaysSites[,which(colnames(tableDaysSites)=="Date")] <- parse_date_time(
      as.character(tableDaysSites[,which(colnames(tableDaysSites)=="Date")]),"%Y-%m-%d", tz="Europe/Paris")
  }
  
  #### I. Meteo data formatting ####
  
  # Open meteo file
  
  ncdf <- nc_open(path_to_meteo_nc)
  
  if(verbose==T){ print(paste0('Extracting ',ncdf$var[[1]]$longname))}
  
  # This file has three dimensions : longitude, latitude, date
  # Extraction of the values taken by each of these dimensions
  
  lon <- ncdf$dim$longitude$vals
  lat <- ncdf$dim$latitude$vals
  date <- ncdf$dim$time$vals
  date <- as_date(date,origin="1950-01-01") # as dates object
  
  # lim date : from 1980-01-01 to today
  
  start_date = which(date=="1980-01-01")
  end_date = length(date)
  len_date <- end_date-start_date+1
  
  # lim longitude 
    
  start_lon <- which(abs(lon-(min(tableDaysSites$Longitude)-1))==min(abs(lon-(min(tableDaysSites$Longitude)-1))))
  stop_lon <- which(abs(lon-(max(tableDaysSites$Longitude)+1))==min(abs(lon-(max(tableDaysSites$Longitude)+1))))
  len_lon <- stop_lon-start_lon+1
    
  # lim latitude 
    
  start_lat <- which(abs(lat-(min(tableDaysSites$Latitude)-1))==min(abs(lat-(min(tableDaysSites$Latitude)-1))))
  stop_lat <- which(abs(lat-(max(tableDaysSites$Latitude)+1))==min(abs(lat-(max(tableDaysSites$Latitude)+1))))
  len_lat <- stop_lat-start_lat+1
  
  if(verbose==T){ print(".nc file loaded, extracting data. Stand by...")}
  
  meteo_array <- ncvar_get(nc=ncdf,
                           varid=ncdf$var[[1]]$name,
                           start=c(start_lon,start_lat,start_date),
                           count=c(len_lon,len_lat,len_date))
  
  dimnames(meteo_array) <- list(c(lon[start_lon:stop_lon]),
                                c(lat[start_lat:stop_lat]),
                                c(as.character(date[start_date:end_date])))
  
  if(verbose==T){ print("1. Data formatting OK")}
  
  #### II. Link between sites and meteo points ####
  
  ####____A. Detect longitude * latitude for which data are available ####
    
  # Take one date not to recent (data has to exist) but not old either
  # Here arbitrarily : 2021-05-15
  # And extract meteo data for each longitude * latitude at this date (array with 2 dimensions)
    
  date_map = which(date=="2021-05-15")
  
  meteo_link_array <- ncvar_get(ncdf,
                                ncdf$var[[1]]$name,
                                start=c(start_lon,start_lat,date_map),
                                count=c(len_lon,len_lat,1)) # extraction
  
  dimnames(meteo_link_array) <- list(c(lon[start_lon:stop_lon]),c(lat[start_lat:stop_lat])) # longitude and latitude coord
  
  meteo_link_arrayDT <- as.data.frame(meteo_link_array)  # array to data.frame
  
  meteo_link_arrayDT$lon <- rownames(meteo_link_arrayDT) # longitude in a column
  
  # reshape the data frame with a column longitude, a column latitude and a column with the meteo value
  
  meteo_link_arrayDT <- gather(meteo_link_arrayDT,
                               key="lat",
                               value=!!ncdf$var[[1]]$name,
                               c(1:((dim(meteo_link_arrayDT)[2])-1)))
  
  meteo_link_arrayDT <- meteo_link_arrayDT[which(!(is.na(meteo_link_arrayDT[3]))),] # remove longitude * latitude with NA
  
  meteo_link_arrayDT$coord <- paste0(meteo_link_arrayDT$lon,
                                     "_",
                                     meteo_link_arrayDT$lat) # column with the concatenation of longitude and latitude
  
  ####____B. Create a shapefile with the points of the meteo data ####
  
  coord_meteo_SHP <- st_as_sf(meteo_link_arrayDT, coords = c("lon","lat"), crs = 4326)
  coord_meteo_SHP <- st_transform(coord_meteo_SHP,crs=2154)
    
  ####____C. create the link between meteo data and sites ####
    
  # Creation of a new column in the shapefile with the sites that indicate, for each site, the closest point
  # of the meteo data
  
  sites_SHP$corresp_EObs <- coord_meteo_SHP$coord[st_nearest_feature(sites_SHP,coord_meteo_SHP)]
  
  if(verbose==T){ print("2. Link between sites and meteo points OK")}
  
  #### III. Daily value (and temperature anomaly) extraction for each night ####
  
  tableDaysSites[,paste0(ncdf$var[[1]]$name,'_','day')] <- rep(NA,dim(tableDaysSites)[1])
  
  if (nbrPreviousDays !=0){
    for (i in 1:nbrPreviousDays){
      tableDaysSites[,dim(tableDaysSites)[2]+1] <- rep(NA,dim(tableDaysSites)[1])
      colnames(tableDaysSites)[dim(tableDaysSites)[2]] <- paste0(ncdf$var[[1]]$name,"_day_before",i)
    }
  }
  
  if(ncdf$var[[1]]$name=='tg'){
    
    tableDaysSites$tg_day_ano <- rep(NA,dim(tableDaysSites)[1])
    
    if (nbrPreviousDays !=0){
      for (i in 1:nbrPreviousDays){
        
        tableDaysSites[,dim(tableDaysSites)[2]+1] <- rep(NA,dim(tableDaysSites)[1])
        colnames(tableDaysSites)[dim(tableDaysSites)[2]] <- paste0("tg_day_ano_before",i)
        }
      }
    }
  
  for (index in c(1:dim(tableDaysSites)[1])){
    
    date_day <- tableDaysSites[index,which(colnames(tableDaysSites)=="Date")]
    
    if (as.character(date_day) %in% (dimnames(meteo_array)[[3]])==T){
      
      coord_meteo <- sites_SHP$corresp_EObs[index]
      coord_lon <- str_split(coord_meteo,"_",simplify = T)[1]
      coord_lat <- str_split(coord_meteo,"_",simplify = T)[2]
      
      # daily value
      
      tableDaysSites[,paste0(ncdf$var[[1]]$name,'_','day')][index] <-
        meteo_array[coord_lon,coord_lat,as.character(date_day)]
      
      # previous days
      
      if (nbrPreviousDays!=0){
        for (i in (1:nbrPreviousDays)){
          tableDaysSites[,paste0(ncdf$var[[1]]$name,'_','day_before',i)][index] <- meteo_array[coord_lon,coord_lat,as.character(date_day-days(i))]
        }
      }
      
      #anomaly
      
      if(ncdf$var[[1]]$name=='tg'){
        if(str_sub(as.character(date_day),start = 6)!="02-29"){
        date_1980_2010 <- paste0(c(1980:2010),str_sub(date_day,start=5))
        anomaly_1980_2010 <- mean(meteo_array[coord_lon,coord_lat,date_1980_2010])
        tableDaysSites$tg_day_ano[index] <- tableDaysSites$tg_day[index]-anomaly_1980_2010
        
        if (nbrPreviousDays!=0){
          for (i in (1:nbrPreviousDays)){
            tableDaysSites[index,which(colnames(tableDaysSites)==paste0("tg_day_ano_before",i))] <-
              tableDaysSites[index,which(colnames(tableDaysSites)==paste0("tg_day_before",i))]-anomaly_1980_2010
            }
          }
        }
      }
      
    }
    
    if(verbose==T){ cat(paste0(round(index/(dim(tableDaysSites)[1])*100,digits=1),"%\r"))}
    
    
  }
  
  if(verbose==T){ print(paste0('3. ',ncdf$var[[1]]$longname,' extraction for each site & date OK'))}
  
  return(tableDaysSites)
  
  #### IV. Summary French values ####
  
  # # Subset French values
  # bboxFR = st_bbox(France_SHP)
  # meteo_array_FR = meteo_array %>%
  #   select(num_range(c(41,52)))
  
}
