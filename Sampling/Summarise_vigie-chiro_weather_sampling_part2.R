
library(tidyverse)
library(ncdf4)
library(lubridate)
library(sf)
library(driver)

# Summary for Vigie-Chiro Sampling
meteo_wind = read_csv("C:/Users/croemer01/Documents/Donnees vigie-chiro/Meteo/data/fg_extract.csv")
meteo_rain = read_csv("C:/Users/croemer01/Documents/Donnees vigie-chiro/Meteo/data/rr_extract.csv")
meteo_temp = read_csv("C:/Users/croemer01/Documents/Donnees vigie-chiro/Meteo/data/tg_extract.csv")

meteo1 = left_join(meteo_wind, meteo_rain)
meteo2 = left_join(meteo1, meteo_temp)

# Check that dataset is in correct area
#meteo2_sf = st_as_sf(meteo2, coords = c("Longitude", "Latitude"), crs=4326, remove=FALSE)
#ggplot(meteo2_sf)+geom_sf()

# summary(meteo2$fg_day) # wind
# summary(meteo2$rr_day) # rain
# summary(meteo2$tg_day) # temperature

#write_csv(meteo2, "C:/Users/croemer01/Documents/Donnees vigie-chiro/Meteo/Meteo_summary.csv")

#### Summary for all France all dates ####

FRANCE= map_data("world", region = "France") %>% 
  st_as_sf(coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") 

names = c("rr", "fg", "tg")

rm(Meteo_BIND)
for(i in 1:length(names)){
  path <- paste0("E:/SIG/Meteo_Lea_Eobs/", names[i], "_ens_mean_0.25deg_reg_v27.0e.nc")
  name=names[i]
  ncdf <- nc_open(path)
  lon <- ncdf$dim$longitude$vals
  lat <- ncdf$dim$latitude$vals
  date <- ncdf$dim$time$vals
  date <- as_date(date,origin="1950-01-01") # as dates object
  
  # lim longitude 
  start_lon <- which(abs(lon-(min(st_coordinates(FRANCE)[,"X"])-1))==min(abs(lon-(min(st_coordinates(FRANCE)[,"X"])-1))))
  stop_lon <- which(abs(lon-(max(st_coordinates(FRANCE)[,"X"])+1))==min(abs(lon-(max(st_coordinates(FRANCE)[,"X"])+1))))
  len_lon <- stop_lon-start_lon+1
  
  # lim latitude 
  start_lat <- which(abs(lat-(min(st_coordinates(FRANCE)[,"Y"])-1))==min(abs(lat-(min(st_coordinates(FRANCE)[,"Y"])-1))))
  stop_lat <- which(abs(lat-(max(st_coordinates(FRANCE)[,"Y"])+1))==min(abs(lat-(max(st_coordinates(FRANCE)[,"Y"])+1))))
  len_lat <- stop_lat-start_lat+1
  
  # lim date : from 1980-01-01 to today
  start_date = which(date=="2014-01-01")
  end_date = length(date)
  len_date <- end_date-start_date+1
  
  # Extract data in desired range
  meteo_array <- ncvar_get(nc=ncdf,
                           varid=ncdf$var[[1]]$name,
                           start=c(start_lon,start_lat,start_date),
                           count=c(len_lon,len_lat,len_date))
  
  dimnames(meteo_array) <- list(c(lon[start_lon:stop_lon]),
                                c(lat[start_lat:stop_lat]),
                                c(as.character(date[start_date:end_date])))
  
  # Bind all values
  Meteo_temp = gather_array(meteo_array)
  if(exists("Meteo_BIND")){
    Meteo_BIND = cbind(Meteo_BIND, Meteo_temp$var)
  }else{
    Meteo_BIND = as.data.frame(Meteo_temp$var)
  }
  colnames(Meteo_BIND)[which(colnames(Meteo_BIND)=="Meteo_temp$var")]=name
}

# Summarise
Meteo_BIND %>% # France all years
  drop_na() %>% 
  gather("variable", "value") %>% 
  group_by(variable) %>% 
  summarize(mean_val = mean(value), 
            sd_val = sd(value), 
            min = min(value),
            q25 = quantile(value, probs = .25),
            q50 = quantile(value, probs = .5),
            q75 = quantile(value, probs = .75),
            max = max(value)) %>% 
  as.data.frame()

meteo2 %>% # Vigie-Chiro sampling
  select(fg_day, rr_day, tg_day) %>% 
  drop_na() %>% 
  gather("variable", "value") %>% 
  group_by(variable) %>% 
  summarize(mean_val = mean(value), 
            sd_val = sd(value), 
            min = min(value),
            q25 = quantile(value, probs = .25),
            q50 = quantile(value, probs = .5),
            q75 = quantile(value, probs = .75),
            max = max(value)) %>% 
  as.data.frame()
