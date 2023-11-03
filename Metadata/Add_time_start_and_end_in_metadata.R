
library(tidyverse)
library(lubridate)
library(suncalc)
library(sf)
library(hms)

TimeSunset = -30 # Start (time before/after sunset in minutes)
TimeSunrise = +30  # End (time before/after sunrise in minutes)
NameMetadataBMRE = "C:/Users/croemer01/Documents/Post-Doc/MIGRATION/Metadonnees/Return_from_partners_FINAL/Alona_Prylutska_2NationalParks_Metadata_table.csv"
TimeZonesShape = "C:/Users/croemer01/Documents/SIG/TimeZones/combined-shapefile.shp"

# Load metadata
MetadataBMRE=read_delim(NameMetadataBMRE) %>% 
  as.data.frame()

# Check date, lat and lon
if(class(MetadataBMRE$StartDate)=="character"){
  MetadataBMRE$StartDate = dmy(MetadataBMRE$StartDate)
}
if(class(MetadataBMRE$StartDate)=="character"){
  MetadataBMRE$EndDate = dmy(MetadataBMRE$EndDate)
}

if (NA %in% MetadataBMRE$StartDate| NA %in% MetadataBMRE$EndDate){
  print(paste0("StartDate Line 1: ", MetadataBMRE$StartDate[1]))
  print(paste0("EndDate Line 1: ", MetadataBMRE$EndDate[1]))
  stop("NA in StartDate or EndDate: probable wrong date format written by participant")
}

for (i in 1:length(MetadataBMRE)){ # Y coordinates should be greater values than X
  if(MetadataBMRE$Y[1]<MetadataBMRE$X[1]){
    print(paste0("warning: X coord greater than Y coord for ", Affiliation))
    print("thus I am inversing the column values")
    YTemp = MetadataBMRE$X
    XTemp = MetadataBMRE$Y
    MetadataBMRE$X = XTemp
    MetadataBMRE$Y = YTemp
  }
  for(j in 1:nrow(MetadataBMRE))
    if(is.na(MetadataBMRE$Y[j])){
      print(paste0("Line ", j))
      print(paste0("warning: missing Y coordinates for ", Affiliation))
    }
  if(is.na(MetadataBMRE$X[j])){
    print(paste0("warning: missing X coordinates for ", Affiliation))
  }
}

# Add time
Points = st_as_sf(MetadataBMRE, coords = c("X","Y"), crs = 4326, remove=FALSE) # Create sf to intersect with TZ
TimeZones = read_sf(TimeZonesShape) %>%  # Load time zones
  st_crop(st_bbox(Points)) %>% 
  st_buffer(2000) #add a buffer of 2 km because st_crop impacts st_intersection (forgets some points)

# Check plot
#ggplot(TimeZones)+geom_sf()+geom_sf(data=Points)

MetadataBMRE_inter = st_intersection(TimeZones, Points) %>% 
  as.data.frame

names(MetadataBMRE_inter)[names(MetadataBMRE_inter)=="tzid"] = "tz"

MetadataBMRE_inter$date = MetadataBMRE_inter$StartDate
MetadataBMRE_inter$lat = MetadataBMRE_inter$Y
MetadataBMRE_inter$lon = MetadataBMRE_inter$X

# TEST case where a table contains 2 different time zones
# TEST = rbind(MetadataBMRE_inter, 
#              data.frame("tz" = "CET", MetadataBMRE_inter[2:ncol(MetadataBMRE_inter)]))

if (exists("SunRSunS")){rm(SunRSunS)}
for (i in 1:length(unique(MetadataBMRE_inter$tz))){
  tzi = unique(MetadataBMRE_inter$tz)[i]
  print(tzi)
  TableTemp = subset(MetadataBMRE_inter, MetadataBMRE_inter$tz==tzi)
  SunRSunSTemp = getSunlightTimes(data = TableTemp,
                                  tz = tzi,
                                  keep = c("sunrise", "sunset"))
  SunRSunSTemp$sunrise = as.character(as_hms(SunRSunSTemp$sunrise+minutes(TimeSunrise)))
  SunRSunSTemp$sunset = as.character(as_hms(SunRSunSTemp$sunset+minutes(TimeSunset)))
  print(SunRSunSTemp[1,])
  if (exists("SunRSunS")){
    SunRSunS = rbind(SunRSunS, SunRSunSTemp)
  }else{
    SunRSunS = SunRSunSTemp
  }
}

MetadataBMRE_inter_join = left_join(MetadataBMRE_inter, SunRSunS) %>% 
  mutate(StartTime = sunset, EndTime = sunrise, StartDate = format(StartDate, "%d/%m/%Y")) %>% 
  select(-c(geometry, date, lat, lon, sunrise, sunset)) %>% 
  relocate(tz, .after = last_col())

write_csv(MetadataBMRE_inter_join, paste0(tools::file_path_sans_ext(NameMetadataBMRE), "_correctTime.csv"))
