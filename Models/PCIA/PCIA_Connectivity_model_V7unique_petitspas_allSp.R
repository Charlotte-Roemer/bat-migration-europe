

library(data.table)
library(tidyverse)
library(raster)
library(sf)
library(gdistance)
library(geosphere)

Name = "0_VC0V_Yves_2023-03" # "weighted_2022-08-15"
THETA = 0.1
Time_gaps = 15 # in days
N_paths = 500 # number of pathways to calculate for each time gap


START=Sys.time()

List_Species = c(
  #"Barbar", "Eptnil", "Eptser", "Hypsav", "Minsch", "Myoalc", "Myobec", 
  "Myocap", "Myodau", "Myodas", "Myoema", "Myomys", "Myonat", "Nyclas",
  "Nyclei", "Nycnoc", "Pipkuh", "Pippip", "Pipnat", "Pippyg", "Pleaur", 
  "Pleaus", "Plemac", "Rhieur", "Rhifer", "Rhihip", "Tadten", "Vesmur")
ListTimes = c("0315", "0401", "0415", "0501", "0515", "0601", 
              #"0615", "0701", "0715",
              "0801", "0815", "0901", "0915", "1001")

dir.create(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", Name))

for (i in 1:length(List_Species)){
  Sp = List_Species[i]
  print(Sp)
  if(Sp %in% c("Nycnoc", "Nyclei", "Nyclas", "Pipnat", "Tadten", "Minsch", "Vesmur")) {
    DistanceMaxSp = 35
  }else if(Sp %in% c("Pipkuh", "Pippip", "Hypsav", "Eptser", "Pippyg", "Eptnil")){
    DistanceMaxSp = 20
  }else{
    DistanceMaxSp = 10
  }
  for (j in 1:length(ListTimes)){
    Date = ListTimes[j]
    print(Date)
    
    if(TRUE %in% str_detect(Date, pattern = c("03", "04", "05", "06"))){
      Season = "Spring"
    }else{
      Season = "Autumn"
    }
    
    # Read Origin
    Origin = fread(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/",
                          Name, "/", Sp, "_", Date, "_", "Origin", ".csv"))
    # Origin = fread(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/",
    #                       Name, "/", Sp, "_", Date, "_", "Origin", ".csv"))
    
    # Read Goal
    Goal = fread(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/",
                        Name, "/", Sp, "_", Date, "_", "Goal", ".csv"))
    # Goal = fread(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/",
    #                     Name, "/", Sp, "_", Date, "_", "Goal", ".csv"))
    
    # Read Transition
    land_cond_sub = readRDS(paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/",
                                   Name, "/", Sp, "_", Season, "_", "Transition", ".rds"))
    # land_cond_sub = readRDS(paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/",
    #                                Name, "/", Sp, "_", Season, "_", "Transition", ".rds"))
    
    k=1
    while (k < (N_paths + 1)){
      print(k)
      ID_Origin <- sample(Origin$id, size=1) #draw random points (random pairs)
      ID_Goal <- sample(Goal$id, size=1)
      pt_Origin <- SpatialPoints(cbind(Origin$x[Origin$id == ID_Origin], 
                                       Origin$y[Origin$id == ID_Origin])) #get xy for each point
      pt_Goal <- SpatialPoints(cbind(Goal$x[Goal$id == ID_Goal], 
                                     Goal$y[Goal$id == ID_Goal])) #get xy for each point
      
      
      # Check that the direction of the flyway corresponds to long-distance migration
      
      pt_Origin_v = as.data.frame(pt_Origin)
      names(pt_Origin_v) = c("x", "y")
      pt_Goal_v = as.data.frame(pt_Goal)
      names(pt_Goal_v) = c("x", "y")
      
      pt_Origin_sf = st_as_sf(pt_Origin_v, coords = c("x", "y"), crs=4326, remove=FALSE) %>% 
        #  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
        st_transform(2154)
      pt_Goal_sf = st_as_sf(pt_Goal_v, coords = c("x", "y"), crs=4326, remove=FALSE) %>% 
        #  st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
        st_transform(2154)
      
      my_data_points=as.data.frame(rbind(st_coordinates(pt_Origin_sf), st_coordinates(pt_Goal_sf)))
      Distance = dist(my_data_points)[1]/1000
      x_vecteur = my_data_points$X[2]-my_data_points$X[1]
      y_vecteur = my_data_points$Y[2]-my_data_points$Y[1]
      
      if(x_vecteur == 0 | y_vecteur == 0){
        # does not go further because origin and goal are probably the same point
      }else{
        
        orientation<-vector(length=0)
        if (x_vecteur>0 & y_vecteur>0) { 
          orientation=180*(atan(y_vecteur/-x_vecteur)/pi)+90 # ok
        }else {
          if (x_vecteur<0 & y_vecteur>0) { 
            orientation=(180*(atan(-x_vecteur/(-y_vecteur))/pi))+360 #ok
          }else {
            if (x_vecteur<0 & y_vecteur<0) { 
              orientation=(180*(atan((-y_vecteur)/(x_vecteur))/pi))+270 #ok
            }else {
              if (x_vecteur>0 & y_vecteur<0) { 
                orientation=(180*(atan(x_vecteur/(y_vecteur))/pi))+180 #ok
              }
            }}}
        
        
        # Plot check
        library(maps)
        countries <- c("France")
        France <- map_data("world", region = countries)
        
        France_sf = st_as_sf(
          France, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
          group_by(group) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON") %>%
          #st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
          st_transform(2154)
        
        ggplot(France_sf) +
          geom_sf() +
          geom_sf(data = pt_Origin_sf, col = "blue") +
          geom_sf(data = pt_Goal_sf, col = "green")
        
        # Radar of angle
        dataT = data.frame("x" = round(orientation), "y" = 10)
        if(dataT$x == 360){
          dataT$x=0
        }
        ggplot(dataT, aes(x, y)) +
          geom_col(aes(x, y), width = 5)  +
          scale_x_continuous(breaks = seq(0, 359, by = 10), limits = c(-10, 350)) +
          coord_polar(start = -pi/18, clip = "off")
        
        Tolerance = 6.6
        
        if(Distance<(Time_gaps*DistanceMaxSp)){ 
          
          # Change angle so that in Spring, North-East = 0 and in Autumn, South-West = 0
          if(Season == "Spring"){
            orientation2 = ifelse(orientation>45, orientation-45, -orientation-45+360)
          }
          if(Season == "Autumn"){
            orientation2 = ifelse(orientation>225, orientation-225, -orientation-225+360)
          }
          orientation3 = ifelse(orientation2<180, orientation2, -orientation2+360)
          
          if(orientation3>90){ # if wrong direction then stop unless short distance
            AngleDist = log10(orientation2^2*Distance+1)
            if(AngleDist<Tolerance){
              
              # Calculate paths
              pasT <- passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA)
              
              # Save result
              UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), "_", round(runif(1, 1, 100000000000)))
              raster::writeRaster(pasT, paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", Name, "/",
                                               Name, "_", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
              # raster::writeRaster(pasT, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/TEST",
              #                                  "/", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
              k=k+1
              
            }else{
              #warning("AngleDist > Tolerance")
            }
          }
          # Calculate paths
          pasT <- passage(land_cond_sub, pt_Origin, pt_Goal, theta=THETA)
          
          # Save result
          UniqueName = paste0(format(Sys.Date(), "%Y%m%d"), "_", format(Sys.time(), "%H%M%S"), "_", round(runif(1, 1, 100000000000)))
          raster::writeRaster(pasT, paste0("/mnt/beegfs/croemer/VigieChiro/Connectivity_maps/", Name, "/",
                                           Name, "_", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
          # raster::writeRaster(pasT, paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/TEST",
          #                                  "/", Sp, "_", Date, "_", THETA, "_", UniqueName, ".tif"), overwrite=TRUE)
          k=k+1
        }else{
          #warning("Distance > 525 km")
        }
      }
    }
  }
}

END=Sys.time()
TIMEDIFF=END-START
TIMEDIFF

# beep(2)



# plot(Raster_TRANSITION_wtNA)
# plot(pt_Origin, add=T)
# plot(pt_Goal, add=T)
# plot(pasT)



