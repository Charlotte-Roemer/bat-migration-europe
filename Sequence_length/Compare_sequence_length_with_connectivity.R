

library(tidyverse)
library(raster)
library(sf)
library(ggeffects)
library(glmmTMB)
library(viridis)

FolderRaster = "C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/Stacked"
FolderGoalOrigin = "C:/Users/croemer01/Documents/Donnees vigie-chiro/Connectivity_maps/0_VC0V_Yves_2023-03"
SeqLength = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/SequenceDuration/Sequence_length_2022only.csv")
TableLatLon = read_delim("C:/Users/croemer01/Documents/Donnees vigie-chiro/Table_VC.csv") %>% 
  filter(protocole == "POINT_FIXE") %>% 
  dplyr::select(Participation = participation, longitude, latitude) 

# France extent
FRANCE <- map_data("world", region = "France")
FRANCE_sf = st_as_sf(
  FRANCE, coords = c("long", "lat"), crs=4326, remove=FALSE)  %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# List rasters connectivity
ListRaster = list.files(FolderRaster, pattern=".tif$")
ListGoalOrigin = list.files(FolderGoalOrigin, pattern=".csv$")
SpList0 = strsplit(ListRaster, split="_")
SpList = unique(sapply(SpList0, "[[", 5))
Listj = strsplit(ListRaster, split="_")
Listj15 = unique(sapply(Listj, "[[", 6))

# Select only samples = 2022 to avoid using the same data as for training the model
# SeqLength$Timestamp=SeqLength$TempsDebutSequence
# year(SeqLength$Timestamp)=2018 
# SeqLength2022 = subset(SeqLength, SeqLength$Timestamp == 2022)
SeqLength$Timestamp=SeqLength$TempsDebutSequence

# Add coordinates
SeqLengthCoord = left_join(SeqLength, TableLatLon) %>% 
  as.data.frame() %>% 
  drop_na(longitude)

# For each species
for (i in 1:length(SpList)){
  Sp = SpList[i]
  print(Sp)
  
  # For each 15 days connectivity
  for (j in 2:length(Listj15)){
    j15 = Listj15[j-1]
    j30 = Listj15[j]
    
    print(paste0(j15, " to ", j30))
    
    # Load raster connectivity
    Sp15 = subset(ListRaster, grepl(Sp, ListRaster) & grepl(j15, ListRaster))
    SpRaster15=raster(paste0(FolderRaster, "/", Sp15))
    
    # Load dataframes goal and origin
    Sp15Origin = subset(ListGoalOrigin, grepl(Sp, ListGoalOrigin) 
                        & grepl(j15, ListGoalOrigin) & grepl("Origin", ListGoalOrigin))
    SpOrigin15=read_delim(paste0(FolderGoalOrigin, "/", Sp15Origin)) %>% 
      st_as_sf(coords = c("x", "y"), crs=4326, remove=FALSE)
    
    Sp15Goal = subset(ListGoalOrigin, grepl(Sp, ListGoalOrigin) 
                      & grepl(j15, ListGoalOrigin) & grepl("Goal", ListGoalOrigin))
    SpGoal15=read_delim(paste0(FolderGoalOrigin, "/", Sp15Goal)) 
    
    # Convert to sf
    if(exists("c_final")){rm(c_final)}
    for(k in 1:length(names(table(SpGoal15$L2)))){
      outer_k = matrix(c(SpGoal15$x[which(SpGoal15$L1==1 & SpGoal15$L2==k)], SpGoal15$y[which(SpGoal15$L1==1 & SpGoal15$L2==k)]), nrow = nrow(SpGoal15[which(SpGoal15$L1==1 & SpGoal15$L2==k),]), ncol = 2) # exterior rings
      if(exists("list_k")){rm(list_k)}
      for (l in 1:length(names(table(SpGoal15$L1[which(SpGoal15$L2==k)])))){
        hole_l = matrix(c(SpGoal15$x[which(SpGoal15$L1==l & SpGoal15$L2==k)], SpGoal15$y[which(SpGoal15$L1==l & SpGoal15$L2==k)]), nrow = nrow(SpGoal15[which(SpGoal15$L1==l & SpGoal15$L2==k),]), ncol = 2)
        if(exists("list_k")){
          list_k[[l]] = hole_l
        }else{
          list_k = list(outer_k, hole_l)
        } 
      }
      if(exists("c_final")){
        c_final = c(c_final, st_polygon(list_k))
      }else{
        c_final = st_polygon(list_k)
      }
    }
    
    # Convert to raster and attribute value of max(connectivity)
    MaxPix = SpRaster15@data@max
    pols = st_sf(value = rep(MaxPix),
                 geometry = st_sfc(c_final),
                 crs = 4326) 
    
    ggplot(FRANCE_sf)+
      geom_sf()+
      geom_sf(data=pols, fill="dark green", col="dark green") 
    
    attr(st_geometry(pols), "bbox") = st_bbox(SpRaster15)
    r = raster(x=pols, xmn = ext_pols[1], xmx = ext_pols[2], ymn = ext_pols[3], ymx = ext_pols[4], resolution = res(SpRaster15))
    pols_raster = fasterize::fasterize(pols, r, field = "value") 
    
    # Create one map with connectivity + hotspots of activity
    List_ConHot = list(SpRaster15, pols_raster)
    s <- stack(List_ConHot) # stack rasters
    i <- (maxValue(s))>0 # select only rasters which possess at least one value > 0
    s_no_0 = s[[which(i)]]
    SpRaster15_ConHot <- calc(s_no_0, sum, na.rm=T) 
    
    # Plot
    SpRaster15_ConHot_forplot = replace(SpRaster15_ConHot, SpRaster15_ConHot>MaxPix, MaxPix) # trim uper values to MaxPix
    r_stack_df <- as.data.frame(SpRaster15_ConHot_forplot, xy = TRUE) %>% # convert to data.frame
      pivot_longer(cols = !c(x, y), 
                   names_to = 'variable', 
                   values_to = 'value')
    r_stack_df$value <- ifelse(r_stack_df$value > 0.0000001, r_stack_df$value, NA) # remove background
    
    MyColours=viridis_pal(option = "turbo")(12)[1:7]
    ggplot() +
      geom_sf(data=FRANCE_sf, fill="black") + 
      geom_raster(data=r_stack_df, aes(x = x, y = y, fill = value+1)) +
      scale_fill_gradientn(colours = MyColours,
                           trans = "log",
                           na.value = NA) 
    
    # Subset sequence length
    Seq15 = subset(SeqLengthCoord, as.Date(SeqLengthCoord$Timestamp)>as.Date(paste0("2022", "-",
                                                                                    substr(j15, 1,2), "-", 
                                                                                    substr(j15, 3,4)))
                   & as.Date(SeqLengthCoord$Timestamp)<as.Date(paste0("2022", "-",
                                                                      substr(j30, 1,2), "-", 
                                                                      substr(j30, 3,4)))
                   & grepl(Sp, SeqLengthCoord$Espece, ignore.case = T))
    
    # Mean sequence length for 15 days for each location
    Seq15mean = Seq15 %>% 
      group_by(longitude, latitude) %>% 
      summarize(mean_seq_length = mean(DureeSequence),
                n = length(DureeSequence))
    
    # Calculate correlation
    Seq15mean_sf = Seq15mean %>% 
      as.data.frame() %>% 
      st_as_sf(coords = c("longitude", "latitude"), 
               crs=4326, remove=FALSE) 
    Seq15mean_sf_FR_sf = st_filter (Seq15mean_sf, FRANCE_sf)
    Seq15mean_sf_FR_sf$ConnectivityValue = terra::extract(SpRaster15_ConHot, Seq15mean_sf_FR_sf)
    
    Seq15mean_sf_FR_sf = Seq15mean_sf_FR_sf %>% 
      mutate(Type = ifelse((ConnectivityValue>= MaxPix),"Hotspot", "Other"),
             Site = paste0(longitude, latitude)) 
    
    ggplot(Seq15mean_sf_FR_sf, aes(x = Type, y = mean_seq_length)) +
      geom_boxplot()
    
    Seq15mean_sf_Other = subset(Seq15mean_sf_FR_sf, Seq15mean_sf_FR_sf$Type=="Other")
    # plot(Seq15mean_sf_Other$ConnectivityValue, Seq15mean_sf_Other$mean_seq_length)
    # model <- lm(ConnectivityValue ~ mean_seq_length, data = Seq15mean_sf_Other)
    # abline(model)
    
    model2 = glmmTMB(ConnectivityValue ~  mean_seq_length
                     + (1|Site),
                     data = Seq15mean_sf_Other)
    pr1 = ggpredict(model2, "mean_seq_length")
    ggplot(pr1, aes(x, predicted)) +
      geom_line(linewidth=1) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
      labs(x = "Mean sequence length (s)",
           y = "Connectivity value")
    
  }
}






