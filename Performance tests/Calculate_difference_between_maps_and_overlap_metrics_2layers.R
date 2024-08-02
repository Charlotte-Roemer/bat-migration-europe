
# Si jamais ça bugue alors que ca marchait avant : redemarrer l'ordinateur

library(tidyverse)
library(data.table)
library(beepr)
library(corrplot)
library(sf)
library(grid)

DateModel="2023-06-24" #date of prediction (exactly same writing as the folder name)

SpList <- "C:/Users/croemer01/Documents/Post-Doc/Classificateur/SpeciesListComplete.csv" # species list
PredDir <- "C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/testALAN" # repertory with outputs from Predict_act

olaps <- function(sp1, sp2){
  ## Calculate Schoener's D, Warren's I, and Spearman
  ## Correlation for sp1 and sp2
  
  ## sp1 and sp2 are the relative occupancy values for each
  ## species along the same environmental gradient
  
  ## scale the values for each species 0:1
  sp1 <- sp1/sum(sp1)
  sp2 <- sp2/sum(sp2)
  
  D1_2 = 1 - sum(abs(sp1 - sp2))/2
  
  I1_2 = 1 - sum((sqrt(sp1) - sqrt(sp2))^2)/2
  
  C1_2 = cor(sp1, sp2, method = "pearson")
  
  return(list(
    D = D1_2,
    I = I1_2,
    C = C1_2))
  
}

# To plot maps of differences
Raster_difference <- function(dataCOR2, val1, val2){
  dataa_Month2 = dataCOR2 %>% 
    st_as_sf(coords=c("X", "Y"), crs=4326) %>% 
    st_transform(2154) %>% 
    mutate(x = st_coordinates(.)[,1],
           y = st_coordinates(.)[,2]) %>% 
    as.data.frame()
  
  test1 = which(names(dataa_Month2)==val1)
  test2 = which(names(dataa_Month2)==val2)
  dataa_Month3=data.frame(x=dataa_Month2$x, y=dataa_Month2$y, 
                          z=dataa_Month2[,test1] - dataa_Month2[,test2])
  dataa_Month4 = raster::rasterFromXYZ(dataa_Month3, res = 500)
  
  dataa_Month5 = dataa_Month4 %>% 
    raster::as.data.frame(xy=T)
  return(dataa_Month5)
}

#Load species list
sp_list <- fread(SpList)

# Load predictions
list_file <- list.files(PredDir,recursive=TRUE,pattern="*.csv$")
ls2 = paste(paste0(PredDir,"/",list_file, sep=""))
ld <- lapply(ls2, function(x) read_csv(x, show_col_types = F))
ld <- mapply(cbind, ld, "Species"=tstrsplit(tstrsplit(list_file, split="/")[[2]],split="_")[[1]], SIMPLIFY=F) # add column with species name
ld <- mapply(cbind, ld, "Month"=tstrsplit(tstrsplit(tstrsplit(list_file, split="/")[[2]],split="_")[[3]], split="-")[[2]], SIMPLIFY=F) # add column with month name
ld <- mapply(cbind, ld, "Day"=tstrsplit(tstrsplit(tstrsplit(list_file, split="/")[[2]],split="_")[[3]], split="-")[[3]], SIMPLIFY=F) # add column with day name
ld <- mapply(cbind, ld, "Threshold"=tstrsplit(tstrsplit(list_file, split="/")[[1]],split="_")[[1]], SIMPLIFY=F) # add column with threshold name

file_bind <- do.call("rbind",ld)

# Backtransform data
file_bind$pred=(10^(file_bind$pred))-1

ListSpdata = names(table(file_bind$Species))
#ListSpdata = c("Myodau")
#ListMonthdata = names(table(file_bind$Month))
ListMonthdata = c("03", "07", "10")

list_diff_names = c("dataa_diff_ALAN")

# For each species
Correlation_list2 = list()
for (i in 1: length(ListSpdata) ) {
  
  Sp = ListSpdata[i]
  print(Sp)
  
  # For each month
  Correlation_list = list()
  for (j in 1: length(ListMonthdata) ) {
    # For each day
    for (k in 1){
      
      Month_i = ListMonthdata[j]
      Day_name <- ifelse(k==1, "01", "15")
      print(paste0(Month_i, " ", Day_name))
      
      # Calculate correlations
      f = function(x) {
        log10(x+2) # to normalise variables to allow using pearson's correlation coefficient
      } 
      
      dataCOR = file_bind %>% # prepare table
        dplyr::select(!err) %>%
        filter (Species == Sp & Month == Month_i, Day == Day_name) %>%
        pivot_wider(names_from = Threshold, values_from = pred) 
      
      if(nrow(dataCOR)>0 & ncol(dataCOR)>6){
        dataCOR2 = dataCOR %>% 
          mutate_at(vars(matches(c("weightedALAN", "weightednoALAN"))), list(normal = f)) # normalise
        
        # Correlation matrix
        M<-cor(dataCOR2[,c("weightedALAN_normal",  "weightednoALAN_normal")])
        
        # Store in list
        #Correlation_list_temp = list(OVERlap)
        Correlation_list_temp = list(M)
        names(Correlation_list_temp) = paste0(Sp, " ", Month_i, " ", Day_name)
        Correlation_list = c(Correlation_list, Correlation_list_temp)
        
        # Plot difference map
        valALAN = 'weightedALAN_normal'
        valnoALAN = 'weightednoALAN_normal'
        dataa_diff = Raster_difference(dataCOR2, valnoALAN, valALAN)
        
        list_diff = list(dataa_diff)
        
        # Save plot
        dataa_diff = as.data.frame(list_diff[1])
        
        plot1 = ggplot(dataa_diff, aes(x, y, fill= z)) +
          geom_tile() +
          labs(title = Sp,
               subtitle = paste0("Gain d'activité après suppression de la pollution lumineuse")) +
          viridis::scale_fill_viridis(name = "Nombre de contacts par nuit",
                                      option = "A",
                                      na.value = alpha("lightgrey", 0))
        ggsave(plot1, filename = paste0(PredDir, "/", DateModel, "_", Sp, 
                                        "_", Month_i, "_", Day_name, ".png"), 
               units = "px", width = 2500, height = 1600, dpi=300)
        
        # Save csv
        write.csv(dataa_diff, paste0(PredDir, "/", DateModel, "_", Sp, 
                                     "_", Month_i, "_", Day_name, "_", ".csv"))
        
      }
    }
  }
  
  
  
}



beep(2)



