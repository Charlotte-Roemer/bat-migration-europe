
# Si jamais ça bugue alors que ca marchait avant : redemarrer l'ordinateur

library(tidyverse)
#library(ENMTools)
library(data.table)
library(beepr)
library(corrplot)
library(sf)
library(grid)

DateModel="2023-06-24" #date of prediction (exactly same writing as the folder name)

SpList <- "C:/Users/croemer01/Documents/Post-Doc/Classificateur/SpeciesListComplete.csv" # species list
PredDir <- "C:/Users/croemer01/Documents/Donnees vigie-chiro/PredictionsModels/testALAN" # repertory with outputs from Predict_act

olaps <- function(sp1, sp2, sp3, sp4){
  ## Calculate Schoener's D, Warren's I, and Spearman
  ## Correlation for sp1 and sp2
  
  ## sp1 and sp2 are the relative occupancy values for each
  ## species along the same environmental gradient
  
  ## scale the values for each species 0:1
  sp1 <- sp1/sum(sp1)
  sp2 <- sp2/sum(sp2)
  sp3 <- sp3/sum(sp3)
  sp4 <- sp4/sum(sp4)
  
  D1_2 = 1 - sum(abs(sp1 - sp2))/2
  D1_3 = 1 - sum(abs(sp1 - sp3))/2
  D1_4 = 1 - sum(abs(sp1 - sp4))/2
  D2_3 = 1 - sum(abs(sp2 - sp3))/2
  D2_4 = 1 - sum(abs(sp2 - sp4))/2
  D3_4 = 1 - sum(abs(sp3 - sp4))/2
  
  I1_2 = 1 - sum((sqrt(sp1) - sqrt(sp2))^2)/2
  I1_3 = 1 - sum((sqrt(sp1) - sqrt(sp3))^2)/2
  I1_4 = 1 - sum((sqrt(sp1) - sqrt(sp4))^2)/2
  I2_3 = 1 - sum((sqrt(sp2) - sqrt(sp3))^2)/2
  I2_4 = 1 - sum((sqrt(sp2) - sqrt(sp4))^2)/2
  I3_4 = 1 - sum((sqrt(sp3) - sqrt(sp4))^2)/2
  
  C1_2 = cor(sp1, sp2, method = "pearson")
  C1_3 = cor(sp1, sp3, method = "pearson")
  C1_4 = cor(sp1, sp4, method = "pearson")
  C2_3 = cor(sp2, sp3, method = "pearson")
  C2_4 = cor(sp2, sp4, method = "pearson")
  C3_4 = cor(sp3, sp4, method = "pearson")
  
  return(list(
    D = data.frame("T0" = c(1, D1_2, D1_3, D1_4), 
                   "T50" = c(D1_2, 1, D1_3, D2_4),
                   "T90" = c(D1_3, D2_3, 1, D3_4),
                   "Tweighted" = c(D1_4, D2_4, D3_4, 1),
                   row.names = c("T0", "T50", "T90", "Tweighted")),
    I = data.frame("T0" = c(1, I1_2, I1_3, I1_4), 
                   "T50" = c(I1_2, 1, I1_3, I2_4),
                   "T90" = c(I1_3, I2_3, 1, I3_4),
                   "Tweighted" = c(I1_4, I2_4, I3_4, 1),
                   row.names = c("T0", "T50", "T90", "Tweighted")),
    C = data.frame("T0" = c(1, C1_2, C1_3, C1_4), 
                   "T50" = c(C1_2, 1, C1_3, C2_4),
                   "T90" = c(C1_3, C2_3, 1, C3_4),
                   "Tweighted" = c(C1_4, C2_4, C3_4, 1),
                   row.names = c("T0", "T50", "T90", "Tweighted"))
  ))
  
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

#ListSpdata = names(table(file_bind$Species))
ListSpdata = c("Myodau")
#ListMonthdata = names(table(file_bind$Month))
ListMonthdata = c("03", "07", "10")

list_diff_names = c("dataa_diff_0_50", "dataa_diff_0_90", "dataa_diff_0_Weighted",
                    "dataa_diff_50_90", "dataa_diff_50_Weighted", "dataa_diff_90_Weighted")

# For each species
Correlation_list2 = list()
for (i in 1: length(ListSpdata) ) {
  
  Sp = ListSpdata[i]
  print(Sp)
  
  # For each month
  Correlation_list = list()
  for (j in 1: length(ListMonthdata) ) {
    # For each day
    for (k in 1:2){
      
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
      
      if(nrow(dataCOR)>0 & ncol(dataCOR)>8){
        dataCOR2 = dataCOR %>% 
          mutate_at(vars(matches(c("0", "50", "90", "weighted"))), list(normal = f)) # normalise
        
        # Correlation matrix
        M<-cor(dataCOR2[,c("0_normal",  "50_normal",  "90_normal", "weighted_normal")])
        #OVERlap = olaps(dataCOR2$'0_normal', dataCOR2$'50_normal', 
        #                dataCOR2$'90_normal', dataCOR2$'weighted_normal')
        
        # Store in list
        #Correlation_list_temp = list(OVERlap)
        Correlation_list_temp = list(M)
        names(Correlation_list_temp) = paste0(Sp, " ", Month_i, " ", Day_name)
        Correlation_list = c(Correlation_list, Correlation_list_temp)
        
        # Plot difference map
        val0 = '0_normal'
        val50 = '50_normal'
        val90 = '90_normal'
        valW = 'weighted_normal'
        dataa_diff_0_50 = Raster_difference(dataCOR2, val0, val50)
        dataa_diff_0_90 = Raster_difference(dataCOR2, val0, val90)
        dataa_diff_0_Weighted = Raster_difference(dataCOR2, val0, valW)
        dataa_diff_50_90 = Raster_difference(dataCOR2, val50, val90)
        dataa_diff_50_Weighted = Raster_difference(dataCOR2, val50, valW)
        dataa_diff_90_Weighted = Raster_difference(dataCOR2, val90, valW)
        
        list_diff = list(dataa_diff_0_50, dataa_diff_0_90, dataa_diff_0_Weighted,
                      dataa_diff_50_90, dataa_diff_50_Weighted, dataa_diff_90_Weighted)
        
        # For each pair of datasets
        for(a in 1:length(list_diff)){
          dataa_diff = as.data.frame(list_diff[a])
          name_diff = paste0(sapply(strsplit(list_diff_names[a], "_"), function(x) x[3]),
                             " - ",
                             sapply(strsplit(list_diff_names[a], "_"), function(x) x[4]))
          
          
          plot1 = ggplot(dataa_diff, aes(x, y, fill= z)) +
            geom_tile() +
            labs(title = Sp,
                 subtitle = paste0("Difference of predictions: ", name_diff)) +
            viridis::scale_fill_viridis(name = "Bat passes per night",
                                        option = "A",
                                        limits = c(-1,1),
                                        na.value = alpha("lightgrey", 0))
          ggsave(plot1, filename = paste0(PredDir, "/", DateModel, "_", Sp, 
                                          "_", Month_i, "_", Day_name, "_", name_diff, ".png"), 
                 units = "px", width = 2000, height = 1600, dpi=300)
          
        }
      }
    }
  }
  
  # Calculate mean value of each correlation accross all dates
  Correlation_mean = Reduce(`+`, Correlation_list) / length(Correlation_list)
  Correlation_list_temp2 = list(Correlation_mean)
  names(Correlation_list_temp2) = paste0(Sp)
  Correlation_list2 = c(Correlation_list2, Correlation_list_temp2)
}

TEST = lapply(Correlation_list2, function(x) {x$RowSum<-rowSums(x,na.rm=T)})

beep(2)

ENMTools::raster.overlap()


olaps(dataCOR2$'0_normal', dataCOR2$'50_normal')

