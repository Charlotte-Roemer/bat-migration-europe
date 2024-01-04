
# We need to merge the capture data with environmental data
# so that we can make acoustic predictions for each ***date*** of capture !

library(tidyverse)
library(data.table)
library(sf)
library(spdep)


args = "C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Datacapture_MNHN_traits2021_envoyé.csv" # file with capture data
args[2]=40 #number of coordinates projections (must be a division of 360)
args[3] = "C:/Users/croemer01/Documents/Donnees vigie-chiro/SpeciesList.csv" #Species list
args[4] = "C:/Users/croemer01/Documents/SIG/SIG_Post-Doc_MNHN/ADMIN-EXPRESS-COG/ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-03-25/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-03-25/ADE-COG_2-1_SHP_WGS84G_FRA/COMMUNE.shp" # Municipal corporations
args[5]="C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_FR_sites_localites.csv" # grid on which predictions are made
args[6] = "C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/GCLR_EXPORT_2022_n20_Data_biometrie_LR_MNHN_29_07_2022__202207291639.csv" # file with capture data from LR
args[7] = "C:/Users/croemer01/Documents/SIG/SIG_Post-Doc_MNHN/ADMIN-EXPRESS-COG/ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-03-25/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-03-25/ADE-COG_2-1_SHP_WGS84G_FRA/ENTITE_RATTACHEE.shp" # Municipal corporations (old)


# Load Species list
SpeciesList= read_delim(args[3], delim=";")

####  Load capture data and correct coordinates and INSEE #### 
TableCapture0= as.data.frame(read_delim(args[1], delim=";"))
TableCapture0=TableCapture0[!is.na(TableCapture0$Y_CENTROID_COMMUNE),]
TableCapture0$X_CENTROID_COMMUNE=TableCapture0$X_CENTROID_COMMUNE*100 # to obtain real L93
TableCapture0$Y_CENTROID_COMMUNE=TableCapture0$Y_CENTROID_COMMUNE*100 # to obtain real L93
TableCapture0$INSEE_COM = ifelse(nchar(TableCapture0$INSEE_COM) == 4, # the first 0 is sometimes missing
                                 paste0("0", TableCapture0$INSEE_COM), 
                                 TableCapture0$INSEE_COM)
TableCaptureSummaryGiteMinsch=TableCapture0 %>% 
  filter(TAXON=="Miniopterus schreibersii") %>% 
  count(DATE) %>% 
  mutate(SpGite = ifelse(n>20, 1, 0)) %>% # if more than 20 Minsch captured by night, site is probably a roost
  mutate(n=NULL)

TableCapture0 = left_join(TableCapture0, TableCaptureSummaryGiteMinsch)

# Load LR capture data and merge
TableCaptureLR= as.data.frame(read_delim(args[6], delim=","))
TableCaptureLR = TableCaptureLR %>% 
  select("date_obs", "code_insee", "nom_vern", "gîte")

# /!\ I removed all Rhino + Pleco + Myotis species here
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Eptesicus serotinus "| TableCaptureLR$nom_vern =="Sérotine commune"| TableCaptureLR$nom_vern =="Eptesicus serotinus Schreber 1774"| TableCaptureLR$nom_vern =="EPTSER"| TableCaptureLR$nom_vern =="Eptser"| TableCaptureLR$nom_vern =="Serotine commune"]<- "Eptesicus serotinus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Eptesicus nilssoni Keyserling  Blasius 1839"| TableCaptureLR$nom_vern =="Eptesicus nilssonii "| TableCaptureLR$nom_vern =="Eptesicus nilssonii Keyserling  Blasius 1839"| TableCaptureLR$nom_vern =="EPTNIL"]<- "Eptesicus nilssonii"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "NLAS"| TableCaptureLR$nom_vern =="NYCLAS"| TableCaptureLR$nom_vern =="Grande Noctule"]<- "Nyctalus lasiopterus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Nyctalus leisleri Kuhl 1817"| TableCaptureLR$nom_vern =="Nyctalus leisleri "| TableCaptureLR$nom_vern =="Nyctalus leisleri leisleri Kuhl 1817"| TableCaptureLR$nom_vern =="Nyctalus leislerii"| TableCaptureLR$nom_vern =="NYCLEI"| TableCaptureLR$nom_vern =="Nyclei"| TableCaptureLR$nom_vern =="Noctule de Leisler"| TableCaptureLR$nom_vern =="Nyctalus leisleri leisleri"| TableCaptureLR$nom_vern =="Nyctalus leisleri leisleri "]<- "Nyctalus leisleri"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Nyctalus noctula Schreber 1774"| TableCaptureLR$nom_vern =="NYCNOC"| TableCaptureLR$nom_vern =="Nyctalus noctula "| TableCaptureLR$nom_vern =="Noctule commune"]<- "Nyctalus noctula"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "PIP KUH"| TableCaptureLR$nom_vern =="Pipistrelle kuhlii"| TableCaptureLR$nom_vern =="pipistrellus kuhlii"| TableCaptureLR$nom_vern =="Pipistrellus kuhlii "| TableCaptureLR$nom_vern =="PIPKHU"| TableCaptureLR$nom_vern =="Pipkuh"| TableCaptureLR$nom_vern =="PIPKUH"| TableCaptureLR$nom_vern =="Pipistrellus kuhlii Kuhl 1817"| TableCaptureLR$nom_vern =="Pipistrellus kuhli Kuhl 1817"| TableCaptureLR$nom_vern =="Pipistrelle de Kuhl"| TableCaptureLR$nom_vern =="Pipistrel kuhli"]<- "Pipistrellus kuhlii"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipnat"|TableCaptureLR$nom_vern == "PIP NAT"| TableCaptureLR$nom_vern =="Pipistrelle de Nathusius"| TableCaptureLR$nom_vern =="Pipistrellus nathusii "| TableCaptureLR$nom_vern =="Pipistrellus nathusii Keyserling  Blasius 1839"| TableCaptureLR$nom_vern =="PIPNAT"| TableCaptureLR$nom_vern =="PIPNAT "]<- "Pipistrellus nathusii"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrel pipistrellus"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus "| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus pipistrellus"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus pipistrellus "| TableCaptureLR$nom_vern =="Pipistrelle commune"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus pipistrellus"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus Schreber 1774"| TableCaptureLR$nom_vern =="PIPPIP"| TableCaptureLR$nom_vern =="PiPPIP"| TableCaptureLR$nom_vern =="Pippip"]<- "Pipistrellus pipistrellus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrel pygmaeus"| TableCaptureLR$nom_vern =="Pipistrelle pygmaeus"| TableCaptureLR$nom_vern =="Pipistrellus pygmaeus "| TableCaptureLR$nom_vern =="PIPPYG"| TableCaptureLR$nom_vern =="Pipistrellus cf pygameus"| TableCaptureLR$nom_vern =="Pipistrelle pygmée"]<- "Pipistrellus pygmaeus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrellus specie"|  TableCaptureLR$nom_vern =="Pipistrellus "| TableCaptureLR$nom_vern =="Pipistrelle sp"| TableCaptureLR$nom_vern =="Pipistrellus nathusius ?"| TableCaptureLR$nom_vern =="Pipistrel sp"| TableCaptureLR$nom_vern =="PIPIND"| TableCaptureLR$nom_vern =="Pipistrellus species"| TableCaptureLR$nom_vern =="Pipistrellus sp"]<- "Pipistrellus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrellus kuhlii_Pipistrellus nathusii"|TableCaptureLR$nom_vern == "Pipistrelle de Kuhl/Nathusius"| TableCaptureLR$nom_vern =="Pipkuh-Pipnat"]<- "Pipistrellus kuhlii_Pipistrellus nathusii"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrellus pygmaeus Leach 1825 Pipistrellus nathusii Keyserling  Blasius 1839"|TableCaptureLR$nom_vern == "Pipistrellus nathusii_Pipistrellus pygmaeus"|TableCaptureLR$nom_vern == "Pipistrellus nathusii Keyserling  Blasius 1839 Pipistrellus pygmaeus Leach 1825"]<- "Pipistrellus nathusii_Pipistrellus pygmaeus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrelle commune, Pipistrelle pygmée"|TableCaptureLR$nom_vern == "Pipistrellus pipistrellus Schreber 1774 Pipistrellus pygmaeus Leach 1825"|TableCaptureLR$nom_vern == "PIPPIPPYG"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus / pygmaeus"| TableCaptureLR$nom_vern =="Pipistrellus pipistrellus Schreber, 1774, Pipistrellus pygmaeus Leach, 1825"]<- "Pipistrellus pygmaeus_Pipistrellus pipistrellus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Pipistrellus pipistrellus_Pipistrellus kuhlii"|TableCaptureLR$nom_vern == "Pipistrellus pipistrellus Schreber 1774 Pipistrellus kuhli Kuhl 1817"]<- "Pipistrellus pipistrellus_Pipistrellus kuhlii"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "VESMUR"| TableCaptureLR$nom_vern =="Sérotine bicolore"| TableCaptureLR$nom_vern =="Vespertilio murinus Linnaeus 1758"]<- "Vespertilio murinus"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Tadarida teniotis Rafinesque 1814"| TableCaptureLR$nom_vern =="Molosse de Cestoni"| TableCaptureLR$nom_vern =="TADTEN"]<- "Tadarida teniotis"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "RAS"| TableCaptureLR$nom_vern =="Chiropteres : Aucune observation"| TableCaptureLR$nom_vern =="Aucune espèce"| TableCaptureLR$nom_vern =="Aucune chauve-souris"]<- "BREDOUILLE"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "NYCIND"]<- "Nyctalus sp"
TableCaptureLR$nom_vern[TableCaptureLR$nom_vern == "Minioptère de Schreibers"]<- "Miniopterus schreibersii"

names(TableCaptureLR)[names(TableCaptureLR) == "nom_vern"] = "TAXON"
names(TableCaptureLR)[names(TableCaptureLR) == "date_obs"] = "DATE"
names(TableCaptureLR)[names(TableCaptureLR) == "code_insee"] = "INSEE_COM"
names(TableCaptureLR)[names(TableCaptureLR) == "gîte"] = "SpGite"
TableCaptureLR$SpGite = ifelse(TableCaptureLR$SpGite == '"gîte non précisé" ou "hors gîte"', 0, 1)

TableCaptureLR$DATE = format(TableCaptureLR$DATE, "%d/%m/%Y")

TableCaptureLR$INSEE_COM = as.character(TableCaptureLR$INSEE_COM)
TableCaptureLR$DATE = as.character(TableCaptureLR$DATE)

TableCapture = bind_rows(TableCapture0, TableCaptureLR)

# Add species short name
Match1 = match(TableCapture$TAXON, SpeciesList$'Scientific name')
TableCapture$Sp = SpeciesList$Esp[Match1]

TableCapture=TableCapture[!is.na(TableCapture$Sp),]

# Remove all Pipistrellus pipistrellus and pygmaeus before 2005 (both considered as same species before 2005)
# Remove also Miniopterus schreibersii before 2005 to avoid calculating false proportions in this group
TableCapture = TableCapture %>% 
  filter( !(as.Date(DATE, format = "%d/%m/%Y") < as.Date("2005-01-01") & 
              TAXON=="Pipistrellus pipistrellus") &
            !(as.Date(DATE, format = "%d/%m/%Y") < as.Date("2005-01-01") & 
                TAXON=="Pipistrellus pygmaeus") &
            !(as.Date(DATE, format = "%d/%m/%Y") < as.Date("2005-01-01") & 
                TAXON=="Miniopterus schreibersii"))

# Load delimitation of municipal corporations (recent version)
Limit_municipal_new = read_sf(args[4]) %>% 
  select("ID", "NOM_COM", "NOM_COM_M", "INSEE_COM", "TYPE", "geometry")

# Load delimitation of municipal corporations (old version)
Limit_municipal_old = read_sf(args[7]) %>% 
  select("ID", "NOM_COM", "NOM_COM_M", "INSEE_COM", "TYPE", "geometry")

# Merge
Limit_municipal = rbind(Limit_municipal_new, Limit_municipal_old)

# Crop municipal corporations that are in the capture data
Limit_municipal_capture = Limit_municipal %>% 
  filter(INSEE_COM %in% TableCapture$INSEE_COM)

# Which municipal corporations do not exist in the GIS tables?
`%nin%` = Negate(`%in%`)
TEST = TableCapture %>%
  filter (INSEE_COM %nin% Limit_municipal$INSEE_COM &
            INSEE_COM %nin% Limit_municipal_old$INSEE_COM) %>%
  as.data.frame()
unique(TEST$COMMUNE)

#### Load grid where predictions can be made #### 
CoordSIG=fread(paste0(args[5])) # Load environmental variables
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpAltiS)==F)
CoordSIG=subset(CoordSIG,is.na(CoordSIG$SpBioC1)==F)

# Add rotated coordinates
CoordDS=as.matrix(cbind(CoordSIG$X,CoordSIG$Y))
for (a in 0:(as.numeric(args[2])-1))
{
  Coordi=Rotation(CoordDS,angle=pi*a/as.numeric(args[2]))
  CoordSIG=cbind(CoordSIG,Coordi[,1])
  names(CoordSIG)[ncol(CoordSIG)]=paste0("SpCoord",a)
}

# Add Recorder type (here we use the most common one of the dataset for predictions)
CoordSIG$SpRecorder = "SM2BAT+"

# Make spatial feature
CoordSIG_sf = CoordSIG %>% 
  as.data.frame() %>% 
  st_as_sf(coords = c("X", "Y"), crs=4326, remove=FALSE) 

# Crop points that are within the municipal corporations
CoordSIG_capture = st_filter (CoordSIG_sf, Limit_municipal_capture)

# Cleans coordinates
CoordSIG_capture = CoordSIG_capture %>%
  rename_all(~str_replace_all(.,"\\.x","")) %>% 
  select(-contains(".y"))

# Add date of capture and taxon to municipal corporations limit
TableCapture_sf = merge(Limit_municipal_capture, TableCapture) %>% 
  select(INSEE_COM, NOM_COM, DATE, SpGite)

# /!\ /!\ /!\ THIS INFO SHOULD BE MADE AVAILABLE /!\ /!\ /!\ 
TableCapture_sf$SpGite=ifelse(is.na(TableCapture_sf$SpGite), 0, TableCapture_sf$SpGite)

# Merge environmental variables with date of capture
TableCapture_CoordSIG_sf = CoordSIG_capture %>% 
  st_join(TableCapture_sf, left=T)

TableCapture_CoordSIG = as.data.frame(TableCapture_CoordSIG_sf)
# 
# png(filename=paste("C:/Users/croemer01/Documents/Post-Doc/CACCHI/CACCHI_sampling.png",
#                    sep="_"),width=1000,height=1000,res=300)
# 
# plot1 = ggplot() + # Check
#   #geom_sf(data = Limit_municipal_capture, aes(alpha=0.5, col="red")) +
#   geom_sf(data=Europe_sf, fill="grey", alpha=0.3) +
#   geom_sf(data = TableCapture_CoordSIG_sf , size = 0.1, col="blue4", alpha=0.3) +
#   theme_void()
# print(plot1)
# dev.off()

# Circular date
SpFDate=yday(TableCapture_CoordSIG$DATE)
TableCapture_CoordSIG$SpCDate=cos(SpFDate/365*2*pi) # to create a circular variable for date
TableCapture_CoordSIG$SpSDate=sin(SpFDate/365*2*pi) # to create a circular variable for date

# Add Year
TableCapture_CoordSIG$SpYear = lubridate::year(as.Date(TableCapture_CoordSIG$DATE, format = "%d/%m/%Y"))

# Save capture data + environmental variables
fwrite(TableCapture_CoordSIG,paste0("C:/Users/croemer01/Documents/Donnees vigie-chiro/Capture_CoordSIG_500m.csv"))

# Save capture data
fwrite(TableCapture,paste0("C:/Users/croemer01/Documents/Post-Doc/CACCHI/Data/Capture.csv"))
