library(data.table)
library(dismo)
library(raster)
library(gstat)
library(rgdal)
library(grid)

# Plots predicted activity on maps

# Lists arguments ####
if(length(args)<3)
{
  args=vector()
  args[6]="C:/Users/Yves Bas/Downloads/Pred_Myodau_Act_07_GI_SysGrid__3e+05" # To load prediction already made ?
  #args[7]="C:/Users/Yves Bas/Documents/VigieChiro/GIS/FranceD__30_34.shp"
  args[7]="France_dep_L93.shp" # To load France department limits (shape)
  args[8]=5000 #PixelSize (obsolete ?)
  ModRF_file="C:/Users/Yves Bas/Downloads/ModRFActLog_Eptser50.learner" # To load the classifier for a species ?
    load(ModRF_file)
    MaxSample=100 #pour tester sur un petit ?chantillon (pour faire tourner sur tout, doit etre sup?rieur, genre 1e12)
}

SpeciesList=fread("C:/Users/sarah/Desktop/Stage/Cartes/SpeciesList.csv") # Load species list
Rasteriz=F # To plot a raster instead of a vector
SaveErrors=F # To write a file with error log
VPlot=T # To plot distribution map
RPlot=F # To plot ...

Sys.time()
#Limite
Limite=shapefile(args[7]) # Load contour of French departments
Sys.time()

LimiteL=as(Limite,'SpatialLines') # convert as SpatialLines

Title=substr(basename(args[6]),1,12) # define Title name from args[6]

# To add a subtitle to the plot with the PseudoR2 if ModRF was run
SubT=""
if(exists("ModRF"))
{
  if(sum(grepl("rsq",names(ModRF)))>0)
  {
    SubT=paste0(SubT,"PseudoR2 = ",round(ModRF$rsq[length(ModRF$rsq)],2))
  }
  Num=sum(ModRF$y!=0)
  SubT=paste0(SubT," / N = ",Num)
}

# ???
PredLoc=fread(paste0(args[6],".csv"))
Sys.time()
PredLoc=PredLoc[1:(min(nrow(PredLoc),MaxSample)),]

# Transform PredLoc as a spatial object and reproject as WGS84
coordinates(PredLoc) <- c("Group.1", "Group.2") # Group.1 and Group.2 become the coordinates
proj4string(PredLoc) <- CRS("+init=epsg:4326") # WGS 84

# Make another layer that is PredLoc in L93
CRSL93 <- CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +units=m +no_defs")
PredL93=spTransform(PredLoc,CRSL93)
v <- voronoi(PredL93) # make voronoi polygons
#plot(v)

# Crop the voronoi polygons by the contour of the French departments
VL=crop(v,Limite)

# To scale the color gradient ?
MaxScale=quantile(subset(PredL93$pred,PredL93$pred>0.1),0.95)
if(is.na(MaxScale)){MaxScale=0.1}
ScaleAt=c(-0.1,c(1:49)/49*MaxScale,Inf)

# Plot distribution ####
if((VPlot))
{
  Taxon=substr(basename(args[6]),6,11)
  Taxon=gsub("_","",Taxon)
  test=match(Taxon,SpeciesList$Esp)
  if(is.na(test))
  {
    Title=Taxon
  }else{
    Title=SpeciesList$NomFR[test]
  }
  # print(spplot(VL, 'pred',main=Title,col="transparent"
  #           ,par.settings =
  #            list(axis.line = list(col =  'transparent'))
  #         ,col.regions=get_col_regions(),at=ScaleAt))
  
  png(filename=paste0(args[6],".png"), width = 3000, height = 2500, res=300)
  #tiff(paste0(args[6],".tif"), compression = "lzw")
 #jpeg(paste0(args[6],".jpg"))
 #writeOGR(VL, dirname(args[6]), basename(args[6]), driver="ESRI Shapefile")
  
  
spplot(VL, 'pred',main="Nyctalus leisleri",col="transparent"
           ,par.settings =
            list(axis.line = list(col =  'transparent'))
          ,col.regions=get_col_regions(),at=ScaleAt,sp.layout = LimiteL
         ,xlab=SubT)

grid.text("Relative Activity", x=unit(0.95, "npc"), y=unit(0.50, "npc"), rot=-90)

dev.off()
  
}
Sys.time()
if(Rasteriz)
{
  r <- raster(Limite, res=as.numeric(args[8]))
  vpred <- rasterize(VL, r, 'pred')
  
  #gs <- gstat(formula=PredL93$pred~1, locations=PredL93, nmax=10, set=list(idp = 0))
  #nn <- interpolate(r, gs)
  ## [inverse distance weighted interpolation]
  #nnmsk <- mask(nn, vpred)
  #plot(nnmsk,main=substr(args[6],22,27))
  
  #spplot(VL, 'err', col.regions=get_col_regions())
  
  
  if(VPlot){spplot(vpred,main=substr(args[6],22,27),at=ScaleAt)}
  
  writeRaster(vpred,paste0(args[6],"_pred.asc"),overwrite=T)
  
  if(RPlot)
  {
    png(paste0(args[6],"_R.png"))
    print(spplot(vpred,main=args[1],at=ScaleAt,sp.layout = LimiteL
                 ,xlab=SubT))
    dev.off()
  }
  
  
  if(SaveErrors)
  {
    verr <- rasterize(VL, r, 'err')
    #plot(verr)
    writeRaster(verr,paste0(args[6],"_err.asc"),overwrite=T)
  }
  
}
Sys.time()

