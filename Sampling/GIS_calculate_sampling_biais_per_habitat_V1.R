
library(data.table)
library(tidyverse)
library(dplyr)

# Read systematic Habitat sampling on a grid of 300,000 points
Table_SysGrid = fread("D:/SIG/SIG_Post-Doc_MNHN/Extraction_grille_systematique_100000_points/GI_SysGrid__3e+05.csv")
Table_SysGrid=as.data.frame(Table_SysGrid)

# Read Habitat values on Vigie-Chiro participations
Table_VC = fread("D:/SIG/SIG_Post-Doc_MNHN/GI_coordWGS84_SpNuit2_50_DataLP_PF_exportTot.csv")

# Link with type of protocol
Type_protocol = fread("D:/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Donnees vigie-chiro/GI_sites_localites.csv")
Type_protocol=as.data.frame(Type_protocol[,c("longitude", "latitude", "protocole.x")])

# Keep only fix points
Type_protocol=subset(Type_protocol, Type_protocol$protocole.x == "POINT_FIXE")

# Merge
Table_VC2=left_join(Table_VC, Type_protocol, by=c("Group.1" = "longitude", "Group.2" = "latitude") )
# Some categories are present in Table_SysGrid but missing in the Table_VC2
# Add missing categories
Table_VC3=left_join(Table_VC2, Table_SysGrid[0,])
Table_VC3[,213:292]=0

# List categorical variables
{
  List_variables=vector(mode = "list", length = length(c(27:28, 159:161, 258:275)))
  
  # For Variables_ALAN, Variables_Alti and Variables_ROUTE500 (numeric) :
  j=1
  for ( i in c(27:28, 159:161, 258:275)){
    names(List_variables)[j]=names(Table_SysGrid)[i]
    j=j+1
  }
  # List_variables$Variables_ALAN=names(Table_SysGrid[,27:28])
  # List_variables$Variables_Alti=names(Table_SysGrid[,159:161])
  # List_variables$Variables_ROUTE500=names(Table_SysGrid[,258:275])
  
  # For all other variables (categorical) :
  List_variables$Variables_BIOCLIM=names(Table_SysGrid[,8:26])
  #List_variables$Variables_CLC_500=names(Table_SysGrid[,29:87])
  #List_variables$Variables_CLC_500_Main=names(Table_SysGrid[,89:93])
  List_variables$Variables_CLC_500_Main=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),6,6)=="M" &
                                                                         substring(names(Table_SysGrid),1,4)=="SpHC" ))
  List_variables$Variables_CLC_500_Secondary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),7,7)=="M" &
                                                                              substring(names(Table_SysGrid),1,4)=="SpHC"))
  List_variables$Variables_CLC_500_Tertiary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),8,8)=="M" &
                                                                             substring(names(Table_SysGrid),1,4)=="SpHC"  ))
  List_variables$Variables_CLC_5000_Main=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),6,6)=="L" &
                                                                          substring(names(Table_SysGrid),1,4)=="SpHC" ))
  List_variables$Variables_CLC_5000_Secondary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),7,7)=="L" &
                                                                               substring(names(Table_SysGrid),1,4)=="SpHC" ))
  List_variables$Variables_CLC_5000_Tertiary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),8,8)=="L" &
                                                                              substring(names(Table_SysGrid),1,4)=="SpHC" ))
  #Variables_CLC_5000=names(Table_SysGrid[,88:158])
  #List_variables$Variables_Slope=names(Table_SysGrid[,162:173])
  #List_variables$Variables_WindTurbine=names(Table_SysGrid[,174:176])
  #List_variables$Variables_CARTHAGE=names(Table_SysGrid[,177:209])
  List_variables$Variables_TheiaOSO_50=names(Table_SysGrid[,210:231])
  List_variables$Variables_TheiaOSO_500=names(Table_SysGrid[,232:257])
  
}


# Calculate quantiles for each category and plot difference between sysGrid and Vigie-Chiro
for (i in 1:length(List_variables)){
  
  if(!is.null(List_variables[[i]])){
    Quantiles_SysGrid = apply( as.data.frame(Table_SysGrid %>% select(List_variables[[i]])) , 
                               2 , 
                               quantile , 
                               probs = c(2.5, 50, 97.5)/100 , 
                               na.rm = TRUE )
  }else{
    Quantiles_SysGrid = apply( as.data.frame(Table_SysGrid %>% select(names(List_variables[i]))) ,
                               2 ,
                               quantile ,
                               probs = c(2.5, 50, 97.5)/100 ,
                               na.rm = TRUE )
  }
  Quantiles_SysGrid2=as.data.frame(t(Quantiles_SysGrid))
  Quantiles_SysGrid2=setDT(Quantiles_SysGrid2, keep.rownames = TRUE)[]
  colnames(Quantiles_SysGrid2)=c("Variable", "Q2.5", "Q50", "Q97.5")
  Quantiles_SysGrid2$Data = "Systematic-Grid"
  
  if(!is.null(List_variables[[i]])){
    Quantiles_VC = apply( as.data.frame(Table_VC3 %>% select(List_variables[[i]])) , 
                          2 , 
                          quantile , 
                          probs = c(2.5, 50, 97.5)/100 , 
                          na.rm = TRUE )
  }else{
    Quantiles_VC = apply( as.data.frame(Table_VC3 %>% select(names(List_variables[i]))) , 
                          2 , 
                          quantile , 
                          probs = c(2.5, 50, 97.5)/100 , 
                          na.rm = TRUE )
  }
  Quantiles_VC2=as.data.frame(t(Quantiles_VC))
  Quantiles_VC2=setDT(Quantiles_VC2, keep.rownames = TRUE)[]
  colnames(Quantiles_VC2)=c("Variable", "Q2.5", "Q50", "Q97.5")
  Quantiles_VC2$Data = "Vigie-Chiro"
  
  Quantiles_RBIND = rbind(Quantiles_SysGrid2, Quantiles_VC2)
  
  png(filename=paste("D:/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Donnees vigie-chiro/Graphs_biais_echantillonnage/", 
                     names(List_variables[i]),"_biais_echantillonnage",".png",sep=""), width=2000, height=1500, res=300)
  
  # For categorical variables
  if(!is.null(List_variables[[i]])){
  plot1=ggplot(Quantiles_RBIND, aes(x=factor(Data), y=Q50)) +
    geom_bar(stat="identity",aes(fill=Variable)) +
    labs(x = "",
         y = "Proportion of variable") +
    ggtitle(names(List_variables[i])) +
    theme_bw(base_size = 12) +
    guides(fill=guide_legend(ncol=3,bycol=TRUE)) 
  
  # For numerical variables
  }else{
    plot1=ggplot(Quantiles_RBIND, aes(x=factor(Data), y=Q50+1)) +
      geom_bar(stat="identity",aes(fill=Variable)) +
      geom_errorbar(aes(ymin= Q2.5+1, 
                        ymax= Q97.5+1),
                    width=.2,            
                    position="identity") +
      labs(x = "",
           y = "Value of variable + 1") +
      scale_y_log10()+
      ggtitle(names(List_variables[i])) +
      theme_bw(base_size = 12) +
      guides(fill=guide_legend(ncol=3,bycol=TRUE)) 
  }
  
  print(plot1)
  dev.off()
  
}




