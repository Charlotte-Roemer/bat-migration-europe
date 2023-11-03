
library(data.table)
library(tidyverse)
library(dplyr)
library(ggridges)
library(rowr)

# Read systematic Habitat sampling on a grid of 300,000 points
Table_SysGrid = fread("D:/SIG/SIG_Post-Doc_MNHN/Extraction_grille_systematique_100000_points/GI_SysGrid__3e+05.csv")
Table_SysGrid=as.data.frame(Table_SysGrid)

# Read Habitat values on Vigie-Chiro participations
Table_VC2 = fread("D:/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Donnees vigie-chiro/GI_sites_localites.csv")

# Left-join to add missing columns
Table_VC3=left_join(Table_VC2, Table_SysGrid[0,])

# List variables
{
  List_variables=vector(mode = "list", length = length(c(8:26, 27:158, 159:161, 174:176, 210:257, 258:275)))
  
  j=1
  for ( i in c(8:26, 27:158, 159:161, 174:176, 210:257, 258:275)){
    names(List_variables)[j]=names(Table_SysGrid)[i]
    j=j+1
  }
  # List_variables$Variables_ALAN=names(Table_SysGrid[,27:28])
  # List_variables$Variables_Alti=names(Table_SysGrid[,159:161])
  # List_variables$Variables_ROUTE500=names(Table_SysGrid[,258:275])
  # List_variables$Variables_BIOCLIM=names(Table_SysGrid[,8:26])
  # List_variables$Variables_Slope=names(Table_SysGrid[,162:173])
  # List_variables$Variables_WindTurbine=names(Table_SysGrid[,174:176])
  # List_variables$Variables_CARTHAGE=names(Table_SysGrid[,177:209])
  
  # List_variables$Variables_CLC_500_Main=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),6,6)=="M" &
  #                                                                        substring(names(Table_SysGrid),1,4)=="SpHC" ))
  # List_variables$Variables_CLC_500_Secondary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),7,7)=="M" &
  #                                                                             substring(names(Table_SysGrid),1,4)=="SpHC"))
  # List_variables$Variables_CLC_500_Tertiary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),8,8)=="M" &
  #                                                                            substring(names(Table_SysGrid),1,4)=="SpHC"  ))
  # List_variables$Variables_CLC_5000_Main=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),6,6)=="L" &
  #                                                                         substring(names(Table_SysGrid),1,4)=="SpHC" ))
  # List_variables$Variables_CLC_5000_Secondary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),7,7)=="L" &
  #                                                                              substring(names(Table_SysGrid),1,4)=="SpHC" ))
  # List_variables$Variables_CLC_5000_Tertiary=subset(names(Table_SysGrid), ( substring(names(Table_SysGrid),8,8)=="L" &
  #                                                                             substring(names(Table_SysGrid),1,4)=="SpHC" ))
  
  # List_variables$Variables_TheiaOSO_50=names(Table_SysGrid[,210:231])
  # List_variables$Variables_TheiaOSO_500=names(Table_SysGrid[,232:257])
  
}


# Calculate quantiles for each variable and plot difference between sysGrid and Vigie-Chiro for each Protocol

List_Protocols=table(Table_VC3$protocole.x)
for (i in 1:length(List_variables)){
  
  print(names(List_variables[i]))
  
  # Quantiles for the systematic Grid
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
  
  # Quantiles for each Vigie-Chiro Protocol
  rm(Quantiles_RBIND_VC)
  rm(Table_VC_sub_RBIND)
  for (j in 1:length(List_Protocols)){
    
    Table_VC3sub=subset(Table_VC3, Table_VC3$protocole.x==names(List_Protocols[j]))
    
    if(!is.null(List_variables[[i]])){
      Quantiles_VC = apply( as.data.frame(Table_VC3sub %>% select(List_variables[[i]])) , 
                            2 , 
                            quantile , 
                            probs = c(2.5, 50, 97.5)/100 , 
                            na.rm = TRUE )
    }else{
      Quantiles_VC = apply( as.data.frame(Table_VC3sub %>% select(names(List_variables[i]))) , 
                            2 , 
                            quantile , 
                            probs = c(2.5, 50, 97.5)/100 , 
                            na.rm = TRUE )
    }
    Quantiles_VC2=as.data.frame(t(Quantiles_VC))
    Quantiles_VC2=setDT(Quantiles_VC2, keep.rownames = TRUE)[]
    colnames(Quantiles_VC2)=c("Variable", "Q2.5", "Q50", "Q97.5")
    Quantiles_VC2$Data = names(List_Protocols[j])
    
    if (exists("Quantiles_RBIND_VC")){
      Quantiles_RBIND_VC = rbind(Quantiles_RBIND_VC, Quantiles_VC2)
    }else{
      Quantiles_RBIND_VC = Quantiles_VC2
    }
  }
  
  # Rbind Systematic Grid and Vigie-Chiro Protocols
  Quantiles_RBIND = rbind(Quantiles_SysGrid2, Quantiles_RBIND_VC)
  
  # Tables for density stacked plots
  Table_SysGrid_sub = as.data.frame(Table_SysGrid %>% select(names(List_variables[i])))
  Table_SysGrid_sub$Data = "Systematic_Grid"
  Table_VC_PointFixe_sub = as.data.frame(Table_VC3 %>% select(names(List_variables[i]))
                                         %>% subset(Table_VC3$protocole.x == "POINT_FIXE"))
  Table_VC_PointFixe_sub$Data = "POINT_FIXE"
  Table_VC_Pedestre_sub = as.data.frame(Table_VC3 %>% select(names(List_variables[i]))
                                         %>% subset(Table_VC3$protocole.x == "CARRE"))
  Table_VC_Pedestre_sub$Data = "PEDESTRE"
  Table_VC_Routier_sub = as.data.frame(Table_VC3 %>% select(names(List_variables[i]))
                                        %>% subset(Table_VC3$protocole.x == "ROUTIER"))
  Table_VC_Routier_sub$Data = "ROUTIER"
  
  # Rbind Tables for density stacked plots
  Table_Total = rbind(Table_SysGrid_sub, Table_VC_PointFixe_sub, Table_VC_Pedestre_sub, Table_VC_Routier_sub)
  
  # Create Plot
  # png(filename=paste("D:/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Donnees vigie-chiro/Graphs_biais_echantillonnage/", 
  #                    names(List_variables[i]),"_biais_echantillonnage",".png",sep=""), width=2500, height=1500, res=300)
  # 
  # # For categorical variables
  # if(!is.null(List_variables[[i]])){
  #   plot1 = ggplot(Quantiles_RBIND, aes(x=factor(Data), y=Q50)) +
  #     geom_bar(stat="identity",aes(fill=Variable)) +
  #     labs(x = "",
  #          y = "Proportion of variable") +
  #     ggtitle(names(List_variables[i])) +
  #     theme_bw(base_size = 12) +
  #     guides(fill=guide_legend(ncol=3,bycol=TRUE)) 
  #   
  #   # For numerical variables
  # }else{
  #   plot1 = ggplot(Quantiles_RBIND, aes(x=factor(Data), y=Q50)) +
  #     geom_bar(stat="identity",aes(fill=Variable)) +
  #     geom_errorbar(aes(ymin= Q2.5, 
  #                       ymax= Q97.5),
  #                   width=.2,            
  #                   position="identity") +
  #     labs(x = "",
  #          y = "Value of variable") +
  #     #      scale_y_log10()+
  #     ggtitle(names(List_variables[i])) +
  #     theme_bw(base_size = 12) +
  #     guides(fill=guide_legend(ncol=3,bycol=TRUE)) 
  # }
  # 
  # print(plot1)
  # dev.off()
  
  # Stacked density plots
  png(filename=paste("D:/Post-Doc/Vigie-Chiro et Indicateurs_ecologiques/Donnees vigie-chiro/Graphs_biais_echantillonnage/", 
                     names(List_variables[i]),"_Stacked-density_biais_echantillonnage",".png",sep=""), width=2500, height=1500, res=300)
  
  plot2 = ggplot(Table_Total, aes(x = Table_Total[,1], y = Data)) +
    geom_density_ridges(aes(fill = Data, alpha = 0.5)) + 
    labs(x = "Value of variable",
         y = "") +
    theme(legend.position = "none") +
    ggtitle(names(List_variables[i]))
  
  print(plot2)
  dev.off()
  
  
}




