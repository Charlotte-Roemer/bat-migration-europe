
library(data.table)
library(tidyverse)
library(dplyr)
library(ggridges)
#library(rowr)

# Read systematic Habitat sampling on a grid of 500 m precision
Table_SysGrid = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/SysGrid_500m_de_cote_FULL.csv")
Table_SysGrid=as.data.frame(Table_SysGrid)

# Read Habitat values on Vigie-Chiro participations
Table_VC2 = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/GI_FR_sites_localites.csv")

# Left-join to add missing columns
Table_VC3=left_join(Table_VC2, Table_SysGrid[0,])

# List variables
testPred=(substr(names(Table_VC3),1,2)=="Sp")
List_variables=names(Table_VC3)[testPred]
List_variables = subset(List_variables, 
                        List_variables != "SpHC999M" & List_variables != "SpHC99M" 
                        & List_variables != "SpHC9M" & List_variables != "SpHC241L")
print(List_variables)

# Tables for density stacked plots
Table_SysGrid$Data = "Systematic_Grid"
Table_VC3$Data = "Vigie-Chiro"

# Rbind Tables for density stacked plots
Table_Total = bind_rows(Table_SysGrid, Table_VC3)

# Stacked density plots
for (i in 1:length(List_variables)){
  Table_i = Table_Total[,c(List_variables[i], "Data")]
  names(Table_i)[which(names(Table_i)==List_variables[i])]="x"
  Q1 = quantile(Table_i$x, probs = 0.025, na.rm = T)
  Q3 = quantile(Table_i$x, probs = 0.8, na.rm = T)
  png(filename=paste("C:/Users/croemer01/Documents/Donnees vigie-chiro/Sampling_Bias/Zoom Q2.5-Q80", 
                     List_variables[i],".png",sep=""), width=2500, height=1500, res=300)
  
  plot2 = ggplot(Table_i, aes(x = x, y = Data)) +
    geom_density_ridges(aes(fill = Data, alpha = 0.5)) + 
    xlim(c(Q1,Q3)) +
    labs(x = "Value of variable",
         y = "") +
    theme(legend.position = "none") +
    ggtitle(List_variables[i])
  
  print(plot2)
  dev.off()
}


# # Calculate quantiles for each variable and plot difference between sysGrid and Vigie-Chiro
# Quantiles_TOT = data.frame()
# for (i in 1:length(List_variables)){
#   
#   print(List_variables[i])
#   
#   # Quantiles for the systematic Grid
#   Quantiles_SysGrid = apply(as.data.frame(Table_SysGrid %>% select(List_variables[i])) , 
#                             2 , 
#                             quantile , 
#                             probs = c(2.5, 50, 97.5)/100 , 
#                             na.rm = TRUE )
#   
#   Quantiles_SysGrid2=as.data.frame(t(Quantiles_SysGrid))
#   Quantiles_SysGrid2=setDT(Quantiles_SysGrid2, keep.rownames = TRUE)[]
#   colnames(Quantiles_SysGrid2)=c("Variable", "Q2.5", "Q50", "Q97.5")
#   Quantiles_SysGrid2$Data = "Systematic-Grid"
#   
#   # Quantiles for each Vigie-Chiro Protocol
#   Quantiles_VC = apply(as.data.frame(Table_VC3 %>% select(List_variables[i])) , 
#                        2 , 
#                        quantile , 
#                        probs = c(2.5, 50, 97.5)/100 , 
#                        na.rm = TRUE )
#   
#   Quantiles_VC2=as.data.frame(t(Quantiles_VC))
#   Quantiles_VC2=setDT(Quantiles_VC2, keep.rownames = TRUE)[]
#   colnames(Quantiles_VC2)=c("Variable", "Q2.5", "Q50", "Q97.5")
#   Quantiles_VC2$Data = "Vigie-Chiro"
#   
#   Quantiles_TOT = rbind(Quantiles_TOT, Quantiles_SysGrid2, Quantiles_VC2)
# }



# # Create Plot
# for (i in 1:length(List_variables)){
#   Quantiles_TOT_i = subset(Quantiles_TOT, Quantiles_TOT$Variable==List_variables[i])
#   
#   png(filename=paste("C:/Users/croemer01/Documents/Donnees vigie-chiro/Sampling_Bias/",
#                      List_variables[i],".png",sep=""), width=2500, height=1500, res=300)
#   
#   plot1 = ggplot(Quantiles_TOT_i, aes(x=factor(Data), y=Q50)) +
#     geom_bar(stat="identity",aes(fill=Variable)) +
#     geom_errorbar(aes(ymin= Q2.5,
#                       ymax= Q97.5),
#                   width=.2,
#                   position="identity") +
#     labs(x = "",
#          y = "Value of variable") +
#     #scale_y_log10()+
#     ggtitle(names(List_variables[i])) +
#     theme_bw(base_size = 12) +
#     guides(fill=guide_legend(ncol=3,bycol=TRUE))
#   
#   print(plot1)
#   dev.off()
# }





