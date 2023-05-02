library(data.table)
library(StreamMetabolism)

Table_full = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/export/DataLP_PF_export_560.csv")
Table_weighted1 = fread("C:/Users/croemer01/Documents/Donnees vigie-chiro/export/weighted_DataLP_PF_export_560.csv")


# Resample the dataset with random selection and weight on probability
index<-sample(1:nrow(Table_full), nrow(Table_full), prob = Table_full$probabilite, replace = T)
DataFiable = Table_full[index,]


Table_weighted_sub = subset(DataFiable, DataFiable$espece=="Nyclei")
Table_weighted2_sub = subset(Table_weighted1, Table_weighted1$espece=="Nyclei")
Table_full_sub = subset(Table_full, Table_full$espece=="Nyclei")

hist(Table_weighted_sub$probabilite)
hist(Table_weighted2_sub$probabilite)
hist(Table_full_sub$probabilite)


