#!/bin/bash                                                                                                               

# Les lignes commançant par #SBATCH sont interprétées par SLURM.
# Elles permettent demander les ressources nécessaires au job.

# Nombre de Noeud
#SBATCH --nodes=1

# Nombre de processeur par noeud. si on a un script qui parallelise alors on peut augmenter le nombre de noeuds
#SBATCH --ntasks-per-node=1

# Nom du job
#SBATCH --job-name=Predict_Act

# Quantité de RAM par noeud
#SBATCH --mem=150G

# Quantité de RAM par CPU
#SBATCH --mem-per-cpu=150G

# Quel type de machine demander (type_1 ou type_2 ou high_mem)
#SBATCH --partition=high_mem
 
#SBATCH --time=6-00:00:00

# Chargement des modules : paquets de packages. si un package manque il faut demander à pcia
module load userspace/tr17.10
module load biology

module load userspace/tr17.10
module load biology
module load gcc/7.2.0        
module load lapack/3.7.1
module load blas/gcc72/3.7.1
module load jags/gcc72/4.3.0
module load proj/8.0.0     
module load geos
module load gdal
module load udunits
module load R/4.0.5    

# Commandes et scripte à lancer
Rscript PCIA_Predict_Act_V4.R
