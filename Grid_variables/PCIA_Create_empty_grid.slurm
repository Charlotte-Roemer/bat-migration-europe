#!/bin/bash                                                                                                               

# Les lignes commançant par #SBATCH sont interprétées par SLURM.
# Elles permettent demander les ressources nécessaires au job.

# Nombre de processeur par noeud. si on a un script qui parallelise alors on peut augmenter le nombre de noeuds
#SBATCH --ntasks-per-node=10

# Nom du job
#SBATCH --job-name=empty_grid

# Quantité de RAM par noeud
#SBATCH --mem=100G

# Quel type de machine demander (type_1 ou type_2)
#SBATCH --partition=type_2
 
#SBATCH --time=3-00:00:00

# Chargement des modules : paquets de packages. si un package manque il faut demander à pcia
module load userspace/tr17.10
module load biology
module load gcc/7.2.0        
module load lapack/3.7.1
module load blas/gcc72/3.7.1
module load jags/gcc72/4.3.0
module load proj/8.0.0     
module load geos/3.11.2
module load gdal
module load udunits
module load R/4.3.1   

# Commandes et scripte à lancer
Rscript PCIA_Create_empty_grid.R