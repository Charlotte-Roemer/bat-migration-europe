#!/bin/bash     

#SBATCH --job-name=SpNuit    # Nom du job

#SBATCH --partition=htc               # Choix de partition (htc par défaut)

#SBATCH --cpus-per-task=1             # Utiliser un seul CPU pour cette tâche (job)
#SBATCH --mem=50G                    # Mémoire
#SBATCH --time=1-00:00:00             # Délai max = 7 jours

#SBATCH --mail-user=<charlotte.roemer1@mnhn.fr>          # Où envoyer l'e-mail
#SBATCH --mail-type=END,FAIL          # Événements déclencheurs (NONE, BEGIN, END, FAIL, ALL)

#SBATCH --licenses=sps      

#module load R
source ~/.bashrc
conda activate r-spatial

Rscript SpNuitTot_BMRE.R 


