#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=mrpresso
#SBATCH --time=72:00:00
#SBATCH --mem=100g
#SBATCH --partition=scavenge

module load miniconda
conda activate Renv

Rscript --vanilla /home/mz575/multivariable_MR41_mvp_harm.R