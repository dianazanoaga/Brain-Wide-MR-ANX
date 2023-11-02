#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=brain.imaging1
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=50g

module load miniconda
conda activate Renv
Rscript --vanilla /gpfs/ysm/home/mz575/multivariable_MR41.R