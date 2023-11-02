#!/bin/bash    
#SBATCH --mail-type=ALL  
#SBATCH --mail-user=mihaela-diana.zanoaga@yale.edu 
#SBATCH --job-name=brain.imaging.gen1
#SBATCH --time=24:00:00
#SBATCH --mail-type=ALL
#SBATCH --mem=5g
module load miniconda
conda activate ldsc
cd /gpfs/gibbs/pi/polimanti/diana/brain_analysis/output
files=(0039.proc.txt.out.sumstats.gz 0705.proc.txt.out.sumstats.gz 0715.proc.txt.out.sumstats.gz 0772.proc.txt.out.sumstats.gz 0800.proc.txt.out.sumstats.gz 0802.proc.txt.out.sumstats.gz 0862.proc.txt.out.sumstats.gz 0952.proc.txt.out.sumstats.gz 0974.proc.txt.out.sumstats.gz 1444.proc.txt.out.sumstats.gz 3190.proc.txt.out.sumstats.gz 3386.proc.txt.out.sumstats.gz 3446.proc.txt.out.sumstats.gz) 
for file1 in "${files[@]}"; do
  for file2 in "${files[@]}"; do
    if [ "$file1" != "$file2" ]; then
       python /gpfs/ysm/project/polimanti/mz575/ldsc/ldsc.py --rg /gpfs/gibbs/pi/polimanti/diana/brain_analysis/output/$file1,/gpfs/gibbs/pi/polimanti/diana/brain_analysis/output/$file2 --ref-ld-chr /gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr/ --w-ld-chr /gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr/ --out /gpfs/gibbs/pi/polimanti/diana/corr_analysis/brain_brain/$file1$file2
    fi
  done
done