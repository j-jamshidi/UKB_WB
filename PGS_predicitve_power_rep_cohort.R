#This code run the analysis and plot the results for prediction power of wellbeing prs in replication sample
#Load Prepare data-------
setwd("Z:/UKB/p_analysis/R/UKB")
nokin_wbnjrep <- read.csv("replication_cohort_nokinship_num.csv", header = TRUE)
nokin_wbnjrep <- as.data.frame(nokin_wbnjrep)
#deal with missing data&change the codes
nokin_wbnjrep[nokin_wbnjrep == -1] <- NA
nokin_wbnjrep[nokin_wbnjrep == -3] <- NA
nokin_wbnjrep[nokin_wbnjrep == -818] <- NA
nokin_wbnjrep[nokin_wbnjrep == -121] <- NA
nokin_wbnjrep[nokin_wbnjrep == -9] <- NA
nokin_wbnjrep[nokin_wbnjrep == " "] <- NA
nokin_wbnjrep$age_sec_sqrt <- (nokin_wbnjrep$age_sec)^2
#------------PRS prediction==========
#The following scripts calculate the incrimental R2 along with the 95% CI around the R2
#library for computing 95% confidence intervals
library(psychometric)
library(fmsb)
#linear models PRS prediction for selected variables======
prs <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(prs) <- c("variable", "estimate", "r2", "pval", "lci","uci")
for (i in c(372,377,375,374,376,373,64)) {
  fits <- summary(lm(nokin_wbnjrep[,i] ~ PRScs_wbnj + age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,nokin_wbnjrep))
  nfits <- summary(lm(nokin_wbnjrep[,i]	~  age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,nokin_wbnjrep))
  p_val <- fits$coefficients[2,4]
  estimate <- fits$coefficients[2,1]
  full.r2 <- fits$r.squared
  null.r2 <- nfits$r.squared
  lci <- CI.Rsq((full.r2 - null.r2 ), length(na.omit(nokin_wbnjrep[,i])), 15, level = 0.95)$LCL
  uci <- CI.Rsq((full.r2 - null.r2 ), length(na.omit(nokin_wbnjrep[,i])), 15, level = 0.95)$UCL
  prs[nrow(prs)+1,] <- c(colnames(nokin_wbnjrep) [i],estimate,(full.r2 - null.r2 ), p_val, lci, uci ) #save in a dataframe
  }
#PRS prediction for Binary outcomes using a logistic regression and Nagelkerke (incremental) R2
library(fmsb)
for (i in c(53,385,384)) {
  p_val <- summary(glm(nokin_wbnjrep[,i] ~ PRScs_wbnj + age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,family = "binomial", data=nokin_wbnjrep))$coefficients[2,4]
  estimate <- summary(glm(nokin_wbnjrep[,i] ~ PRScs_wbnj + age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,family = "binomial", data=nokin_wbnjrep))$coefficients[2,1]
  full.r2 <- NagelkerkeR2(glm(nokin_wbnjrep[,i] ~ PRScs_wbnj + age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,family = "binomial", data=nokin_wbnjrep))$R2
  null.r2 <- NagelkerkeR2(glm(nokin_wbnjrep[,i] ~ age_sec + age_sec_sqrt + sex +genotype_array + pc1+pc2+pc3+pc4+pc5+pc6+pc7+pc8+pc9+pc10,family = "binomial", data=nokin_wbnjrep))$R2
  lci <- CI.Rsq((full.r2 - null.r2 ), length(na.omit(nokin_wbnjrep[,i])), 15, level = 0.95)$LCL
  uci <- CI.Rsq((full.r2 - null.r2 ), length(na.omit(nokin_wbnjrep[,i])), 15, level = 0.95)$UCL
  prs[nrow(prs)+1,] <- c(colnames(nokin_wbnjrep) [i],estimate,(full.r2 - null.r2 ), p_val, lci, uci )}

#make results ready for the plot=====
prs$variable <- c("Wellbeing index",	"Happiness",	"Friendship satisfaction",	"Family satisfaction",	"Health satisfaction",
                  "Financial satisfaction",	"Neuroticism",	"Loneliness",	"Depressive Symptomes",	"Seen GP or Psychiatrist")
prs[2:6] <- as.numeric(unlist(prs[2:6]))

#Plot the results=======
{
  prs$variable <- factor(prs$variable , levels= prs$variable)
opar <- par(lwd = 3,mar = c(17, 6, 1, .1))
barCentres<- barplot(height=prs$r2*100, names=prs$variable, 
                     col="#b3e6e6", lwd=2,  cex.axis = 1.7, cex.names = 1.7,ylim = c(0,1.3), las=3, 
                     ylab = (expression('R'^2*'%'*' (95% CI)')), cex.lab=1.5)
arrows(barCentres, prs$uci*100, barCentres,
       prs$lci*100, lwd = 3, angle = 90,
       code = 3, length = 0.07, )
}
