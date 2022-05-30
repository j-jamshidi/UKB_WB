#load data and prepare data----
setwd("Z:/UKB/p_analysis/R/UKB")
wbnj <- read.csv("replication_cohort_nokinship.csv", header = TRUE)
wbnj <- as.data.frame(wbnj)
#deal with missing data&change the codes
wbnj[wbnj == "Do not know"] <- NA
wbnj[wbnj == "do not know"] <- NA
wbnj[wbnj == "prefer not to say"] <- NA
wbnj[wbnj == "Prefer not to answer"] <- NA
wbnj[wbnj == "prefer not to answer"] <- NA
wbnj[wbnj == " "] <- NA
wbnj[wbnj =="not at all"] <- 1
wbnj[wbnj =="a little"] <- 2
wbnj[wbnj =="a moderate amount"] <- 3
wbnj[wbnj =="very much"] <- 4
wbnj[wbnj =="an extreme amount"] <- 5

##fit a model===== 
#model fit indies
#RMSEA value of < .05 indicates a close fit,and that < .08 suggests a reasonable model fit
#TLI > .90 indicates an acceptable fit
#Hu and Bentler suggested that an RMSEA smaller than .06 and a CFI and TLI larger than .95 indicate relatively good model data fit

##CFA for wbnj in validation sample 
library(lavaan)

wbcfa <- '
wb =~   NA*rev_family2 + rev_happy2 + rev_friendship2 + rev_financial2 + rev_health2
wb~~1*wb' 
#NA*rev_family is to freely estimate the variance and not confined it to 1
#wb~~1*wb is to fix the wb variance to 1. 

fit <- cfa(wbcfa, data = wbnj, estimator="WLSMV")
summary(fit, fit.measures=TRUE)
fitted(fit)
