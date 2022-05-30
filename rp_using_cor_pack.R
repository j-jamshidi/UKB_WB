# Load Prepare data-------
setwd("Z:/UKB/p_analysis/R/UKB")
wbnj <- read.csv("wbnojob_gqc_Caucasian_numeric.csv")
wbnj <- as.data.frame(wbnj)
wbnj[wbnj == -1] <- NA
wbnj[wbnj == -3] <- NA
wbnj[wbnj == -818] <- NA
wbnj[wbnj == -121] <- NA
wbnj[wbnj == -9] <- NA
wbnj[wbnj == " "] <- NA

wbnj$Friendships_satisfaction <- as.numeric(wbnj$rev_friendship)
wbnj$Happiness <- as.numeric(wbnj$rev_happy)
wbnj$Family_satisfaction <- as.numeric(wbnj$rev_family)
wbnj$Financial_satisfaction <- as.numeric(wbnj$Rev_Finantial)
wbnj$wellbeing_score <- as.numeric(wbnj$zwb_nojob)
wbnj$Health_satisfaction <- as.numeric(wbnj$rev_health)
wbnj$Meaningful_life <- as.numeric(wbnj$Belief_that_own_life_is_meaningful)
wbnj$Neuroticism <- as.numeric(wbnj$Neuroticism_score)
wbnj$Depressive_symptoms <- as.numeric(wbnj$Depressive_symptomes_Ever)
wbnj$Seen_GP_or_psychiatrist <- as.numeric(wbnj$Ever_seen_GP_OR_Psychiatrist_for_nerv_anx_dep)
wbnj$Loneliness <- as.numeric(wbnj$Loneliness_Isolation)
wbnj$MH_Happiness <- as.numeric(wbnj$MH_General_happiness)

library(dplyr)
library(correlation)
#organize data ==========
newdat <- wbnj %>% select(39:44,205:211)
nn <- newdat %>% select(1:4,6,7,9:13)
nn <- nn %>% select(!11)
nn <- nn %>% select(6,5,2,3,1,4,7,10,8,9)

corm <- correlation(na.omit(nn), method = "auto", p_adjust = "bonferroni")

corm <- correlation(na.omit(nn), method = "spearman", p_adjust = "bonferroni")

summary(corm)

corm$Method

write.csv(corm, "corm-auto")
write.csv(corm, "corm-spearman")

packageVersion("correlation")                
