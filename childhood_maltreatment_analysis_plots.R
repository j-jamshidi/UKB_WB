# Load Prepare data----
setwd("Z:/UKB/p_analysis/R/UKB")
wbnj <- read.csv("wbnojob_gqc_Caucasian.csv", header = TRUE)
wbnj <- as.data.frame(wbnj)
#libraries
library(lm.beta)
require(ggplot2)
require(ggpubr)
library(RColorBrewer)
#deal with missing data&change the codes
wbnj[wbnj == "Do not know"] <- NA
wbnj[wbnj == "prefer not to say"] <- NA
wbnj[wbnj == "Prefer not to answer"] <- NA
wbnj[wbnj == " "] <- NA
#make a new dataset to work with sum score later
wbnj_s <- wbnj
#a bit of extra changes for ploting each categorical maltretment
wbnj[wbnj == "Never true"] <- "Never"
wbnj[wbnj == "Rarely true"] <- "Rarely"
wbnj[wbnj == "Sometimes true"] <- "Sometimes"
wbnj[wbnj == "Very often true"] <- "Very often"
#set different types of trauma as factor and assign names***
wbnj$Childhood_Physical_neglect <- factor(wbnj$T_Ch_MH_Someone_to_take_to_doctor_when_needed_as_a_child , levels=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"))
wbnj$Childhood_Emotional_neglect <- factor(wbnj$T_Ch_MH_Felt_loved_as_a_child, levels=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"))
wbnj$Childhood_sexual_maltreatment <- factor(wbnj$T_Ch_MH_Sexually_molested_as_a_child, levels=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"))
wbnj$Childhood_Physical_maltreatment <- factor(wbnj$T_Ch_MH_Physically_abused_by_family_as_a_child, levels=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"))
wbnj$Childhood_Emotional_maltreatment <- factor(wbnj$T_Ch_MH_Felt_hated_by_family_member_as_a_child, levels=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"))
#prepare data for the multreatment sum and linear regresion with renamed database wbnj_s
wbnj_s[wbnj_s == "Never true"] <- as.numeric(0)
wbnj_s[wbnj_s == "Rarely true"] <-  as.numeric(1)
wbnj_s[wbnj_s == "Sometimes true"] <-  as.numeric(2)
wbnj_s[wbnj_s == "Often"] <-  as.numeric(3)
wbnj_s[wbnj_s == "Very often true"] <-  as.numeric(4)
#change the variables to numeric
wbnj_s$Emotional_neglect <- 4- as.numeric(wbnj_s$T_Ch_MH_Felt_loved_as_a_child) # solving the reversed code
wbnj_s$sexual_maltreatment <- as.numeric(wbnj_s$T_Ch_MH_Sexually_molested_as_a_child)
wbnj_s$Physical_maltreatment <- as.numeric(wbnj_s$T_Ch_MH_Physically_abused_by_family_as_a_child)
wbnj_s$Emotional_maltreatment <- as.numeric(wbnj_s$T_Ch_MH_Felt_hated_by_family_member_as_a_child)
#Transef to binary variables 
wbnj_s$bi_Emotional_neglect <- wbnj_s$Emotional_neglect 
wbnj_s$bi_sexual_maltreatment <- wbnj_s$sexual_maltreatment
wbnj_s$bi_Physical_maltreatment <- wbnj_s$Physical_maltreatment
wbnj_s$bi_Emotional_maltreatment  <- wbnj_s$Emotional_maltreatment
#
wbnj_s$bi_Emotional_neglect[wbnj_s$bi_Emotional_neglect == 1] <- 0
wbnj_s$bi_Emotional_neglect[wbnj_s$bi_Emotional_neglect != 0] <- 1
wbnj_s$bi_sexual_maltreatment[wbnj_s$bi_sexual_maltreatment != 0] <- 1
wbnj_s$bi_Physical_maltreatment[wbnj_s$bi_Physical_maltreatment != 0] <- 1
wbnj_s$bi_Emotional_maltreatment [wbnj_s$bi_Emotional_maltreatment != 0] <- 1
#making them as numeric variables 
wbnj_s$bi_Emotional_neglect <- as.numeric(wbnj_s$bi_Emotional_neglect )
wbnj_s$bi_sexual_maltreatment <- as.numeric(wbnj_s$bi_sexual_maltreatment)
wbnj_s$bi_Physical_maltreatment <- as.numeric(wbnj_s$bi_Physical_maltreatment)
wbnj_s$bi_Emotional_maltreatment <- as.numeric(wbnj_s$bi_Emotional_maltreatmen)
#make the sum variable
wbnj_s$Ch_Trauma_Sum <-NULL
wbnj_s$ch_Trauma_Sum <- (wbnj_s$bi_Emotional_neglect + wbnj_s$bi_sexual_maltreatment + wbnj_s$bi_Physical_maltreatment + wbnj_s$bi_Emotional_maltreatment)

#STATS OF MALE AND FEMALE and the plot--------
wbnj$sex <- as.factor(wbnj$sex)
compare_means(zwb_nojob ~ sex , wbnj)
s <- paste0((tapply(wbnj$zwb_nojob, wbnj$sex, length))," ","(",as.data.frame(round(tapply(wbnj$zwb_nojob, wbnj$sex, length) /
                                                                                     length(na.omit(wbnj$sex))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnj$zwb_nojob, wbnj$sex, mean),digits = 3))

ggviolin(wbnj,"sex", "zwb_nojob" , ylab = "Wellbeing Score",xlab="Sex" ,fill = "sex",
         palette=c("lightgoldenrodyellow","whitesmoke"), select=c("Female", "Male"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 0.8, color="gray10")) + 
  geom_hline(yintercept = median(wbnj$zwb_nojob), linetype = 2 ) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnj$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:2, y = -6.7,  label = c(s[1],s[2]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:2, y = 3.4,  label = c(m[1],m[2]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.4, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17)) 


#childhood Physical_neglect====================
#Just people with valid info on childhood trauma =
wbnjpn <- wbnj[complete.cases(wbnj$Childhood_Physical_neglect), ]
#Standardize the wellbeing index score 
wbnjpn$zwb_nojob <- (wbnjpn$zwb_nojob - mean(wbnjpn$zwb_nojob)) / sd(wbnjpn$zwb_nojob)
#summary(wbnjpn$zwb_nojob)
compare_means(zwb_nojob ~ Childhood_Physical_neglect , wbnjpn)
my_comparisons <- list( c("Never","Rarely"),c("Sometimes","Rarely"),c("Sometimes","Often"),c("Often","Very often"))
s <- paste0((tapply(wbnjpn$zwb_nojob, wbnjpn$Childhood_Physical_neglect, length))," ","(",as.data.frame(round(tapply(wbnjpn$zwb_nojob, wbnjpn$Childhood_Physical_neglect, length) /
                                                                                                                length(na.omit(wbnjpn$Childhood_Physical_neglect))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnjpn$zwb_nojob, wbnjpn$Childhood_Physical_neglect, mean),digits = 3))

ggviolin(wbnjpn,"Childhood_Physical_neglect", "zwb_nojob" , ylab = "Wellbeing Score",xlab = "Childhood physical neglect (reverse)", fill = "Childhood_Physical_neglect",
         palette="RdYlGn", select=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 1, color="gray10")) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnjpn$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[1],s[2],s[3],s[4],s[5]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[1],m[2],m[3],m[4],m[5]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.5, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17)) 

#Childhood_Emotional_neglect =====================
wbnjen <- wbnj[complete.cases(wbnj$Childhood_Emotional_neglect), ]
#Standardize the wellbeing index score 
wbnjen$zwb_nojob <- (wbnjen$zwb_nojob - mean(wbnjen$zwb_nojob)) / sd(wbnjen$zwb_nojob)
#summary(wbnjen$zwb_nojob)
compare_means(zwb_nojob ~ Childhood_Emotional_neglect , wbnjen)
my_comparisons <- list( c("Never","Rarely"),c("Sometimes","Rarely"),c("Sometimes","Often"),c("Often","Very often"))
s <- paste0((tapply(wbnjen$zwb_nojob, wbnjen$Childhood_Emotional_neglect, length))," ","(",as.data.frame(round(tapply(wbnjen$zwb_nojob, wbnjen$Childhood_Emotional_neglect, length) /
                                                                                                                 length(na.omit(wbnjen$Childhood_Emotional_neglect))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnjen$zwb_nojob, wbnjen$Childhood_Emotional_neglect, mean),digits = 3))

ggviolin(wbnjen,"Childhood_Emotional_neglect", "zwb_nojob" , ylab = "Wellbeing Score",xlab = "Childhood emotional neglect (reverse)", fill = "Childhood_Emotional_neglect",
         palette="RdYlGn", select=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 1, color="gray10")) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnjen$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[1],s[2],s[3],s[4],s[5]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[1],m[2],m[3],m[4],m[5]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.5, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17)) 

#Childhood_sexual_maltreatment================================
wbnjsm <- wbnj[complete.cases(wbnj$Childhood_sexual_maltreatment), ]
#Standardize the wellbeing index score 
wbnjsm$zwb_nojob <- (wbnjsm$zwb_nojob - mean(wbnjsm$zwb_nojob)) / sd(wbnjsm$zwb_nojob)
compare_means(zwb_nojob ~ Childhood_sexual_maltreatment , wbnjsm)
my_comparisons <- list( c("Never","Rarely"),c("Sometimes","Rarely"),c("Sometimes","Often"),c("Often","Very often"))
s <- paste0((tapply(wbnjsm$zwb_nojob, wbnjsm$Childhood_sexual_maltreatment, length))," ","(",as.data.frame(round(tapply(wbnjsm$zwb_nojob, wbnjsm$Childhood_sexual_maltreatment, length) /
                                                                                                                   length(na.omit(wbnjsm$Childhood_sexual_maltreatment))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnjsm$zwb_nojob, wbnjsm$Childhood_sexual_maltreatment, mean),digits = 3))

ggviolin(wbnjsm,"Childhood_sexual_maltreatment", "zwb_nojob" , ylab = "Wellbeing Score",xlab = "Childhood sexual maltreatment", fill = "Childhood_sexual_maltreatment",
         palette="RdYlGn", select=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"),
         order =c("Very often","Often", "Sometimes","Rarely","Never"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 1, color="gray10")) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnjsm$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[5],s[4],s[3],s[2],s[1]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[5],m[4],m[3],m[2],m[1]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.5, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17)) 

#Childhood_Physical_maltreatment==================
wbnjpm <- wbnj[complete.cases(wbnj$Childhood_Physical_maltreatment), ]
#Standardize the wellbeing index score 
wbnjpm$zwb_nojob <- (wbnjpm$zwb_nojob - mean(wbnjpm$zwb_nojob)) / sd(wbnjpm$zwb_nojob)
compare_means(zwb_nojob ~ Childhood_Physical_maltreatment , wbnjpm)
my_comparisons <- list( c("Never","Rarely"),c("Sometimes","Rarely"),c("Sometimes","Often"),c("Often","Very often"))
s <- paste0((tapply(wbnjpm$zwb_nojob, wbnjpm$Childhood_Physical_maltreatment, length))," ","(",as.data.frame(round(tapply(wbnjpm$zwb_nojob, wbnjpm$Childhood_Physical_maltreatment, length) /
                                                                                                                     length(na.omit(wbnjpm$Childhood_Physical_maltreatment))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnjpm$zwb_nojob, wbnjpm$Childhood_Physical_maltreatment, mean),digits = 3))

ggviolin(wbnjpm,"Childhood_Physical_maltreatment", "zwb_nojob" , ylab = "Wellbeing Score",xlab = "Childhood physical maltreatment", fill = "Childhood_Physical_maltreatment",
         palette="RdYlGn", select=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"),
         order =c("Very often","Often", "Sometimes","Rarely","Never"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 1, color="gray10")) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnjpm$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[5],s[4],s[3],s[2],s[1]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[5],m[4],m[3],m[2],m[1]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.5, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17))  

#Childhood_Emotional_maltreatment=============
wbnjem <- wbnj[complete.cases(wbnj$Childhood_Emotional_maltreatment), ]
#Standardize the wellbeing index score 
wbnjem$zwb_nojob <- (wbnjem$zwb_nojob - mean(wbnjem$zwb_nojob)) / sd(wbnjem$zwb_nojob)
compare_means(zwb_nojob ~ Childhood_Emotional_maltreatment , wbnjem)
my_comparisons <- list( c("Never","Rarely"),c("Sometimes","Rarely"),c("Sometimes","Often"),c("Often","Very often"))
s <- paste0((tapply(wbnjem$zwb_nojob, wbnjem$Childhood_Emotional_maltreatment, length))," ","(",as.data.frame(round(tapply(wbnjem$zwb_nojob, wbnjem$Childhood_Emotional_maltreatment, length) /
                                                                                                                      length(na.omit(wbnjem$Childhood_Emotional_maltreatment))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnjem$zwb_nojob, wbnjem$Childhood_Emotional_maltreatment, mean),digits = 3))

ggviolin(wbnjem,"Childhood_Emotional_maltreatment", "zwb_nojob" , ylab = "Wellbeing Score",xlab = "Childhood emotional maltreatment", fill = "Childhood_Emotional_maltreatment",
         palette="RdYlGn", select=c("Never","Rarely" ,"Sometimes" ,"Often","Very often"),
         order =c("Very often","Often", "Sometimes","Rarely","Never"),color = 'gray10',
         add='median_iqr',  add.params = list(size = 1, color="gray10")) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnjem$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[5],s[4],s[3],s[2],s[1]) ,color="gray10", size=6) +
  annotate("text", x = .5, y = -6.7, label = "N", color="gray20", size=5.5) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[5],m[4],m[3],m[2],m[1]) ,color="gray10", size=6) +
  annotate("text", x = .6, y = 3.5, label = "Mean", color="gray20", size=5.5) +
  theme(text = element_text(size = 17))   
#Comparing mean wellbeing in SUM score groups (PLOT)---------------
wbnj_s <- wbnj_s[complete.cases(wbnj_s$ch_Trauma_Sum),]
wbnj_s$ch_Trauma_Sum <- as.factor(wbnj_s$ch_Trauma_Sum)
wbnj_s$zwb_nojob <- scale(as.numeric(wbnj_s$zwb_nojob))
compare_means(zwb_nojob ~ ch_Trauma_Sum , wbnj_s)
my_comparisons <- list( c("0","1"),c("1","2"),c("2","3"),c("3","4"))
s <- paste0((tapply(wbnj_s$zwb_nojob, wbnj_s$ch_Trauma_Sum, length))," ","(",as.data.frame(round(tapply(wbnj_s$zwb_nojob, wbnj_s$ch_Trauma_Sum, length) /
                                                                                                   length(na.omit(wbnj_s$ch_Trauma_Sum))*100,digits = 1))[,1],"%",")")
m<- paste0(round(tapply(wbnj_s$zwb_nojob, wbnj_s$ch_Trauma_Sum, mean),digits = 3))
ggviolin(wbnj_s,"ch_Trauma_Sum", "zwb_nojob" , ylab = "Wellbeing index",xlab="Childhood Maltreatment Sum" ,fill = "ch_Trauma_Sum",
         palette=c("white", "gray95", "gray90", "gray85", "gray80"), select=c('0',"1","2" ,"3" ,"4"), lwd=5,
         add='median_iqr',  add.params = list(size = 1)) + 
  stat_compare_means(comparisons = my_comparisons, label.y = c(4,4.4,4,4.4,4), color="gray20", size=5.5) +
  stat_compare_means(label.y = 5.7, label.x = .7 , size =5.5)+
  geom_hline(yintercept = median(wbnj_s$zwb_nojob), linetype = 2 ) +
  annotate("text", x = 1:5, y = -6.7,  label = c(s[1],s[2],s[3],s[4],s[5]) ,color="gray10", size=6) +
  annotate("text", x = 1:5, y = 3.5,  label = c(m[1],m[2],m[3],m[4],m[5]) ,color="gray10", size=6) +
  scale_y_continuous(breaks=c(-6.7,-5,-2.5,0,2.5,3.5), labels = c("N(%) ","-5","-2.5","0","2.5","Mean")) +
  theme(text = element_text(size = 18), legend.position = "none") 
















##linear models for  each maltreatment separatly-------
malt <- data.frame(matrix(ncol = 6, nrow = 0))
colnames(malt) <- c("variable", "estimate", "lci_estimate", "uci_estimate", "stderr_estimate","pval")
#loop for models
for (i in c(193:197)) {
  fitm <-lm.beta(lm( zwb_nojob ~ wbnj_s[,i] +age + age_sqrt + sex + deprivation_index ,wbnj_s)) #full model
  fit <- summary(fitm)
  p_val <- fit$coefficients[2,5] #p-value
  estimate <- fit$coefficients[2,2] # standardized coefficient of the regression or estimate
  lci_estimate <- confint(fitm)[2,1]# 95% confidence interval of the estimate lower band
  uci_estimate <- confint(fitm)[2,2]# 95% confidence interval of the estimate upper band
  std_error <- fit$coefficients[2,3]# standard error of estimate
  n <- length(na.omit(wbnj_s[,i])) # Sample size of the test
  malt[nrow(malt)+1,] <- c(colnames(wbnj_s)[i],estimate,lci_estimate,uci_estimate,std_error,p_val ) #save in a dataframe
}

##linear models for  all maltreatments together-------
for (i in c(2:5)) {
  fitm <-lm.beta(lm( zwb_nojob ~ bi_sexual_maltreatment+bi_Physical_maltreatment+bi_Emotional_maltreatment+bi_Emotional_neglect
                     +age + age_sqrt + sex + deprivation_index ,wbnj_s)) #full model
  fit <- summary(fitm)
  p_val <- fit$coefficients[i,5] #p-value
  estimate <- fit$coefficients[i,2] # standardized coefficient of the regression or estimate
  lci_estimate <- confint(fitm)[i,1]# 95% confidence interval of the estimate lower band
  uci_estimate <- confint(fitm)[i,2]# 95% confidence interval of the estimate upper band
  std_error <- fit$coefficients[i,3]# standard error of estimate
  nam <- as.data.frame(names(fitm$coefficients))[i,1]
  malt[nrow(malt)+1,] <- c(nam,estimate,lci_estimate,uci_estimate,std_error,p_val ) #save in a dataframe
}
malt$group <- c("A",	"A",	"A",	"A",	"A",	"B",	"B",	"B",	"B")
#Plot the results from linear models=======
malt[2:5] <- as.numeric(unlist(malt[2:5]))
ggplot() +
  geom_pointrange(data=malt, aes(x=variable, y=estimate, ymin=lci_estimate, ymax=uci_estimate), size=1)  +
  ggtitle("")+
  geom_hline(yintercept=0, lty=2, color ="gray20") +  # add a dotted line at x=1 after flip
  facet_grid(cols = vars(group))+
  coord_flip() +  # flip coordinates (puts labels on y axis)
  ylab("standardised estimate (95% CI)") + xlab("")+
  theme_classic(base_size = 15)+
  theme(axis.text=element_text(size=15))+
  scale_x_discrete(limits= c("ch_Trauma_Sum","bi_Emotional_neglect","bi_Emotional_maltreatment","bi_Physical_maltreatment","bi_sexual_maltreatment"
  ),
  labels = c("Maltreatment Sum Score","Emotional neglect","Emotional maltreatment","Physical maltreatment","Sexual maltreatment"
  ))
