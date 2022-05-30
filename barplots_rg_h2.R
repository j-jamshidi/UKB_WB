setwd("Z:/UKB/p_analysis/R/UKB")
#plot with ggplot=======
library(ggplot2)
tesdata <- read.csv("rgs_plot.csv", header = T)
tesdata$p2 <- factor(tesdata$p2 , levels= tesdata$p2[order(tesdata$ord)])
    ggplot(data=tesdata,aes(x=p2, y=rg )) +
    geom_bar(stat="identity", color= "black",
             fill= c(rep("#ffc266",2),"gray60",rep("#ffc266",3),"gray60",rep("#ffc266",4), rep("#53c653", 3),"gray60", rep("#53c653", 2)), size=1.3, width = 0.80)+
    geom_errorbar( aes(ymin=rg-(1.96*se), ymax=rg+(1.96*se)),width=.15 , size=1.3, color= "black") +
    coord_flip() +  # flip coordinates (puts labels on y axis)
    ylab("Genetic correlation") + xlab("")+
    theme_classic()+ # use a white background
    scale_y_continuous(breaks=c(-0.8,-0.4,0,0.4,.8))+
    theme(text = element_text(size = 23, color = "black"),axis.line.y= element_blank() ,axis.ticks.y = element_blank(), legend.position = "none")+
    theme(axis.ticks = element_line(size = 1.7) , axis.ticks.length = unit(.3, "cm"), axis.line = element_line(size = 1.3), 
          axis.text = element_text(size = 23, color = "black"))

#NEW  Plot for rgs of wellbeing with other traits outsise ukb======
    opar <- par(lwd = 3,mar = c(5, 16, .1, 1))
    data <- read.csv("rgs_plot.csv", header = T) 
    data$p2 <- factor(data$p2 , levels= data$p2[order(data$ord)])
    barplot1 <- barplot(data$rg,
                        xlab="Genetic correlation",  
                        col=c(rep("#ffc266",2),"gray60",rep("#ffc266",3),"gray60",rep("#ffc266",4), rep("#66cc00", 3),"gray60", rep("#66cc00", 2)), 
                        las=1, 
                        horiz = TRUE,
                        names.arg=data$p2, 
                        cex.names=1.7,
                        cex.axis = 1.7,
                        cex.lab=1.7,
                        lwd=2,
                        xlim=range(-0.8,-.4,0,.4,.8,1.2))
    
    
    segments(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96 , barplot1,
             lwd = 2)
    
    arrows(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96, barplot1, 
           lwd = 2, angle = 90,
           code = 3, length = 0.05)
#the same plot BLACK & white 
    opar <- par(lwd = 3,mar = c(5, 16, .1, 1))
    data <- read.csv("rgs_plot.csv", header = T) 
    data$p2 <- factor(data$p2 , levels= data$p2[order(data$ord)])
    barplot1 <- barplot(data$rg,
                        xlab="Genetic correlation (rg)",  
                        col=c(rep("gray80",2),"white",rep("gray80",3),"white",rep("gray80",4), rep("gray80", 3),"white", rep("gray80", 2)), 
                        las=1, 
                        horiz = TRUE,
                        names.arg=data$p2, 
                        cex.names=1.7,
                        cex.axis = 1.7,
                        cex.lab=1.7,
                        lwd=2,
                        xlim=range(-0.8,-.4,0,.4,.8,1.2))
    
    
    segments(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96 , barplot1,
             lwd = 2)
    
    arrows(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96, barplot1, 
           lwd = 2, angle = 90,
           code = 3, length = 0.05)
    
    
    
#NEW  Plot for rgs of childhood maltreatment ======
    opar <- par(lwd = 3,mar = c(5, 16, .1, 1))
    data <- read.csv("chTrauma_rg.csv", header = T) 
    data$p2 <- factor(data$p2 , levels= data$p2[order(data$rg)])
    barplot1 <- barplot(data$rg,
                        xlab="Genetic correlation (rg)", 
                        col=c(rep("#ffc266",6), rep("#66cc00", 4)), 
                        las=1, 
                        horiz = TRUE,
                        names.arg=data$p2, 
                        cex.names=1.7,
                        cex.axis = 1.7,
                        cex.lab=1.7,
                        lwd=2,
                        xlim=range(-0.8,.8))
    
    
    segments(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96 , barplot1,
             lwd = 2)
    
    arrows(data$rg - data$se*1.96, barplot1, data$rg + data$se*1.96, barplot1, 
           lwd = 2, angle = 90,
           code = 3, length = 0.05)

##Plots for SNP Heritability==================
h2 <- read.csv("snp_h2.csv", header = T)
h2$Traits <- factor(h2$Traits , levels= h2$Traits[order(h2$Ldsc_h2)])    
      opar <- par(lwd = 3,mar = c(16, 6, 2, .1))
      barCentres<- barplot(height=h2$Ldsc_h2, names=h2$Traits, 
                           col="#cfe2e2", lwd=2.5,  cex.axis = 1.8, cex.names = 1.8,ylim = c(0,.1), las=3, 
                           ylab = (expression('SNP heritability (h'^2*')')), cex.lab=1.7)
      arrows(barCentres, h2$Ldsc_h2+h2$se, barCentres,
             h2$Ldsc_h2-h2$se, lwd = 3, angle = 90,
             code = 3, length = 0.07, )

    