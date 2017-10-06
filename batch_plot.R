library(descr)
library(pROC)
beacon_imp$cc2= ifelse(beacon_imp$cc==0, 0, 1)
beacon_imp$risk10a= ifelse(beacon_imp$risk10>22, 22, beacon_imp$risk10)
p1= ggplot(beacon_imp, aes(risk10a, ..density.., color= as.factor(cc2))) +
  geom_freqpoly(bins= 26) +
  scale_color_discrete(name= "", labels= c("Controls", "BE/EA")) +
  theme(legend.position= c(.9, .5)) +
  xlab("10 year risk of EA") +
  geom_hline(yintercept = 0, linetype = "longdash", size= 0.8, colour= "black")+
  geom_vline(xintercept = 0.8, linetype = "longdash", size= 0.6, colour= "blue")+
  geom_vline(xintercept = 1.3, linetype = "longdash", size= 0.6, colour= "blue")+
  geom_vline(xintercept = 3.5, linetype = "longdash", size= 0.8, colour= "green")+
  geom_vline(xintercept = 6.0, linetype = "longdash", size= 0.8, colour= "green")+
  geom_vline(xintercept = 11.6, linetype = "longdash", size= 0.8, colour= "green")

myroc= roc(beacon_imp$cc2, beacon_imp$risk10, auc=TRUE, ci=TRUE)

plot.roc(myroc, xlim= c(1,0), ylim= c(0,1))
myroc

beacon_imp$pcc= ifelse(beacon_imp$risk10a> 0.8, 1, 0)
crosstab(beacon_imp$cc2, beacon_imp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp$pcc= ifelse(beacon_imp$risk10a> 1.3, 1, 0)
crosstab(beacon_imp$cc2, beacon_imp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp$pcc= ifelse(beacon_imp$risk10a> 3.5, 1, 0)
crosstab(beacon_imp$cc2, beacon_imp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp$pcc= ifelse(beacon_imp$risk10a> 6.0, 1, 0)
crosstab(beacon_imp$cc2, beacon_imp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp$pcc= ifelse(beacon_imp$risk10a> 11.6, 1, 0)
crosstab(beacon_imp$cc2, beacon_imp$pcc, prop.r= TRUE, plot= FALSE)

p1