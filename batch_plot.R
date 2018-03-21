# batch_plot.R
library(descr)
library(pROC)
library(reshape)
library(readxl)

# Recode some variables====
beacon_imp$cc2= ifelse(beacon_imp$cc==0, 0, 1)
beacon_imp$risk10a= ifelse(beacon_imp$risk10>20, 21, beacon_imp$risk10)
crosstab(beacon_imp$race, beacon_imp$sex)

beacon_imp$demog= ifelse(beacon_imp$sex==1 & beacon_imp$race==1,"WM", 
                    ifelse(beacon_imp$sex==2 & beacon_imp$race==1, "WF",
                      ifelse(beacon_imp$sex==1 & beacon_imp$race==2, "BM", "NA")))

beacon_imp$age5= ifelse(beacon_imp$age<45, 1,
                    ifelse(beacon_imp$age<50, 2,
                      ifelse(beacon_imp$age<55, 3,
                             ifelse(beacon_imp$age<60, 4,
                                    ifelse(beacon_imp$age<65, 5,
                                           ifelse(beacon_imp$age<70, 6,
                                                  ifelse(beacon_imp$age<75, 7,
                                                         ifelse(beacon_imp$age<80, 8, NA)
                                                         )))))))

# Determine fraction of BEACON controls in each age group
sum_bea= beacon_imp %>% group_by(age5) %>%
  filter(cc==0) %>% summarise(numbea= n())
sum_bea$tot_cont= sum(sum_bea$numbea)
sum_bea= mutate(sum_bea, BeaFrac= numbea/tot_cont)

# Add information on study
# site_info= read_excel("site_info.xlsx")

# Add variables from US Census====
USpop2015= read_excel("USpop2015.xlsx")

# Append above info to datafile
beacon_imp= merge(beacon_imp, USpop2015)
beacon_imp= merge(beacon_imp, sum_bea)
# beacon_imp2= merge(beacon_imp, site_info, all.y= FALSE)

# Expand controls to represent US population====
# beacon_imp= mutate(beacon_imp, wt2= ifelse(age2==1, sample(c(4,5,6)), sample(c(1,2,3))))

beacon_imp= mutate(beacon_imp, wt2= (round(10*PopFrac/BeaFrac)))
beacon_imp= mutate(beacon_imp, wt2= ifelse(wt2>0, wt2, 1))

# Don't expand cases
beacon_imp= mutate(beacon_imp, wt3= ifelse(cc2==1, 1, wt2))
beacon_imp_exp= untable(beacon_imp[,c(1:26)], num=beacon_imp$wt3)

p1= ggplot(beacon_imp_exp, aes(risk10a, ..density.., color= as.factor(cc2))) +
  geom_freqpoly(bins= 26) +
  scale_color_discrete(name= "", labels= c("Controls", "BE/EA")) +
  theme(legend.position= c(.7, .5)) +
  xlab("10 year risk of EA") +
  geom_hline(yintercept = 0, linetype = "longdash", size= 0.8, colour= "black")+
  geom_vline(xintercept = 0.6, linetype = "longdash", size= 0.6, colour= "blue")+
  geom_vline(xintercept = 1.3, linetype = "longdash", size= 0.6, colour= "blue")+
  geom_vline(xintercept = 4.2, linetype = "longdash", size= 0.8, colour= "green")+
  geom_vline(xintercept = 6.8, linetype = "longdash", size= 0.8, colour= "green")+
  geom_vline(xintercept = 8.4, linetype = "longdash", size= 0.8, colour= "green")

myroc= roc(beacon_imp_exp$cc2, beacon_imp_exp$risk10, auc=TRUE, ci=TRUE)

plot.roc(myroc, xlim= c(1,0), ylim= c(0,1))
myroc

beacon_imp_exp$pcc= ifelse(beacon_imp_exp$risk10a> 0.6, 1, 0)
crosstab(beacon_imp_exp$cc2, beacon_imp_exp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp_exp$pcc= ifelse(beacon_imp_exp$risk10a> 1.3, 1, 0)
crosstab(beacon_imp_exp$cc2, beacon_imp_exp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp_exp$pcc= ifelse(beacon_imp_exp$risk10a> 4.2, 1, 0)
crosstab(beacon_imp_exp$cc2, beacon_imp_exp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp_exp$pcc= ifelse(beacon_imp_exp$risk10a> 6.8, 1, 0)
crosstab(beacon_imp_exp$cc2, beacon_imp_exp$pcc, prop.r= TRUE, plot= FALSE)
beacon_imp_exp$pcc= ifelse(beacon_imp_exp$risk10a> 8.4, 1, 0)
crosstab(beacon_imp_exp$cc2, beacon_imp_exp$pcc, prop.r= TRUE, plot= FALSE)

p1
