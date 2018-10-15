# batch_plot.R

example1= 0.66
example2= 0.97
addvertlines= function(theplot) {
  theplot + 
    geom_vline(xintercept = example1, linetype = "solid", size= 0.5, colour= "black")+
    geom_vline(xintercept = example2, linetype = "solid", size= 0.5, colour= "black")+
    geom_vline(xintercept = 2., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 4., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 6., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 8., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 10., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 12., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 14., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 16., linetype = "longdash", size= 0.4, colour= "darkgray")+
    geom_vline(xintercept = 18., linetype = "longdash", size= 0.4, colour= "darkgray")
}
toprisk= 18

beacon_imp$cc2= ifelse(beacon_imp$cc==0, 0, 1)

beacon_imp$risk10a= ifelse(beacon_imp$risk10>(toprisk-1), toprisk, beacon_imp$risk10)
# crosstab(beacon_imp$race, beacon_imp$sex)

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

# Determine fraction of BEACON controls in each age & race group
sum_bea= beacon_imp %>% 
  group_by(age5, demog) %>%
  filter(cc==0) %>% 
  summarise(numbea= n())
sum_bea$tot_cont= sum(sum_bea$numbea)
sum_bea= mutate(sum_bea, BeaFrac= numbea/tot_cont)

# Add information on study
# site_info= read_excel("./indata/site_info.xlsx")

# Add variables from US Census====
USpop2015= read_excel("./input_data/USpop2015.xlsx")

# Append above info to datafile
beacon_imp= merge(beacon_imp, USpop2015)
beacon_imp= merge(beacon_imp, sum_bea)
# beacon_imp2= merge(beacon_imp, site_info, all.y= FALSE)

# Expand controls to represent US population by age5 and race ====
library(reshape)
beacon_imp= mutate(beacon_imp, wt2= (round(10*PopFrac/BeaFrac)))
beacon_imp= mutate(beacon_imp, wt2= ifelse(wt2>0, wt2, 1))    # wt2 must be minimum 1
beacon_imp= mutate(beacon_imp, wt3= ifelse(cc2==1, 1, wt2))   # wt3 = 1 if case
beacon_imp_exp= untable(beacon_imp[,c(1:26)], num=beacon_imp$wt3) #expand all rows by wt3
detach("package:reshape", unload=TRUE)

# Determine which cases to use
# whoiscase= "BE/EA"
# beacon_imp_exp= filter(beacon_imp_exp, cc== 0 | cc==1 | cc== 2)
whoiscase= "EACs"
beacon_imp_exp= filter(beacon_imp_exp, cc== 0| cc== 2)

#this has already been filtered by case type  
myroc= roc(beacon_imp_exp$cc2, beacon_imp_exp$risk10, auc=TRUE, ci=TRUE)
plot.roc(myroc)
myroc[["ci"]]
myroc[["auc"]]
# Area under the curve: 0.8136
# 95% CI: 0.7961-0.831 (DeLong)

p1= ggplot(beacon_imp_exp) +
  geom_freqpoly(aes(risk10a, y= ..density.., linetype= as.factor(cc2)), bins= toprisk+1, size=.8) +
  # scale_color_manual(values = c("red", "blue"), labels= c("Controls", whoiscase)) +
  scale_linetype_manual(values = c(3,1), labels= c("Controls", whoiscase)) +

  theme(axis.line.x=element_blank(),
        legend.position= c(.2, .7), legend.text=element_text(size=14),
        legend.box.just = c("top"),
        legend.title=element_blank(),
        legend.background = element_rect(fill=alpha('white', 1.0))) +
  xlab("10 year risk of EAC (x1,000)") +
  scale_x_continuous(breaks= c(seq(0,toprisk,2))) +
  geom_hline(yintercept = 0, linetype = "solid", size= 0.6, colour= "black")
  
p1= addvertlines(p1)
  # theme(panel.grid.major.x = element_line(colour="darkgray", size=0.1)) +

res= coords(myroc, x= c(example1, example2, seq(2, toprisk, by=2)), 
            ret= c("threshold", "spec", "sens"))
mycols=rownames(res)      # save row names
tres= data.frame(t(res))  # transpose rows and columns
colnames(tres)= mycols    # re-assign old rownames to column (variables)

caserate= 160/100000   # took number of cases (WM, WF, BM) from SEER9 (40-79 years) and multiplied by 10
# caserate= 160/100000
pop= 100000
numcases= caserate*pop
numnoncases= pop- numcases

tres= mutate(tres, 
             tp= sensitivity*numcases, 
             tn= specificity* numnoncases,
             fp= numnoncases- tn, 
             fn= numcases- tp,
             ppv=tp/(tp+fp),
             npv= tn/(tn+fn)
)
fpbase= tres$fp[1]
tres= mutate(tres, endosav= 100*(fpbase-fp) / fpbase)
# View(tres)

pppv= ggplot(tres, aes(threshold, 100*ppv)) +
  geom_point() +
  # ylim(0, 2.2) +
  ylab("ppv (%)") +
  expand_limits(x=c(-1.5, (toprisk+1.5))) +
  scale_x_continuous(breaks= c(seq(0,toprisk,2))) +
  scale_y_continuous(breaks= c(seq(0, 2.0, .5)), limits= c(0, 2.2)) +
  xlab(NULL) +
  theme(panel.grid.major.y = element_line(colour="darkgray", size=0.5), 
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())
  pppv= addvertlines(pppv)

psens= ggplot(tres, aes(threshold, sensitivity)) +
  geom_point() +
  expand_limits(x=c(-1.5, (toprisk+1.5))) +
  scale_x_continuous(breaks= c(seq(0,toprisk,2))) +
  scale_y_continuous(breaks= c(seq(0, 1.0, .2)), limits= c(0,1.0)) +
  xlab(NULL) +
  theme(panel.grid.major.y = element_line(colour="darkgray", size=0.5),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())
  psens= addvertlines(psens)

pspec= ggplot(tres, aes(threshold, specificity)) +
  geom_point() +
  expand_limits(x=c(-1.5, (toprisk+1.5))) +
  scale_x_continuous(breaks= c(seq(0,toprisk,2))) +
  scale_y_continuous(breaks= c(seq(0.5, 1.0, .1)), limits= c(0.5,1.0)) +
  xlab(NULL) +
  theme(panel.grid.major.y = element_line(colour="darkgray", size=0.5),
        axis.line.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank())
  pspec= addvertlines(pspec)

# p_3= ggdraw() +
#   draw_plot(p1, 0, 0, 1,.46) +
#   draw_plot(pppv, 0, .46, 1, .26) +
#   draw_plot(psens, 0, .72, 1, .26)
# p_3

pfp= ggplot(tres, aes(threshold, endosav)) +
  ylab("test reduction (%)") +
  geom_point() +
  expand_limits(x=c(-1.5, (toprisk+1.5))) +
  scale_x_continuous(breaks= c(seq(0,toprisk,2))) +
  scale_y_continuous(breaks= c(seq(0, 100, 20)), limits= c(0, 100)) +
  xlab(NULL) +
  theme(panel.grid.major.y = element_line(colour="darkgray", size=0.5),
        axis.text.x=element_blank())
pfp= addvertlines(pfp)

p_4= ggdraw() +
draw_plot(p1, 0, 0, 1,.40) +
draw_plot(pppv, 0, .40, 1, .20) +
draw_plot(psens, 0, .60, 1, .20) +
draw_plot(pspec, 0, .80, 1, .20)
# draw_plot(pfp, 0, .80, 1, .20)
p_4

