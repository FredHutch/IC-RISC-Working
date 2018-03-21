#happyplot.R

draw_happyplot= function(numcases) {

x=seq(1,25, length= 25)
y=seq(1,40, length= 40)

happyfield= merge(x, y, by= NULL)
happyfield= rename(happyfield, xcol=x, ycol=y)
happyfield= arrange(happyfield, desc(ycol), xcol)

happyfield= mutate(happyfield, newrow=seq(1:1000))
happyfield= mutate(happyfield, case= ifelse(newrow<=(numcases),0,1))
happyfield= mutate(happyfield, case_shape= ifelse(case==1,21,19))

if(numcases>500){
  mycolors= c("#cc0000", "#cc0000") #red/red
} else if(numcases>100) {
  mycolors= c("#cc0000", "#336600") #red/green
} else if(numcases<.5) {
  mycolors= c("#336600", "#336600") #green/green
}
else {
#  mycolors= c("#cc0000", "#336600") #red/green
  mycolors= c("#cc0000", "black") #red/green
}

p= ggplot(happyfield, aes(xcol, ycol))  +
  geom_point(data= happyfield, mapping= aes(x= xcol, y= ycol, 
                                            color= factor(case)), 
             size=2.8, shape= happyfield$case_shape)  +
  scale_shape_discrete() +
  scale_colour_manual(values = mycolors) +
  theme(legend.position=("none")) +
  labs(list(y= "", x= "")) +
  scale_x_continuous(breaks=NULL) + 
  scale_y_continuous(breaks=NULL)

return(p)
}
