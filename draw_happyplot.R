#happyplot.R

draw_happyplot= function(numcases) {

x=seq(1,25, length= 25)
y=seq(1,40, length= 40)

happyfield= merge(x, y, by= NULL)
happyfield= dplyr::rename(happyfield, xcol=x, ycol=y)
happyfield= arrange(happyfield, desc(ycol), xcol) #descending order for rows

happyfield= mutate(happyfield, newrow=seq(1:1000))  #begins at x,y= 1,40
happyfield= mutate(happyfield, case= ifelse(newrow>(numcases), 1, 0))  #cases are coded 0; non-cases= 1
happyfield= mutate(happyfield, case_shape= ifelse(case==1, 21, 19))

# View(happyfield)

if(numcases>0) {
  mycolors= c("#cc0000", "black")
  } else {
    mycolors= c("black", "black")
  }
# mycolors= c("#cc0000", "black")
# View((mycolors))
p= ggplot(happyfield, aes(xcol, ycol))  +
  geom_point(data= happyfield, mapping= aes(x= xcol, y= ycol, 
                                            color= factor(case)),
             size= 3.0, shape= happyfield$case_shape)  +
  scale_shape_discrete() +
  scale_colour_manual(values = mycolors) +
  theme(legend.position=("none")) +
  labs(y= NULL, x= NULL) +
  # labs(x= NULL) +
  scale_x_continuous(breaks=NULL) + 
  scale_y_continuous(breaks=NULL)

return(p)
}
