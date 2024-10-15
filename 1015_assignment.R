##########################
####### Graphics_1015 ####
##########################

# 0.0 data
treatment <- c(0.02,1.8, 17.5, 55,75.7, 80)

#1.1 plot dot
plot(treatment,  main = "My plot", sub = "a plot", type = 'p',#'p'dot #'o'dot+line type = 'p',
     xlab = "position", ylab = "treatment", #label name
     las=1,    #[las]x-label argument:1-lining,2-horizontal
     pch=20, cex=1 #[pch] point shape [cex] point size
      ) 

#1.2 plot line
plot(treatment,  main = "My plot", sub = "a plot", #'p'dot #'o'dot+line type = 'p',
     xlab = "position", ylab = "treatment", #label name
     las=1,    #[las]x-label argument:1-lining,2-horizontal
     type="l", lty=1, lwd=2, #[lty] line shape [lwd] line weight
     col="blue"
     )

#2.0 [par] multiplot in same windows
par(mfrow = c(1,2))
plot(treatment, type="p", cex=1, pch=20, col="Red")
plot(treatment, type="p", cex=1, pch=20, col='#87736f')
dev.off()  #terminated

#2.1 [lines] multiple treatment in one fig
treatment <- c(0.02, 1.8, 17.5, 55,75.7, 80)
control <- c(0, 20, 40, 60, 80, 100)
plot(control,treatment)
       #---> no function
treatment <- c(0.02, 1.8, 17.5, 55,75.7, 80)
control <- c(0, 20, 40, 60, 80, 100)
plot(treatment, type="o", col= "blue")
lines(control, type="o", pch=22, lty=2, col="red")

#2.2 [range] limit range
#(1)
g_range <- range(0, treatment, control)
g_range
plot(treatment, ylim=g_range)
#(2)
plot(treatment, ylim=c(0,100))

#2.3 [FALSE] comstome x/y lable
plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))

#2.4 [box] freming plot
plot(treatment, type="o", col="blue", ylim=g_range, axes=FALSE, ann=FALSE)
axis(side=1, at=1:6, lab=c("Mon","Tue","Wed","Thu","Fri","Sat"))
axis(2, las=1, at=seq(0,g_range[2],by=20))
box() #

#2.5 [legend] 
legend("topleft",legend=c("treatment","control"), col=c("blue","red"), pch=21:22, lty=1:2) 

#3.1 [hist] histograms
hist(treatment, col="lightblue", ylim=c(0,5),cex.main=0.8) #[cex.main] main size

#4.1 [dotchart] dotchart
data<-data.frame(treatment, control)
row.names(data)<-c("Mon","Tue","Wed","Thu","Fri","Sat")
data
t(data) #[t()] rotation table
dotchart(as.matrix(t(data)))

#5.1 [boxplot] box plot
?ToothGrowth
xaxs='i'#remove margion

getwd()



