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


##########################
####### Practice 4.1 ####
##########################

# iris dataset
data("iris")
iris
# look all graphy
pairs (iris[1:4])
?pairs

# plot
plot(iris$Petal.Length, iris$Petal.Width,
     xlab = "Petal length(cm)",
     ylab = "Petal width(cm)",
     main = "Petal Length vs. Petal Width",
     col = c("black", "red", "green")[as.numeric(iris$Species)],
     pch = c(1, 2, 3)[as.numeric(iris$Species)],
     cex = 0.5,
     axes = FALSE ,)
box()

# range in x/y lable
x_range <- range(iris$Petal.Length)
y_range <- range(iris$Petal.Width)

# re-draw axis
axis(1, at = seq(floor(x_range[1]), ceiling(x_range[2]), by = 1), cex.axis = 0.8)
axis(2, at = seq(floor(y_range[1]), ceiling(y_range[2]), by = 0.5), cex.axis = 0.8) 


# legend label
legend("topleft",legend=c("setosa", "versicolor", "virginica"), 
       col=c("black", "red","green"), 
       pch=1:3, cex = 0.5) 

# r.squared line
model <- lm(Petal.Width ~ Petal.Length, data = iris)
r2_value <- summary(model)$r.squared
abline(model, col = "black", lwd = 1)
text(5.5, 0.5, label = paste("R =", round(r2_value, 2)), col = "black", cex=0.7) 

# saving in pdf format
pdf(file = "1015_assignment.pdf", paper = "A4")
dev.off()









