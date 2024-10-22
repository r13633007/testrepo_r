##########################
####### Graphics_1022 ####
##########################

# dataset
data("iris")
iris

# library
library(ggplot2)

# 1.ggplot
# (1)
ggiris <- ggplot(data=iris)
class(ggiris)

ggiris$data[1:4,]

ggiris <- ggplot(data=iris)
ggiris2 <- ggiris+aes(x=Petal.Length,y=Petal.Width)
ggiris2$mapping
ggiris3 <- ggiris2+geom_point()
ggiris3$mapping
ggiris3
# (2) [geom_point()] 
ggiris3 <- ggplot(data=iris,
                  mapping=aes(x=Petal.Length,y=Petal.Width))
ggiris3+geom_point()

# 2.[geom_line()] Line plots
ggiris_line<- ggiris3 + geom_line()
ggiris_line

# 3.[geom_smooth()] smooth line plots
ggiris_smooth<- ggiris3 + geom_smooth()
ggiris_smooth

# 4.[geom_bar()] Bar plots
ggiris_bar<- ggplot(data=iris, mapping=aes(x=Species))
ggiris_bar2<- ggiris_bar + geom_bar()
ggiris_bar2

# 5.[geom_histogram()] Histograms
ggiris_hist <- ggplot(data=iris,
                      mapping=aes(x=Petal.Length))
ggiris_hist2 <- ggiris_hist + geom_histogram() 
ggiris_hist2

