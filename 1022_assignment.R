##########################
####### Graphics_1022 ####
##########################

# dataset
data("iris")
iris

# library
library(ggplot2)

### 1.0 geom
# 1.1 [geom_point()] ggplot
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
# (2)  
ggiris3 <- ggplot(data=iris,
                  mapping=aes(x=Petal.Length,y=Petal.Width))
ggiris3+geom_point()

# 1.2 [geom_line()] Line plots
ggiris_line<- ggiris3 + geom_line()
ggiris_line

# 1.3 [geom_smooth()] smooth line plots
ggiris_smooth<- ggiris3 + geom_smooth()
ggiris_smooth

# 1.4 [geom_bar()] Bar plots
ggiris_bar<- ggplot(data=iris, mapping=aes(x=Species))
ggiris_bar2<- ggiris_bar + geom_bar() #
ggiris_bar2

# 1.5 [geom_histogram()] Histograms
ggiris_hist <- ggplot(data=iris,
                      mapping=aes(x=Petal.Length))
ggiris_hist2 <- ggiris_hist + geom_histogram() #
ggiris_hist2

### 2.0 Aesthetics
# (1)
ggiris_scatter2 <- ggplot(data=iris,
                          mapping=aes(x=Petal.Length,y=Petal.Width,color=Species, shape=Species)) #color, shape
ggiris_scatter2 + geom_point()
# (2)
ggiris_scatter4 <- ggplot(data=iris)
ggiris_scatter4 + geom_point(aes(x=Petal.Length,y=Petal.Width,color=Species, shape=Species))

### 3.0 Faceting
# 3.1 [facet_grid] 
patients_clean<-read.delim('data/patients_clean.txt')
pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight, 
                                         colour=Sex))+geom_point()
pcPlot + facet_grid(Smokes~Sex) #(1)
pcPlot + facet_grid(~Sex) #(2)
pcPlot + facet_grid(Sex~.) #(3)

# 3.2 [facet_wrap]
patients_clean<-read.delim('data/patients_clean.txt')
pcPlot <- ggplot(data=patients_clean,aes(x=Height,y=Weight,
                                         colour=Sex))+geom_point()
pcPlot + facet_wrap(~Smokes) #(1)
pcPlot + facet_wrap(~Pet+Smokes+Sex) #(2) individule
pcPlot + facet_grid(Smokes~Sex+Pet) #(3)

### 4.0 Plotting order
# 4.1 [boxplot()]
getwd()
patients_clean<-read.delim("C:/Users/peace/Downloads/patients_clean.txt")
patients_clean$Sex <- factor(patients_clean$Sex, 
                             levels=c("Male","Female"))
ggplot(patients_clean,aes(x=Sex, y=Weight)) + geom_boxplot()

pcPlot + geom_point() + facet_grid(Smokes~Sex)+
  scale_x_continuous(name="height ('cm')",
                     limits = c(100,200),
                     breaks=c(125,150,175),
                     labels=c("small","justright","tall"))

##########################
####### Practice 4.2 #####
##########################

# read rairuoho dataset 
url <- "https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt"
data <- read.csv(url, header = TRUE, sep = "\t")
data

# install pakage
install.packages("tidyverse") 
library(dplyr)
library(tidyr)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

# choice the data
data_select1 <- data %>%
  select(-spatial1, -spatial2, -row, -column) %>% #remove
  mutate(growth = day8 - day3) #caculate growth

data_select2 <- data_select1 %>%
  select(-day3, -day4, -day5, -day6, -day7, -day8) %>% #remove
  arrange(desc(treatment == "water"), growth) #group
data_select2

# ggplot
p <- ggplot(data_select2, aes(x = treatment, y = growth, fill = treatment)) +
  geom_boxplot(alpha = 0.7, width = 0.5, outlier.size = 1.5, outlier.shape = 16) + 
  geom_jitter(width = 0.15, size = 1.0, color = "black", shape = 16) +  
  labs(
    title = "Grass Growth under Nutrient and Water Treatments",
    x = "Treatment Type",
    y = "Grass Growth (cm)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.31, size = 10),  
    axis.title = element_text(face = "bold", size = 10),               
    axis.text = element_text(size = 9, color = "black"),              
    panel.grid.major = element_blank(),                               
    panel.grid.minor = element_blank(),                                
    panel.border = element_rect(color = "black", fill = NA, size = 1),  
    legend.text = element_text(size = 10, color = "black"),           
    legend.title = element_text(face = "bold", size = 10, color = "black")
  ) +
  scale_fill_manual(values = c("nutrient" = "#2a9d8f", "water" = "#e76f51"))
p

#  ANOVA
anova_results <- aov(growth ~ treatment, data = data_select2)
summary(anova_results)

# TukeyHSD
posthoc_results <- TukeyHSD(anova_results)
print(posthoc_results)

# statistical results
p1<- p + 
  geom_segment(aes(x = 1, xend = 2, y = max(data_select2$growth) + 5, yend = max(data_select2$growth) + 5),
               color = "black", size = 0.6) +  # line
  geom_text(aes(x = 1.5, y = max(data_select2$growth) + 6, 
                label = "***"), size = 4, color = "black") # annotation
# Saving plot 
ggsave("grass_growth_analysis.pdf", plot = last_plot(), 
       width = 9, height = 9, units = "cm", dpi = 300)
