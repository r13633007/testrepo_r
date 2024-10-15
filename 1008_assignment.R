#########################
#####Practice 3.2########
#########################

#0. library
library(tidyverse)

# 1. my data frame
plants <- data.frame(
  plant = c("plant 1", "plant 2", "plant 3", "plant 4", "plant 5"),
  start_experiment = c(4, 5, 3, 6, 7),
  end_experiment = c(9, 6, 5, 4, 7)
)
plants

# 2. reformat this data frame
#(1)
plants_long <- data.frame(
  plant = rep(plants$plant, each = 2),
  height = c(plants$start_experiment, plants$end_experiment),
  time = factor(rep(c("start_experiment", "end_experiment"), times = 5))
)
plants_long

#(2)
my.df2 <- plants %>%
  tibble::rownames_to_column(var = "plant") %>%
  pivot_longer(start_experiment::end_experiment, names_to = "time", values_to = "height")

# 3. the change in height of each plant 
growth_change <- ((plants$end_experiment - plants$start_experiment) / plants$start_experiment) * 100

# 4. create growth list
growth <- list(
  row_names = plants$plant,                      # [1] plant name
  growth_percentage = growth_change,             # [2] change percentage
  growth_table = data.frame(                     # [3] conbine
    plants = plants$plant,
    growth = growth_change
  )
)
growth

# 5. arbitrary message
message <- "There is an arbitrary message about the growth status of these plants."

# 6. create my.experiment list
my.experiment <- list(
  experiment_data = plants_long,              
  growth = growth,                               
  message = message                              
)
my.experiment


-------------
plants <- c('plant 1','plant 2','plant 3','plant 4','plant 5')
time.exp<- c('start_experiment', 'end_experiment')
height<- c(4,5,3,6,7,9,6,5,4,7)
my.matrix<-matrix(height,5,2)
my.table<-data.frame (my.matrix, row.names=plants)
colnames(my.table)<-time.exp
my.table

------------
##############################
#######1015_teacher##########
##############################

#(0) 
installed.packages(tidyverse)
library(tidyverse)
#(1)
my.df1 <- data.frame(
  plant = c("plant 1", "plant 2", "plant 3", "plant 4", "plant 5"),
  start_experiment = c(4, 5, 3, 6, 7),
  end_experiment = c(9, 6, 5, 4, 7)
)
my.df1

#(2)
my.df2 <- my.df1 %>%
  tibble::rownames_to_column(var = "plant") %>%
  pivot_longer(start_experiment::end_experiment, names_to = "time", values_to = "height")

#(3) create my list "my.expeiment"
my.experiment <- NULL
my.experiment[[1]] <- my.df2
my.experiment

#(4) 
growth<- NULL
growth[[1]]<- rownames(my.df2)
growth[[2]]<- (plants$end_experiment - plants$start_experiment) / plants$start_experiment*100
growth[[3]]<- data.frame(plant=row.names(my.df1). growth=growth[[2]]

my.experiment[[2]] <- growth

names(my.experiment)[[2]]<-'growth'

my.experiment[[3]]<- 'growth is not consistent acrose plants'

#(5) plot
barplot(my.experiment$growth[[3]]$growth, names.arg = my.experiment$growth[[3]]$plant)

