

# 1. create the data frame
plants <- data.frame(
  plant = c("plant 1", "plant 2", "plant 3", "plant 4", "plant 5"),
  start_experiment = c(4, 5, 3, 6, 7),
  end_experiment = c(9, 6, 5, 4, 7)
)
plants

# 2. reformat this data frame
plants_long <- data.frame(
  plant = rep(plants$plant, each = 2),
  height = c(plants$start_experiment, plants$end_experiment),
  time = factor(rep(c("start_experiment", "end_experiment"), times = 5))
)
plants_long
# 3. the change in height of each plant 
growth_change <- ((plants$end_experiment - plants$start_experiment) / plants$start_experiment) * 100

# 4. create growth list
growth <- list(
  row_names = plants$plant,                      # [1] 提取植物名稱
  growth_percentage = growth_change,             # [2] 計算生長變化百分比
  growth_table = data.frame(                     # [3] 組合植物名稱和生長百分比
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
