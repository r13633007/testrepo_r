library()

plant_hight <- read.csv("C:/Users/peace/Downloads/Plant_height.csv", header = TRUE)
mol1_plant <- lm(plant_hight$loght~plant_hight$temp, data = plant_hight)
mol1_plant
summary(mol1_plant)
