######################
### (1.0.0) dplyr#####
######################
install.packages("tidyverse")
library(dplyr)
data(iris)

### (1.1.0)Summarize#############
sum <- summarise(iris, Mean.width = mean(iris$Sepal.Width))
head(sum)

### (1.2.0)Manipulate#############
### (1.2.1)select
# (1) by column names
selection1 <- dplyr::select(iris, Sepal.Length, Sepal.Width, Petal.Length)
head(selection1) 
# (2) by column range
selection2 <- dplyr::select(iris, Sepal.Length:Petal.Length)
head(selection2, 4)
# (3) by column range number
selection3 <- dplyr::select(iris,c(2:5))
head(selection3)
# Use [-] to hide a particular column
selection4 <- dplyr::select(iris, -Sepal.Length, -Sepal.Width)
head(selection4)

### (1.2.2)filter
# (1) Select setosa species
filtered1 <- filter(iris, Species == "setosa" )
head(filtered1,3)
# (2) Select versicolor species with Sepal width more than 3
filtered2 <- filter(iris, Species == "versicolor", Sepal.Width > 3)
tail(filtered2)

### (1.2.3)mutate <- create new columns 
# (1) To create a column “Greater.Half” which stores a logical vector (T/F)
mutated1 <- mutate(iris, Greater.Half = Sepal.Width > 0.5 * Sepal.Length)
tail(mutated1)
head(mutated1)
table(mutated1$Greater.Half)

### (1.2.4)arrange
# (1) Sepal Width by ascending order
arranged1 <- arrange(iris, Sepal.Width)
head(arranged1)
# (2) Sepal Width by descending order [desc]
arranged2 <- arrange(iris, desc(Sepal.Width))
arranged2 <- arrange(iris, -Sepal.Width)
head(arranged2)

### (1.2.5)group_by
# Mean sepal width by Species
gp <- group_by(iris, Species)
gp.mean <- summarise(gp,Mean.Sepal = mean(Sepal.Width))
gp.mean

### (1.3.0)Pipe operator############
#To select the rows with conditions
iris %>% filter(Species == "setosa",Sepal.Width > 3.8)
iris  %>% 
  group_by(Species) %>% 
  summarise(Mean.Length = mean(Sepal.Length))

#############################
### (2.0.0)tidyr#############
#############################

### (2.1.0)Pivoting#############
### (2.1.1)Pivoting- simple data
library (tidyr)
TW_corals<-read.table('C:/Users/peace/Downloads/tw_corals.txt', header=T, sep='\t', dec='.') 
TW_corals

# (1) pivot_longer
TW_corals_long <- TW_corals %>%
  pivot_longer(Southern_TW:Northern_Is, names_to = "Region", values_to = "Richness")
# TW_corals_long <-TW_corals %>% 
#     pivot_longer(cols = everything(), names_to = "Region", values_to = "Richness") 
TW_corals_long

# (2) pivot_wider
TW_corals_wide <- TW_corals_long %>%
  pivot_wider( names_from = Region, values_from = Richness) 
TW_corals_wide

### (2.1.2)Pivoting- simple data
library (tidyr)
income<-read.table('C:/Users/peace/Downloads/metoo.txt',header=T, sep="\t", dec=".", na.strings = "n/a")
income

# (1) pivot_longer
income_long <- income %>%  pivot_longer(cols = -state, 
                                        names_to = c("gender","work"), 
                                        names_sep = "_", 
                                        values_to = "income")
income_long

# (2) pivot_wider
income_long %>% pivot_wider(names_from = c(gender,work), 
                            values_from = income,
                            names_sep = ".")


### (2.2.0)Splitting
# (2.2.1) Columns
# Let's first create a delimited table
income_long_var <- income %>%  pivot_longer(cols = -1, 
                                            names_to = "var1", 
                                            values_to = "income")
income_long_var

# [separate] Split var1 column into two columns
income_sep <- income_long_var %>%  separate(col = var1, 
                                            sep = "_", 
                                            into = c("gender", "work"))
income_sep

# (2.2.2) Rows
income_long_var %>% separate_rows(var1, sep = "_")


#############################
##### Practice 2.3 ##########
#############################


#(0) read url data
url <- "https://www.dipintothereef.com/uploads/3/7/3/5/37359245/rairuoho.txt"
data <- read.csv(url, header = TRUE, sep = "\t")
data

#(1) In the variable treatment, nutrient is misleading, as it refers to nutrient- enriched water. Replace nutrient with enriched in the dataset. 
install.packages("tidyverse") 
library(dplyr)
library(tidyr)
data_replace <- data %>%
  mutate(treatment = gsub("nutrient", "enriched", treatment))
data_replace

#(2) Reformat the table so that the day is as a single variable (factor) with 6 levels (day3, day4, day5, day6, day7, day8). 
#(3) Another variable length is created to get the length of the grass on the given day.
data_replace_long <- data_replace %>% 
  pivot_longer(cols = day3:day8, #select
               names_to = "day",   #rename
               values_to = "length") %>%  #order
  mutate(day = factor(day, levels = c("day3", "day4", "day5", "day6", "day7", "day8"))) #change to one colume
data_replace_long

#(4) The variables Spatial1 and Spatial2 are merged to obtain the following format for the spatial coordinates of an observation upper_left.
data_replace_long <- data_replace %>% 
  mutate(spatial = paste(spatial1, spatial2, sep = "_")) %>% #merge
  select(-spatial1, -spatial2) %>% #remove
  pivot_longer(cols = day3:day8, 
               names_to = "day",   
               values_to = "length") %>%  
  mutate(day = factor(day, levels = c("day3", "day4", "day5", "day6", "day7", "day8")))
data_replace_long

#(5) Remove the variables row and column.
data_replace_long <- data_replace %>% 
  mutate(spatial = paste(spatial1, spatial2, sep = "_")) %>%
  select(-spatial1, -spatial2) %>%
  select(-row, -column) %>% #remove
  pivot_longer(cols = day3:day8, 
               names_to = "day",   
               values_to = "length") %>%  
  mutate(day = factor(day, levels = c("day3", "day4", "day5", "day6", "day7", "day8")))
data_replace_long





