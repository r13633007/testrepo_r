
#install packages
install.packages("readxl")
install.packages("ggplot2")
install.packages("maps")
install.packages("dplyr")

library(readxl)
library(ggplot2)
library(maps)
library(dplyr)

#Fig. 1: Global hotspots of N2O fluxes in relation to archaeal nitrifiers across various land use types.
-----------------------------------------------------
  #a
  # data and world map
  data_1a <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "Fig.1a")
head(data_1a)
world_map <- map_data("world")

datav2 <- data_1a %>%
  mutate(N2O_scale = case_when(
    `N2O` >= -10 & `N2O` <= 10 ~ 1,    
    `N2O` > 10 & `N2O` <= 100 ~ 5,    
    `N2O` > 100 & `N2O` <= 500 ~ 8,   
    TRUE ~ 10                        
  ))
head(datav2)

# Plot 
plot_a <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(map_id = region), 
           fill = "gray86", size = 0.5) +
  geom_point(data = datav2, 
             position = position_jitter(h = 7, w = 7), #automatically disperse
             aes(x = Longitude, y = Latitude, shape = datav2$Land_use, color = datav2$`AamoA/nir`, size = datav2$N2O_scale),
             alpha = 0.7, stroke = 2.5 #stroke:thicker edges
  ) +
  scale_shape_manual(values = c(16, 17, 2, 6, 0, 21, 22, 23)) + 
  scale_color_gradientn(colors = c("#1874CD", "olivedrab3", "#EEC900","red"))+
  scale_size_continuous(range = c(2, 8)) +  # This keeps the original size scale
  scale_size_continuous(
    breaks = c(2, 4, 6, 8),  # Use breaks to control which values to display in the legend
    labels = c("-2~10", "10~100", "100~500", ">500")  # Change the legend labels
  ) +
  theme_minimal() +
  labs(
    x = "Longitude", y = "Latitude",
    color = "AamoA/nir",  # Change the color legend title
    shape = "Land use",    # Change the shape legend title
    size = "N2O"           # Change the size legend title
  ) +
  theme(legend.position = "right",
        legend.title = element_text(size = 10, face = "bold"),  # Change legend title size and font
        legend.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5))  # legend
plot_a

ggsave("comA_plot.pdf", plot_a, width = 12, height = 6, dpi = 300)
#--------------------------------------------------------
#b
#data
data_1b <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "Fig1b-d")
head(data_1b)

data_1bv2 <- data_1b  %>%
  mutate(LogN2O = log10(15 + N2O)) # calculate
head(data_1bv2)

# standard error (SE) 
data_1bv3 <- data_1bv2 %>%
  group_by(Latitude) %>%
  summarise(
    mean_LogN2O = mean(LogN2O, na.rm = TRUE),
    se_LogN2O = sd(LogN2O, na.rm = TRUE) / sqrt(n())
  )
summary(data_1bv3)

data_1bv4 <- lm(mean_LogN2O ~ Latitude, data = data_1bv3)
data_1bv4
summary(data_1bv4)
r_squared <- summary(data_1bv4)$r


# Plot
pcPlot_b<- ggplot(data_1bv3, aes(x = Latitude, y = mean_LogN2O)) +
  geom_point(color = "black", size = 3) +  # Scatter plot for means
  geom_errorbar(aes(ymin = mean_LogN2O - se_LogN2O, ymax = mean_LogN2O + se_LogN2O), 
                width = 1, color = "black") +  # Error bars
  labs(
    x = "Latitude", 
    y = expression("Log"[10] * "15 + N"[2] * "O emission"), 
    title = expression("N"[2] * "O emission")
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )+
  scale_x_continuous(
    limits = c(0, 70),      
    breaks = seq(0, 70, 10),
    expand = c(0.01, 0.01)
  )
pcPlot_b
plot_b <- pcPlot_b+
  stat_smooth(method="gam", color = "red")+
  annotate("text", 
           x = 35, y = 3,
           label = paste("r2 = 0.29", "\n p = 5.642e-07"), 
           size = 5, color = "black", hjust = 0)
plot_b

#c
#data
data_1c <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "Fig1b-d")
head(data_1c)

data_1cv2 <- data_1c  %>%
  mutate(LogN2OA = log10(15 + Archaeal_amoA)) # calculate
head(data_1cv2)

# standard error (SE) 
data_1cv3 <- data_1cv2 %>%
  group_by(Latitude) %>%
  summarise(
    mean_LogN2OA = mean(LogN2OA, na.rm = TRUE),
    se_LogN2OA = sd(LogN2OA, na.rm = TRUE) / sqrt(n())
  )
summary(data_1cv3)

data_1cv4 <- lm(mean_LogN2OA ~ Latitude, data = data_1cv3)
data_1cv4
summary(data_1cv4)

# Plot
pcPlot_c <- ggplot(data_1cv3, aes(x = Latitude, y = mean_LogN2OA)) +
  geom_point(color = "black", size = 3) +  # Scatter plot for means
  geom_errorbar(aes(ymin = mean_LogN2OA - se_LogN2OA, ymax = mean_LogN2OA + se_LogN2OA), 
                width = 1, color = "black") +  # Error bars
  labs(
    x = "Latitude", 
    y = expression("Log"[10] * " 15 + Archaeal " * italic(" amoA")),
    title =  expression("Archaeal "* italic("amoA"))
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )+
  scale_x_continuous(
    limits = c(0, 70),      
    breaks = seq(0, 70, 10),
    expand = c(0.01, 0.01)
  )
plot_c <- pcPlot_c+
  stat_smooth(method="lm", color = "red")+
  annotate("text", 
           x = 3, y = 3,
           label = paste("r2 = 0.22", "\n p = 2.528e-05"), 
           size = 5, color = "black", hjust = 0)
plot_c

#d
#data
data_1d <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "Fig1b-d")
head(data_1d)

data_1dv2 <- data_1d  %>%
  mutate(LogN2On = log10(15 + nir)) # calculate
head(data_1dv2)

# standard error (SE) 
data_1dv3 <- data_1dv2 %>%
  group_by(Latitude) %>%
  summarise(
    mean_LogN2On = mean(LogN2On, na.rm = TRUE),
    se_LogN2On = sd(LogN2On, na.rm = TRUE) / sqrt(n())
  )
summary(data_1dv3)

data_1dv4 <- lm(mean_LogN2On ~ Latitude, data = data_1dv3)
data_1dv4
summary(data_1dv4)
r2_value_1 <- summary(data_1dv4)$r.squared



# Plot
pcPlot_d <- ggplot(data_1dv3, aes(x = Latitude, y = mean_LogN2On)) +
  geom_point(color = "black", size = 3) +  # Scatter plot for means
  geom_errorbar(aes(ymin = mean_LogN2On - se_LogN2On, ymax = mean_LogN2On + se_LogN2On), 
                width = 1, color = "black") +  # Error bars
  labs(
    x = "Latitude", 
    y = expression("Log"[10] * " 15 + " * italic("nir")),
    title =  expression("nir( "* italic("nirK + nirS")*")")
  ) +
  theme_classic() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.5)
  )+
  scale_x_continuous(
    limits = c(0, 70),      
    breaks = seq(0, 70, 10),
    expand = c(0.01, 0.01)
  )
pcPlot_d
plot_d <-pcPlot_d +
  stat_smooth(method="lm", color = "red")+
  annotate("text", 
           x = 3, y = 7,
           label = paste("r2 = 0.0002", "\n p = 0.8924"), 
           size = 5, color = "black", hjust = 0)
plot_d

#combine
install.packages("cowplot")
library(cowplot)
combined_plot<-plot_grid(plot_b, plot_c, plot_d, 
                         ncol = 3,   
                         nrow = 1,   
                         rel_widths = c(1, 1, 1),   
)
ggsave("combined_plot.png", combined_plot, width = 12, height = 4, dpi = 300)


#Fig. 2: Global hotspots of N2O fluxes in relation to archaeal nitrifiers across various land use types.
-----------------------------------------------------
  #data
  data_1 <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "EDFig.1")
head(data_1)

data_1v21 <- data_1  %>%
  mutate(Biome_type = ifelse(data_1$Latitude >= -23.5 & data_1$Latitude <= 23.5, "Tropical", "Non-Tropical"))
head(data_1v21)

data_1v2 <- data_1v21  %>%
  mutate(LogN2Oe = log10(15 + N2O)) # calculate
head(data_1v2)
data_1v2

# standard error (SE) 
data_1v3 <- data_1v2 %>%
  group_by(Latitude) %>%
  summarise(
    mean_LogN2Oe = mean(LogN2Oe, na.rm = TRUE),
    se_LogN2Oe = sd(LogN2Oe, na.rm = TRUE) / sqrt(n()),
    Max_temperature_of_warmest_month = first(Max_temperature_of_warmest_month),
    Agricultural_intensity = first(Agricultural_intensity), 
    Land_use = first(Land_use),  
    Biome_type = first(Biome_type)
  ) 
summary(data_1v3)
data_1v3

data_1v3$Land_use[data_1v3$Land_use %in% c("hay_field", "peat_extraction", "Recreation")] <- "other"

data_1v3 <- data_1v3 %>%
  mutate(Land_use = as.factor(Land_use),
         Biome_type = as.factor(Biome_type),
         Agricultural_intensity = as.factor(Agricultural_intensity))

data_1v4 <- lm(mean_LogN2Oe ~ Max_temperature_of_warmest_month, data = data_1v3)
data_1v4
summary(data_1v4)
plot(data_1v4)
r2_value_1 <- summary(data_1v4)$r.squared

# Plot 
pcPlot_1 <- ggplot(data = data_1v3,aes(x = Max_temperature_of_warmest_month, 
                                       y = mean_LogN2Oe,
                                       color = data_1v3$Land_use, 
                                       size = data_1v3$Agricultural_intensity, 
                                       shape = data_1v3$Biome_type)) + 
  geom_point()+
  geom_errorbar(aes(ymin = mean_LogN2Oe - se_LogN2Oe, ymax = mean_LogN2Oe + se_LogN2Oe), 
                width = 0.3, color = "black", size = 0.1) +  # Error bars
  scale_shape_manual(values = c(17, 16)) + 
  scale_color_manual(
    values = c(
      "fen" = "#1874CD", 
      "forest" = "olivedrab3", 
      "others" = "#CCCCCC",
      "arable" = "#FA8072", 
      "pasture" = "paleturquoise3", 
      "bog" = "#CD96CD")
  )+
  scale_size_manual(
    values = c(2, 4, 6, 8)
  )+ 
  theme_classic() +
  labs(x = "Max temperature of warmest month", y = expression("Log"[10] * "15 + N"[2] * "O emission"),
       color = "Land use type",  # Change the color legend title
       shape = "Biome",    # Change the shape legend title
       size = "Land use intensity"           # Change the size legend title
  ) +
  theme(legend.title = element_text(size = 10, face = "bold"),  # Change legend title size and font
        legend.text = element_text(size = 10),
        panel.border = element_rect(color = "black", fill = NA, size = 0.5)) + # legend
  scale_x_continuous(
    limits = c(11, 34),      
    breaks = seq(15, 30, 5),
    expand = c(0.03, 0.03)
  )
pcPlot_1

plot_1 <- pcPlot_1+
  annotate("text", 
           x = 13, y = 2.5,
           label = paste("r2 = 0.2416", "\n p = 0.0003"), 
           size = 5, color = "black", hjust = 0)+
  geom_abline(intercept = coef(data_1v4)[1], 
              slope = coef(data_1v4)[2], 
              color = "black", linetype = "solid", size = 1)
plot_1

ggsave("com_plot.png", plot_1, width = 6, height = 5, dpi = 300)


----------
  # Load necessary libraries
  library(ggplot2)
install.packages("reshape")
library(reshape)
library(viridis)

install.packages("corrplot")
library(corrplot)
library(reshape)
library(RColorBrewer)
data_4 <- read_excel("E:/hpliu/Mikk_dataset/Source_data.xlsx", sheet = "Fig4a")
head(data_4)

numeric_data <- data_4 %>% select(where(is.numeric))
M<-cor(numeric_data)
head(round(M,2))
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=40,
         col=brewer.pal(n=8, name="RdBu"))
plot(corrplot)
library(RColorBrewer)
pdf("com_4.pdf", width = 6, height = 5)
corrplot(M, type="upper", order="hclust", tl.col="black", tl.srt=40,
         col=brewer.pal(n=8, name="RdBu"))
dev.off()

----------
  
tinytex::install_tinytex()
tinytex::tinytex_root()