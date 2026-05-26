##title: Spartina alterniflora Root Tensile Strength Analysis
##author: Cody S. Quiroz

#install packages
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("xlsx")
#install.packages("Cairo")
#install.packages("patchwork")

#load packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(xlsx)
library(Cairo)
library(patchwork)

#read in data
data = read_excel("YourData.xlsx", "SheetName")
filt_data = data[data$Break == "Y", ] #Only use data from roots that broke in the middle, excludes roots that broke at the clamp contact point

#check for normality of TS w/ QQ plot
qq_plot <- ggplot(filt_data, aes(sample = Avg.TS)) +
  stat_qq_line(color = "red", size=1) +      #reference line
  stat_qq(size=1, alpha=0.6) +               #points for the quantiles
  theme_bw() +               
  labs(             
    x = "Theoretical Quantiles",
    y = "Sample Quantiles") +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"))

qq_plot

ggsave("SFIG1.tiff", qq_plot,width = 3.35, height = 3.5, units="in", device= tiff, dpi=1000)

###ANCOVA for dist and strength
#Fit the ANCOVA model with linear, quadratic, and cubic terms for Distance. Cubic fits data best
model <- lm(Avg.TS ~ Site + Distance + I(Distance^2) + I(Distance^3), data = filt_data)

#Display the model summary
summary(model)

###Plot of ANCOVA with obs and avg of obs
#calculate the average of Avg_TS for each combination of Site and Distance
avg_data <- filt_data %>%
  group_by(Site, Distance) %>%
  summarize(avg_Avg_TS = mean(Avg.TS), .groups = 'drop')

#create predicted values from the model
filt_data$predicted <- predict(model, newdata = filt_data)

#extracting predicted values
newdata <- filt_data %>%
  group_by(Site) %>%
  summarize(
    Distance = seq(min(Distance), max(Distance), length.out = 200)
  )

#plot model results with actual data and avgs
ts.model <- ggplot(filt_data, aes(x = Distance, y = Avg.TS, color = Site)) +
  geom_point(alpha = 0.65, size = 1.75, shape=16) +
  geom_line(
    data = newdata,
    aes(x = Distance, y = predicted, color = Site),
    size = 1, alpha=0.8)+
  geom_point(
    data = avg_data,
    aes(x = Distance, y = avg_Avg_TS, fill = Site),
    size = 2.5,
    alpha = 0.8,
    shape = 21,
    color = "black",
    stroke = 0.5) +
  scale_color_manual(values = c( #color blind friendly color scheme
    "#FF934F", "#E1DA9E", "#CC2D35",
    "#2D3142", "#848FA2", "#058ED9")) +
  scale_fill_manual(values = c(
    "#FF934F", "#E1DA9E", "#CC2D35",
    "#2D3142", "#848FA2", "#058ED9")) +
  labs(x = "Distance (m)", 
       y = as.expression(Tensile~Strength~(N~mm^-2))) +
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.title = element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        legend.text = element_text(size = 12, color="black"))

ts.model

#save plot
ggsave("FIG2.tiff", ts.model, width = 6.31, height = 4.5, units="in", device= tiff, dpi=1000)

##Average values at distance at each site
avg_data = filt_data %>%
  group_by(Site, Distance) %>%
  summarize(Avg_TS = mean(Avg.TS, na.rm = TRUE), .groups = "drop")

#Plot Avg TS vs Distance each individual site
TSDistSite <- ggplot(avg_data, aes(x = Distance, y = Avg_TS, group = Site)) +
  geom_point(size = 1.5) +
  geom_line(size = 1) +
  labs(
    x = "Distance (m)",
    y = as.expression(Tensile~Strength~(N~mm^-2))
  ) +
  theme_bw() +
  theme(
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_text(size = 12, color = "black"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  facet_wrap(~ Site, scales = "free") +
  scale_y_continuous(limits = c(0, 15))

TSDistSite

#save plot
ggsave("SFIG2.tiff", TSDistSite, width = 6.31, height = 4.5, units="in", device= tiff, dpi=1000)

##Height vs TS
cor.test(filt_data$Height, filt_data$Avg.TS, method = "pearson")
cor.test(filt_data$Length, filt_data$Avg.TS, method = "pearson")
cor.test(filt_data$Diameter, filt_data$Avg.TS, method = "pearson")

##Linear regression of TS vs Height
lm_model = lm(Avg.TS ~ Height, data = filt_data)
summary(lm_model)

TSHeight <- ggplot(filt_data, aes(x = Height, y = Avg.TS)) +
  geom_point(size = 1, alpha=0.8) +                       
  geom_smooth(method = "lm", color = "red", se = T) +  # Linear regression line
  labs(title = "A",
       x = "Above Ground Height (cm)",
       y = as.expression(Tensile~Strength~(N~mm^-2))) +
  theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

TSHeight

#save plot
ggsave("TSvsHeight.pdf", TSHeight, width = 8, height = 6, device= pdf)

#view R squared and p-value for height in linear model
r_squared = summary(lm_model)$r.squared 
p_val <- summary(lm_model)$coefficients["Height", "Pr(>|t|)"]
p_val

#Plot of height vs distance at each site
ggplot(filt_data, aes(x = Distance, y = Height, group = Site)) +
  geom_line(aes(color = Site), size = 1) +
  geom_point(size = 2) +  
  labs(title = "Height vs Distance",
       x = "Distance (m)", 
       y = "Above Ground Height (cm)") + 
  facet_wrap(~ Site, scales = "free") +  #facet by Site to show each separately
  theme_minimal()

##TS vs Root Length pearson correlation
cor.test(filt_data$Length, filt_data$Avg.TS, method = "pearson")

#Linear regression of TS vs Root Length
lm_model_length = lm(Avg.TS ~ Length, data = filt_data)
summary(lm_model_length)

LengthTS <- ggplot(filt_data, aes(x = Length, y = Avg.TS)) +
  geom_point(size = 1, alpha=0.8) +                       
  geom_smooth(method = "lm", color = "red", se = T) + 
  labs(title = "C",
       x = "Root Length (mm)",
       y="")+
  theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

LengthTS

#save plot
ggsave("SFIG3.tiff", LengthTS, width = 8, height = 6, device= tiff)

#pull out r squared and p-value for length in linear model
r_squared_length = summary(lm_model_length)$r.squared 
r_squared_length
p_value_length <- summary(lm_model_length)$coefficients["Length", "Pr(>|t|)"]
p_value_length

#root diameter vs avg strength
lm_model_diameter <- lm(Avg.TS ~ Diameter, data = filt_data)
summary_diam <- summary(lm_model_diameter)

rootdiam.ts <- ggplot(filt_data, aes(x = Diameter, y = Avg.TS)) +
  geom_point(size = 1, alpha=0.8, shape=16) +                       
  geom_smooth(method = "lm", color = "red", se = T) +  # Linear regression line
  labs(title = "B",
       x = "Root Diameter (mm)",
       y = "") + theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

rootdiam.ts

#save plot
ggsave("TSvsDiameter.tiff", rootdiam.ts, width = 8, height = 6, units="in", device=tiff, dpi=1000)

#pull out r squared and p-value for diameter in linear model
r_squared_diam <- summary_diam$r.squared
p_value_diam <- summary_diam$coefficients["Diameter", "Pr(>|t|)"]
r_squared_diam
p_value_diam

#patchwork linear regression plots together
(TSHeight) | (rootdiam.ts) | (LengthTS)

#save patchwroked plot
ggsave("FIG3.tiff", width = 6.85, height = 3.5, units="in", device= tiff, dpi=1000)
