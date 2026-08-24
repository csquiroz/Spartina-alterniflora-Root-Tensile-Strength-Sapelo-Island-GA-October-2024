##Title: Spartina alterniflora Root Tensile Strength Analysis
##Author: Cody S. Quiroz
# Setting Up --------------------------------------------------------------

##install required packages
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
library(readxl)
library(Cairo)
library(patchwork)
library(tidyr)

#read in data
setwd("C:/Users/coquiroz/OneDrive - University of Arkansas/Data/UW-Madison/Sapelo")
data = read_xlsx("20241020_Spartina-alterniflora_Root-Data_Sapelo-Island-GA.xlsx", "Clean_Data")
filt_data = data[data$Break == "Y", ] #Only use data from roots that broke in the middle, excludes roots that broke at the clamp contact point

# Assess for Normality ----------------------------------------------------

#check for normality of TS w/ QQ plot
qq_plot <- ggplot(filt_data, aes(sample = Avg.TS.Nmm2)) +
  stat_qq_line(color = "red", size=1) +      #reference line
  stat_qq(size=1, alpha=0.6) +               #points for the quantiles
  theme_bw() +               
  labs(             
    x = "Theoretical Quantiles",
    y = "Sample Quantiles") +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"))

qq_plot
#ggsave("QQ-TS-Plot.tiff", qq_plot,width = 3.35, height = 3.5, units="in", device= tiff, dpi=1000)

# Cubic Model/ANCOVA ------------------------------------------------------

###ANCOVA for dist and strength
#Fit the ANCOVA model with linear, quadratic, and cubic terms for Distance. Cubic fits data best
model <- lm(Avg.TS.Nmm2 ~ Site + Distance.m + I(Distance.m^2) + I(Distance.m^3), data = filt_data)
summary(model)

###Plot of ANCOVA with obs and avg of obs
#calculate the average of Avg_TS for each combination of Site and Distance
avg_data <- filt_data %>%
  group_by(Site, Distance.m) %>%
  summarize(avg_Avg_TS = mean(Avg.TS.Nmm2), .groups = 'drop')

# Prediction grid
newdata <- filt_data %>%
group_by(Site) %>%
reframe(
  Distance.m = seq(min(Distance.m),
                   max(Distance.m),
                   length.out = 200))

# Predicted values
newdata$predicted <- predict(model, newdata = newdata)

#plot model results with actual data and avgs
ts.model <- ggplot(filt_data, aes(x = Distance.m, y = Avg.TS.Nmm2, color = Site)) +
  geom_point(alpha = 0.65, size = 1.75, shape=16) +
  geom_line(
    data = newdata,
    aes(x = Distance.m, y = predicted, color = Site),
    size = 1, alpha=0.8)+
  geom_point(
    data = avg_data,
    aes(x = Distance.m, y = avg_Avg_TS, fill = Site),
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
        legend.text = element_text(size = 12, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

ts.model
#ggsave("TS-Dist-Model.tiff", ts.model, width = 6.31, height = 4.5, units="in", device= tiff, dpi=1000)

# Avg TS at Each Site/Distance --------------------------------------------

##Average values at distance at each site
avg_data = filt_data %>%
  group_by(Site, Distance.m) %>%
  summarize(Avg_TS = mean(Avg.TS.Nmm2, na.rm = TRUE), .groups = "drop")

#Plot Avg TS vs Distance each individual site
TSDistSite <- ggplot(avg_data, aes(x = Distance.m, y = Avg_TS, group = Site)) +
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
    strip.text = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
  facet_wrap(~ Site, scales = "free") +
  scale_y_continuous(limits = c(0, 15))

TSDistSite
#ggsave("Avg-TS-Dist-Site.tiff", TSDistSite, width = 6.31, height = 4.5, units="in", device= tiff, dpi=1000)

# Linear Regressions with TS ----------------------------------------------

##Height vs TS
cor.test(filt_data$Height.cm, filt_data$Avg.TS.Nmm2, method = "pearson")
cor.test(filt_data$Length.mm, filt_data$Avg.TS.Nmm2, method = "pearson")
cor.test(filt_data$Diameter.mm, filt_data$Avg.TS.Nmm2, method = "pearson")

##Linear regression of TS vs Height
lm_model = lm(Avg.TS.Nmm2 ~ Height.cm, data = filt_data)
summary(lm_model)

TSHeight <- ggplot(filt_data, aes(x = Height.cm, y = Avg.TS.Nmm2)) +
  geom_point(size = 1, alpha=0.8) +                       
  geom_smooth(method = "lm", color = "red", se = T) +  # Linear regression line
  labs(title = "A",
       x = "Above Ground Height (cm)",
       y = as.expression(Tensile~Strength~(N~mm^-2))) +
  theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

TSHeight
#ggsave("TS-Height-lm.pdf", TSHeight, width = 8, height = 6, device= pdf)

#Plot of height vs distance at each site
ggplot(filt_data, aes(x = Distance.m, y = Height.cm, group = Site)) +
  geom_line(aes(color = Site), size = 1) +
  geom_point(size = 2) +  
  labs(title = "Height vs Distance",
       x = "Distance (m)", 
       y = "Above Ground Height (cm)") + 
  facet_wrap(~ Site, scales = "free") +  #facet by Site to show each separately
  theme_minimal()

##TS vs Root Length Pearson correlation
cor.test(filt_data$Length.mm, filt_data$Avg.TS.Nmm2, method = "pearson")

#Linear regression of TS vs Root Length
lm_model_length = lm(Avg.TS.Nmm2 ~ Length.mm, data = filt_data)
summary(lm_model_length)

#plot
LengthTS <- ggplot(filt_data, aes(x = Length.mm, y = Avg.TS.Nmm2)) +
  geom_point(size = 1, alpha=0.8) +                       
  labs(title = "C",
       x = "Root Length (mm)",
       y="")+
  theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

LengthTS
#ggsave("TS-Root-Length.tiff", LengthTS, width = 8, height = 6, device= tiff)

#root diameter vs avg strength
lm_model_diameter <- lm(Avg.TS.Nmm2 ~ Diameter.mm, data = filt_data)
summary_diam <- summary(lm_model_diameter)
summary_diam

rootdiam.ts <- ggplot(filt_data, aes(x = Diameter.mm, y = Avg.TS.Nmm2)) +
  geom_point(size = 1, alpha=0.8, shape=16) +                       
  geom_smooth(method = "lm", color = "red", se = T) +  # Linear regression line
  labs(title = "B",
       x = "Root Diameter (mm)",
       y = "") + theme_bw() +
  theme(axis.title=element_text(size = 12, color="black"),
        axis.text = element_text(size = 12, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(
          size = 12,
          color = "black",
          face = "bold"
        ))

rootdiam.ts
#ggsave("TS-Diameter-lm.tiff", rootdiam.ts, width = 8, height = 6, units="in", device=tiff, dpi=1000)

#patchwork linear regression plots together
(TSHeight) | (rootdiam.ts) | (LengthTS)

#save patchwroked plot
#ggsave("lm-figure.tiff", width = 6.85, height = 3.5, units="in", device= tiff, dpi=1000)

# Elevation data analysis -------------------------------------------------

#read in elevation data
elev.prof = read_xlsx("20241020_Spartina-alterniflora_Root-Data_Sapelo-Island-GA.xlsx", "Elevation")

##plot elevation profiles
elev.prof.plots <- elev.prof %>%
  filter(Distance.m <= 30) %>%
  ggplot(aes(x = Distance.m, y = elevation.m)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  facet_wrap(~ Site, scales = "free_y") +
  scale_x_continuous(
    breaks = seq(0, 30, by = 5),
    limits = c(0, 30)
  ) +
  labs(
    x = "Distance from Creek (m)",
    y = "Elevation (m)",
    title = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 24),
    axis.title.y = element_text(size = 24),
    axis.text.x = element_text(size = 18, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

elev.prof.plots
#ggsave("Sapelo-Elevation-Profiles.pdf", elev.prof.plots, width = 12, height = 8)

#avg TS by site and dist
ts_data <- avg_data %>%
  rename(value = Avg_TS) %>%
  mutate(variable = "Tensile Strength")

#elevation data
elev_data <- elev.prof %>%
  select(Site, Distance.m, elevation.m) %>%
  rename(value = elevation.m) %>%
  mutate(variable = "Elevation")

#combine and standardize w/in site for visual comparison of trends
plot_data <- bind_rows(elev_data, ts_data) %>%
  group_by(Site, variable) %>%
  mutate(
    scaled_value = (value - min(value, na.rm = TRUE)) /
      (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))
  ) %>%
  ungroup()

#plot relative values to visualize trends (do TS and elevation peak around the same places?)
elev.ts.plots <- ggplot(
  plot_data,
  aes(
    x = Distance.m,
    y = scaled_value,
    color = variable
  )
) +
  geom_line(linewidth = 0.75, alpha=0.8) +
  geom_point(size = 1,alpha=0.8) +
  facet_wrap(~Site) +
  coord_cartesian(xlim = c(0, 30)) +
  scale_x_continuous(breaks = seq(0, 30, by = 10)) +
  scale_color_manual(
    values = c(
      "Elevation" = "black",
      "Tensile Strength" = "#6B8E23"
    )
  ) +
  labs(
    x = "Distance from Creek (m)",
    y = "Relative Value (0-1)",
    color = NULL
  ) +
  theme_bw() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size =11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.text = element_text(size = 10, color = "black"),
    legend.box.margin = margin(t = -5),
    legend.margin = margin(t = -5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
elev.ts.plots
#ggsave("Sapelo-TS-Elevation-Profiles.tiff", elev.ts.plots, width = 6.85, height = 3.75, units="in", device= tiff, dpi=1000)

# Spearman correlations for elevation and TS profiles to evaluate  --------

#create 2.5 elevation by averaging 2 and 3 m elevation. Allows for 6 paired obs
elev_2.5 <- elev.prof %>%
  filter(Distance.m %in% c(2, 3)) %>%
  group_by(Site) %>%
  summarise(
    Distance.m = 2.5,
    elevation.m = mean(elevation.m, na.rm = TRUE),
    .groups = "drop"
  )
elev_2.5

#bind, join, and keep only needed rows data
elev.prof2 <- bind_rows(elev.prof, elev_2.5)

dat <- left_join(
  elev.prof2,
  avg_data,
  by = c("Site", "Distance.m"))

dat2 <- dat %>%
  filter(!is.na(Avg_TS))

#ensure there are 6 pairs for each transect
dat2 %>%
  count(Site)

#Spearman correlations
site_corrs <- dat2 %>%
  group_by(Site) %>%
  group_modify(~{
    test <- cor.test(
      .x$elevation.m,
      .x$Avg_TS,
      method = "spearman",
      exact = FALSE
    )
    tibble(
      n = nrow(.x),
      rho = unname(test$estimate),
      p.value = test$p.value
    )
  }) %>%
  ungroup()

#Spearman results
site_corrs
