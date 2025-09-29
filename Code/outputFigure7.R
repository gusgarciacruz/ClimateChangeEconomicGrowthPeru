# Paper: Institutional quality, climate change impacts, and regional economic growth. Evidence from Peru
# Authors: Gustavo A. García (Universidad EAFIT, Colombia)
#          Paula Restrepo (The World Bank)
#          Juan Manuel Aristizábal (Universidad de Manizales, Colombia)
# Date: 09/2025

# Script for producing the output of Figure 7.

#Load libraries
library(pacman)
p_load(sp, splm, plm, spdep, nlme, tidyverse, summarytools, 
       Hmisc, readxl, modelsummary, statar, ggview, margins) 

#Set the current working directory
setwd("")

data <- read_excel("data_paper.xlsx") |>
  mutate(IQPA_quintile = ntile(IQPA, n = 5),
         efficiency_quintile = ntile(efficiency, n = 5),
         capacity_quintile = ntile(capacity, n = 5),
         internet_ratio=internet_ratio+0.00001)|> 
  arrange(dpto)

datapanel <- pdata.frame(data, c("dpto","year")) 

Fe <- plm(growth_gdp_pc ~ gdp_pc_t_1 +
             IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
             precipitation + I(precipitation^2) +
             temperature + I(temperature^2) +      
             log(secondary_ratio)+
             log(internet_ratio) +
             log(density),
           data = datapanel, 
           model = "within")
summary(Fe)

ols <- lm(growth_gdp_pc ~ gdp_pc_t_1 +
            IQPA_quintile + I(IQPA_quintile*gdp_pc_t_1) +
            precipitation + I(precipitation^2) +
            temperature + I(temperature^2) +      
            log(secondary_ratio)+
            log(internet_ratio) +
            log(density) + factor(dpto),
          data = data)
summary(ols)
coefs <- coef(ols)

# Inflection point taking the coefficients of the SAR model (see Column 4 in Table 5) = 22.75
-0.091/(2*(-0.002))

# Figure 7
newdata <- data.frame(dpto = c(rep(1, 30)),
                      year = c(2000:2029),
                      gdp_pc_t_1=mean(data$gdp_pc_t_1),
                      IQPA_quintile   = mean(data$IQPA_quintile),
                      precipitation   = mean(data$precipitation),
                      secondary_ratio = mean(data$secondary_ratio),
                      internet_ratio  = mean(data$internet_ratio),
                      density         = mean(data$density),
                      temperature     = seq(from = min(data$temperature),
                                        to = max(data$temperature), 
                                        length.out = 30))

x<-predict(ols, 
           newdata, 
           interval = "confidence",
           level=0.9)

p <- data.frame(temperature = seq(from = min(data$temperature), 
                         to = max(data$temperature), 
                 length.out = 30))

dpto_coefs <- c(coefs["(Intercept)"],coefs[grep("^factor\\(dpto\\)", names(coefs))])
mean(dpto_coefs)

p$p <- mean(dpto_coefs) +
  coefs["gdp_pc_t_1"] * mean(data$gdp_pc_t_1) +
  coefs["IQPA_quintile"] * mean(data$IQPA_quintile) +
  coefs["I(IQPA_quintile * gdp_pc_t_1)"] * mean(data$IQPA_quintile) * mean(data$gdp_pc_t_1) +
  coefs["precipitation"] * mean(data$precipitation) +
  coefs["I(precipitation^2)"] * (mean(data$precipitation)^2) +
  coefs["temperature"] * p$temperature +
  coefs["I(temperature^2)"] * (p$temperature^2) +
  coefs["log(secondary_ratio)"] * log(mean(data$secondary_ratio)) +
  coefs["log(internet_ratio)"] * log(mean(data$internet_ratio)) +
  coefs["log(density)"] * log(mean(data$density))

p <- cbind(p,predict=x[1:30,1]+(mean(dpto_coefs)-coefs["(Intercept)"]),
           ic_lb=x[1:30,2]+(mean(dpto_coefs)-coefs["(Intercept)"]),
           ic_ub=x[1:30,3]+(mean(dpto_coefs)-coefs["(Intercept)"]))

ggplot(p) +
  geom_line(aes(x = temperature, y = p), colour="blue")+
  geom_line(aes(x = temperature, y = ic_lb), colour="red", linetype = "dashed", alpha=0.3) +
  geom_line(aes(x = temperature, y = ic_ub), colour="red", linetype = "dashed", alpha=0.3) +
  geom_text(x=24, y=.95, label="Inflection point \n 22.75 °C", 
            size=3, color="black") +
  geom_vline(xintercept=22.75, linetype="dashed", color = "black")+
  scale_y_continuous(
    name = "Prediction of per capita GDP growth rate",
    limits = c(-0.4, 1.5),
    breaks = c(-0.50, -0.25, 0, 0.25, 0.5, 0.75, 1, 1.25, 1.5), # Manually set axis breaks
    expand = c(0, 0)
  ) + 
  scale_x_continuous(
    name = "Temperature (°C)",
    limits = c(7, 28),
    breaks = c(8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28), # Manually set axis breaks
    expand = c(0, 0)
  ) +
  theme_bw() + canvas(8,6)

ggsave("Figure7.png", 
       width = 8, height = 6, units = "in", dpi = 1000, bg="white")

