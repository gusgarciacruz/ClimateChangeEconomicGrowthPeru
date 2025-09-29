# Paper: Institutional quality, climate change impacts, and regional economic growth. Evidence from Peru
# Authors: Gustavo A. García (Universidad EAFIT, Colombia)
#          Paula Restrepo (The World Bank)
#          Juan Manuel Aristizábal (Universidad de Manizales, Colombia)
# Date: 09/2025

# Script for producing the output of Figure 6.

#Load libraries
library(pacman)
p_load(ggplot2, dineq) 

#Set the current working directory
setwd("")

#Import database
data <- read_excel("data_paper.xlsx")

Figure6 <- data |>
  select(year, GDP_percap) |> 
  dplyr::group_by(year) %>% 
  summarise(cv = sd(GDP_percap)/mean(GDP_percap),
            theil = theil.wtd(GDP_percap))

ggplot(Figure6, aes(x = year)) +
  # CV line (grey dashed, left axis)
  geom_line(aes(y = cv, linetype = "CV"), color = "black", size = 1) +
  # Theil line rescaled (black solid, right axis)
  geom_line(aes(y = theil*3.9, linetype = "Theil"), color = "black", size = 1) +
  
  # Axes
  scale_y_continuous(
    name = "Coefficient of variation (CV)",
    limits = c(0.5, 1),
    breaks = seq(0.5, 1, by = 0.05),
    expand = c(0, 0),
    sec.axis = sec_axis(
      ~./3.9,
      name   = "Theil Index",
      breaks = seq(0.13, 0.25, by = 0.02)
    )
  ) +
  scale_x_continuous(name="Year",
    breaks = seq(min(Figure6$year), max(Figure6$year), 1) # show all years
  ) +
  
  # Legend and line types
  scale_linetype_manual(
    name = NULL,
    values = c("CV" = "solid", "Theil" = "dashed")
  ) +
  scale_size_manual(values = c(10, 20)) +
  theme_bw() + 
  theme(legend.position = c(0.9, 0.9),
        legend.text = element_text(size = 10, margin = margin(l = -0.8, unit = "pt")),
        legend.key.width = unit(0.5, "in"),
        legend.key.spacing.x = unit(0.2, "in")) + canvas(8,6)

ggsave("Figure6.png", 
       width = 8, height = 6, units = "in", dpi = 1000, bg="white")

