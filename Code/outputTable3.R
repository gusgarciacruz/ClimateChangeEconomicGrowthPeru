#Institutional quality, climate change impacts, and regional economic growth
#Evidence from Peru

#García, Restrepo, Aristizábal (2025)

#Script for producing the output of Table 3.

#Load libraries
library(pacman)
p_load(sp, splm, plm, spdep, nlme, tidyverse, summarytools, 
       Hmisc, readxl, modelsummary, statar, writexl,tibble, dineq) 

##Set the current working directory
setwd("")


#filtering the dataset to the years 2004–2019 while creating 
#quintile categories for IQPA, efficiency, and capacity, and arranging the results by department.
data <- read_excel("data_paper.xlsx") |>
  mutate(quintile_IQPA = xtile(IQPA, n = 5),
         efficiency_quintile = xtile(efficiency, n = 5),
         capacity_quintile = xtile(capacity, n = 5))|> 
  arrange(dpto)

##Table 3
#Columns 3,4, 8, 9, and 10. 
df <- data[data$year %in% c(2004,2012,2019),
            c("year",
              "GDP_percap",
              "icgp",
              "efficiency",
              "capacity",
              "precipitation",
              "temperature",
              "secondary_ratio",
              "internet_ratio",
              "density")] |> 
  group_by(year) %>% 
  descr(., stats     = c("mean", "sd",  "med", "min", "max", "CV"),
        transpose = TRUE,
        headings  = FALSE)

#creating a table
df2004 <- df[["year = 2004"]]
df2004 <- tibble::rownames_to_column(df2004, "var")
df2012 <- df[["year = 2012"]]
df2012 <- tibble::rownames_to_column(df2012, "var")
df2019 <- df[["year = 2019"]]
df2019 <- tibble::rownames_to_column(df2019, "var")

#visualizing the table 
View(df2004)
View(df2012)
View(df2019)

#Table 3
#(Columns 5, 6, 7, and 11)
x <- data |> filter(year %in% c(c(2004,2012,2019))) |> 
  select(c("year",
           "GDP_percap",
           "IQPA",
           "efficiency",
           "capacity",
           "precipitation",
           "temperature",
           "secundary_ratio",
           "internet_ratio",
           "density")) |> 
  dplyr::group_by(year) %>% 
  summarise(Gp25 = quantile(GDP_percap, .25),
            Gp75 = quantile(GDP_percap, .75),
            Gt = theil.wtd(GDP_percap),
            IQPAp25 = quantile(IQPA, .25),
            IQPAp75 = quantile(IQPA, .75),
            IQPAt = theil.wtd(IQPA),
            e25 = quantile(efficiency, .25),
            ep75 = quantile(efficiency, .75),
            et = theil.wtd(efficiency),
            cp25 = quantile(capacity, .25),
            cp75 = quantile(capacity, .75),
            ct = theil.wtd(capacity),
            pp25 = quantile(precipitation, .25),
            pp75 = quantile(precipitation, .75),
            pt = theil.wtd(precipitation),
            tp25 = quantile(temperature, .25),
            tp75 = quantile(temperature, .75),
            tt = theil.wtd(temperature),
            sp25 = quantile(secundaria_ratio, .25),
            sp75 = quantile(secundaria_ratio, .75),
            st = theil.wtd(secundaria_ratio),
            ip25 = quantile(internet_ratio, .25),
            ip75 = quantile(internet_ratio, .75),
            it = theil.wtd(internet_ratio),
            dp25 = quantile(density, .25),
            dp75 = quantile(density, .75),
            dt = theil.wtd(density)
  )

#creating a table
x2004 <- x[["year = 2004"]]
x2004 <- tibble::rownames_to_column(x2004, "var")
x2012 <- x[["year = 2012"]]
x2012 <- tibble::rownames_to_column(x2012, "var")
x2019 <- x[["year = 2019"]]
x2019 <- tibble::rownames_to_column(x2019, "var")

#visualizing the table 
View(x2004)
View(x2012)
View(x2019)