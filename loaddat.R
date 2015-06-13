library(dplyr)

#read the data
sim.p <- read.csv("SIM_properties.csv",header=TRUE, sep = ";")
sim.u <- read.csv("SIM_usage.csv",header=TRUE, sep = ";")

sim.u=sim.u %>% filter(data_usage_raw_total  != 0)
