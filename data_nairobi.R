rm(list=ls())
## Import and check a dataset ----

?read.csv

data_nairobi=read.csv("data_teeth_dirty.csv",
                      na.strings=c("","???"),
                      row.names=1) #use the first column as row names
