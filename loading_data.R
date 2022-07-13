#### Load in csv file ####

phone_benchmarks <- read.csv("data/ML_ALL_benchmarks.csv")


# saving as RDS allows us to use it as an object, which should be a lot smaller than the csv file
saveRDS(phone_benchmarks, "data/phone_benchmarks.RDS") 
# yep after rdsing its -8kb


