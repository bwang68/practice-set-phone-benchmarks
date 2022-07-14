
## practice plotting with data

library(ggplot2)

ggplot(data = phone_benchmarks, aes(cpuScore)) +
  geom_histogram(binwidth = 30, aes(fill = company)) +
  labs(x = "CPU Performance Score (higher is better)", y = "Count") +
  scale_fill_brewer(palette = "Reds") + 
  theme_dark()


## practice subsetting data
# use a combo of filter, select, mutate, arrange, summarise, group_by, sample_n and/or slice, rename
# create visulization using new datasets

phone_benchmarks %>% 
  filter(company == "Qualcomm") %>% 
  ggplot(aes(cpuScore)) + 
  geom_histogram(binwidth = 30)

phone_benchmarks %>% 
  filter(company == "Apple") %>% 
  ggplot(aes(cpuScore)) + 
  geom_histogram(binwidth = 70)



# sidenote for changing a certain value in a column to something else we can do
# data$colname <- sub(val_to_change, new_value, data$colname)


