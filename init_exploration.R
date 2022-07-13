
## practice plotting with data

library(ggplot2)

ggplot(data = phone_benchmarks, aes(cpuScore)) +
  geom_histogram(binwidth = 30, aes(fill = company)) +
  labs(x = "CPU Performance Score (higher is better)", y = "Count") +
  scale_fill_brewer(palette = "Reds") + 
  theme_dark()


