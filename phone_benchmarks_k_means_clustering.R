## k means clustering

# load lib
library(tidyverse)

benchmark_numerics <- phone_benchmarks %>% 
  select(cpuScore, gpuScore, npuScore) %>% 
  scale()

benchmark_clusters <- kmeans(benchmark_numerics, centers = 7)
benchmark_clusters

benchmark_clusters$cluster


phone_benchmarks$cluster <- benchmark_clusters$cluster

phone_benchmarks %>% 
  ggplot(aes(cpuScore, gpuScore)) +
    geom_point(aes(color = as.factor(cluster)))

phone_benchmarks %>% 
  ggplot(aes(cpuScore, gpuScore)) +
  geom_point(aes(color = company))

phone_benchmarks %>% 
  ggplot(aes(cluster)) +
  geom_bar(aes(fill = company))
