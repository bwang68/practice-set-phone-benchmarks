
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

phone_benchmarks %>% 
  filter(company == "Qualcomm" | company == "Apple") %>% 
  ggplot(aes(cpuScore, color = company)) + 
  geom_boxplot()

phone_benchmarks %>% 
  filter(grepl('iPhone|Samsung', device)) %>%
  arrange(desc(cpuScore)) %>%
  ggplot(aes(cpuScore, fill = company)) + 
  geom_histogram(binwidth = 50) +
  labs(x = "Cpu Score, higher is better",
       y = "Count of phones",
       title = "Phone CPU benchmarks, Apple vs Qualcomm vs Samsung") +
  scale_fill_brewer(palette = "Set2")
  

# practice w dplyr

library(stringr)

android_only <- phone_benchmarks %>% 
  filter(company != "Apple") %>% 
  select(device, company, cpuName, clock, cpuScore, gpuScore) %>% 
  mutate(totalScore = cpuScore + gpuScore) %>% 
  mutate(cpuMaker = company) %>% 
  # want to select the first word from device for phone maker name
  # use stringr library
  mutate(phone_company = word(device, 1)) %>% 
  #notice that ASUS and Asus are separate, need to join these grps together
  mutate(phone_company = str_replace(phone_company, "ASUS", "Asus"))

# barplot of phone performance by company
android_only  %>% 
  arrange(desc(totalScore)) %>% 
  ggplot(aes(reorder(phone_company, -totalScore), totalScore, fill = phone_company)) +
  geom_bar(stat = "summary",
           fun = "mean") + 
  labs(x = "Phone Company",
       y = "Phone CPU + GPU Score",
       title = "Phone average performance scores by company (higher is better)") +
  scale_fill_brewer(palette = "Set3") +
  theme_light()


#look at summary stats

android_only %>% 
  group_by(phone_company) %>% 
  summarize(mean_total_score = mean(totalScore, na.rm = TRUE))



