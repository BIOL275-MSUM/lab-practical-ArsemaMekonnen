

##Package-------------------------

library(tidyverse) 
library(readxl)

mosquitos <- read_csv("Lab_Practice_files/practical-data-2019.csv")

ggplot(
  data = mosquitos,                    
  mapping = aes(x = facets for county,      
                fill = outcome)       
) +
  geom_histogram(bins = 14) +         
  facet_wrap(~ outcome, ncol = 1) +   
  guides(fill = FALSE) +              
  labs(
    title = "Figure 1.",             
    x = "facets for county",          
    y = "mosq_count"            
  )

plot(temp)
mosquitos
ggplot(data = mosquitos) +
  geom_histogram(mapping = aes(x = mosq_count), binwidth = 0.5)

summary(mosq_count)
ggplot(data = mosquito) +
  geom_bar(mapping = aes(x = mosq_count))

mosq_count_grouped_summary <- 
  mosquitos %>% 
  filter(!is.na(mosq_count)) %>% 
  group_by(county) %>% 
  summarize(mean = mean(mosq_count),
            sd = sd(mosq_count),
            n = n()) %>% 
  mutate(sem = sd / sqrt(n),
         upper = mean + 2 * sem,
         lower = mean - 2 * sem)
