#Load Required Libraries
library(dplyr)
library(ggplot2)
install.packages('choroplethr')
library(choroplethr)
install.packages('choroplethrMaps')
library(choroplethrMaps)
install.packages('openintro')
library(openintro)
 

#Importing DataSet 
data = read.csv(file.choose())
data

#Removing unwanted Column
df = subset(data,select = -c(X))
df

#Lets Count number of state in dataset 
count(unique(df[c("State")]))

#Filtering Data with Filter Function, here we are only interested in three state
df %>% 
  filter(State == "CA" | State == "TX" | State == "FL")

#Lets see the faiures of Car in Arizona State with Mileage more than 1200 and Labor Cost more than 300

df %>% 
  filter(State =="TX", Mileage > 1200 , lc > 300)  

#Data Manipulation 

# Which State with high labour cost ? 

df %>% 
  filter(State == "CA" | State == "TX" | State == "FL") %>% 
  arrange(desc(lc))

#State with high labour cost and labour hours

df %>% 
  filter(State=='CA' | State == 'TX' | State=='AZ') %>% 
  arrange(desc(lc),desc(lh))

# Statistical Analysis of our : 
#Texas State
df %>% 
  filter(State == "TX") %>% 
  summarise(Avg_lc = mean(lc),
            Median_lc = median(lc),
            Max_lc = max(lc),
            Min_lc = min(lc),
            sd_lc = sd(lc),
            no_of_obs = n())

#California
df %>% 
  filter(State == "CA") %>% 
  summarise(Avg_lc = mean(lc),
            Median_lc = median(lc),
            Max_lc = max(lc),
            Min_lc = min(lc),
            sd_lc = sd(lc),
            no_of_obs = n())
#Arizona
df %>% 
  filter(State == "AZ") %>% 
  summarise(Avg_lc = mean(lc),
            Median_lc = median(lc),
            Max_lc = max(lc),
            Min_lc = min(lc),
            sd_lc = sd(lc),
            no_of_obs = n())

## Lets see relatio between Labour cost and Labour working hours in big state of USA i.e. California , Texas and Arizona

#Scatter Plot

df %>% 
  filter(State == "CA" |  State == "TX" |  State == "AZ") %>% 
  ggplot(aes(x = lh, y = lc,fill = State)) + 
  geom_point(alpha = 0.5,colour = "blue") + 
  geom_smooth(se = 0) +
  facet_wrap(~State)

#Histogram
df %>%
  filter(State=='CA' | State == 'TX' | State=='AZ') %>%
  ggplot(aes(x=lc, fill = State)) +
  geom_histogram(alpha=0.8, color='darkblue') +
  ggtitle('Labor cost in Top 3 states') +
  facet_wrap(~State)


#Density Plot
df %>%
  filter(State=='CA' | State == 'TX' | State=='AZ') %>%
  ggplot(aes(x=lc, fill = State)) +
  geom_density(alpha=0.8, color='darkblue') +
  ggtitle('Labor cost in Top 3 states')

#Bar Plot 

new_data  <- df %>%
  group_by(State) %>%
  mutate(cph = sum(lc)/sum(lh)) %>%
  summarise(Avg_cph = mean(cph),
            Avg_mileage = mean(Mileage)) %>%
  arrange(desc(Avg_cph))

ggplot(new_data, aes(x=State, y = Avg_cph, fill = State)) +
  geom_col() +
  coord_flip() +
  ggtitle('Cost per hour in 50 states')

#Box Plot for Number of failures in Mileage in all states 
df %>% 
  group_by(State) %>% 
  filter(n()>45) %>% 
  ggplot(aes (x = State, y = Mileage,col = State))+
  geom_boxplot()




 


