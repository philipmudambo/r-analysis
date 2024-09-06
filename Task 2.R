# 1. DATA SET THAT PIQUES MY CURIOSITY

#Question (a)
#I will be working with The Marvel Universe Social Network dataset 
#Original source: https://www.kaggle.com/datasets/csanhueza/the-marvel-universe-social-network/

#Question (b)
#Loading the data
edges<-read_csv("edges.csv")
edges
network<-read_csv("heronetwork.csv")
network
nodes<-read_csv("nodes.csv")
nodes

#Question (c)
#The dataset contains heroes and comics, and the relationship between them, divided into three files:
#hero-edge.csv: Contains the network of heroes which appear together in the comics.
#edges.csv: Contains two columns (hero, comic), indicating in which comics the heroes appear.
#nodes.csv: Contains two columns (node, type), indicating the name and the type (comic, hero) of the nodes.

#Exploring the dataframes
head(nodes)
head(edges)
head(network)

#Summary Statistics
#Number of heroes and comics
heroes <- nrow(nodes[nodes$type == "hero", ])
heroes
comics <- nrow(nodes[nodes$type == "comic", ])
comics
#Most popular comics
most_popular_comics <- edges %>% group_by(comic) %>% summarise(n_heroes = n()) %>%  arrange(desc(n_heroes))
most_popular_comics

#Calculating the degree centrality of the heroes
degree_centrality <- degree(g)
# Identifying the top 10 most connected heroes
top_10_connected_heroes <- sort(degree_centrality, decreasing = TRUE)[1:10]
#Creating a bar plot of the degree centrality of the top 10 most connected heroes
barplot(top_10_connected_heroes, names.arg = names(top_10_connected_heroes), main = "Degree Centrality of Top 10 Most Connected Heroes")

#Calculating the betweenness centrality of the heroes
betweenness_centrality <- betweenness(g)
#Identifying the top 10 most influential heroes
top_10_influential_heroes <- sort(betweenness_centrality, decreasing = TRUE)[1:10]
#Creating a bar plot of the betweenness centrality of the top 10 most influential heroes
barplot(top_10_influential_heroes, names.arg = names(top_10_influential_heroes), main = "Betweenness Centrality of Top 10 Most Influential Heroes")

#Question (d)
#A question I believe the data can help with an answer. 
#How does the network of heroes and comics change over time?


# 2. DIAMONDS DATA
#Question (a)
#Loading the data
library(ggplot2)
data(diamonds, package = "ggplot2")
diamonds

#Creating the scatter plot
ggplot(diamonds, aes(x = carat, y = price)) + geom_point() + labs(title = "Carat vs. Price", x = "Carat", y = "Price")
#From the plot:
#Yes, there is a positive relationship between carat and price which means that as the carat of a diamond increases, its price also increases. Here is an R code to draw a scatter plot of carat vs. price:

Question (b)
#Creating a subset of the data
subset <- diamonds[diamonds$carat >= 1 & diamonds$carat <= 2, ]
#Plotting the scatter plot
ggplot(subset, aes(x = carat, y = price)) +  geom_point() +  labs(title = "Carat vs. Price (1 ≤ carat ≤ 2)", x = "Carat", y = "Price")
#From the plot:
#Yes, there is a positive relationship between carat and price for diamonds for the subset 1 ≤ carat ≤ 2.

#Question (c)
ggplot(diamonds, aes(x = clarity)) + geom_bar() +  labs(title = "Distribution of Clarity", x = "Clarity", y = "Count")
#The bar plot shows the distribution of clarity for diamonds. 
#Clarity is a measure of the number and severity of inclusions (flaws) in a diamond. 
#Diamonds with higher clarity ratings have fewer and less severe inclusions, and are therefore more valuable.
#From the bar plot, it is evident that the most common clarity rating for diamonds is SI1, followed by VS2, SI2, VS1,VVS2, VVS1, IF and lastly I1. 

#Question (d)
#Filling each bar with different colors corresponding to cut. 
ggplot(diamonds, aes(x = clarity, fill = cut)) +  geom_bar() +  labs(title = "Distribution of Clarity", x = "Clarity", y = "Count")

#Question (e)
#Plotting a scatter plot with facets
ggplot(subset, aes(x = carat, y = price, color = cut)) +  geom_point() +  labs(title = "Carat vs. Price (1 ≤ carat ≤ 2) by Cut", x = "Carat", y = "Price") +  facet_wrap(~ cut)
#Interpretation:
#The plot shows that the relationship between carat, price, and cut for diamonds is complex and multifaceted. 
#Diamonds with a higher cut grade have a stronger relationship between carat and price, and are generally more expensive than diamonds with a lower cut grade.
#This means that consumers should consider all of these factors when purchasing a diamond in order to get the best value for their money.


# 3. HOW MANY CLUSTERS OF GALAXIES?
#Question (a)
#The bw=2 density estimate shows the clearest evidence that the galaxies are clustered.
#This is because the bw=2 estimate has the lowest bandwidth, which means that it is the least smoothed estimate. 
#This reveals the local maxima in the density estimate, which correspond to clusters of galaxies. 
#The other two density estimates are too smoothed, and they obscure the local maxima.
#Therefore, the bw=2 density estimate provides the clearest evidence that the galaxies are clustered in the Corona Borealis region.

#Question (b)
library(MASS)
gal <- galaxies/1000
plot(x = c(0, 40), y = c(0, 0.15), type = "n", xlab = "Velocity of galaxy (1000km/s)", ylab = "Density")
rug(gal)  # Adding a 'rug' (ticks along x-axis)
#Adding density lines with different bandwidths and line types
lines(density(gal, bw = 6), lty = 3)  # Less smooth line with larger bandwidth
lines(density(gal, bw = 4), lty = 2)
lines(density(gal, bw = 2), lty = 1)
#Experimenting with a less smooth line (larger bandwidth)
lines(density(gal, bw = 8), lty = 4, col = "red")  #Adjusting the bw value as needed

#Question (c)
#The bw=1 density estimate is the best choice because it is the most readable & also the most accurate. 
#Meaning that it's the easiest to understand and interpret & the most likely to represent the true distribution of the data.
#Other density estimates are either too smooth or too rough, and they do not show the data as clearly thus may not accurately reflect the underlying distribution.
#Therefore, the bw=1 density estimate is the best choice for both readability and accuracy.


# 4. WILL MY FLIGHT BE ON TIME?
#Question (a)
#Loading the necessary libraries
library(pacman)
library(nycflights13)
#Loading the data and merging it with airline carrier data
p_load(nycflights13, dplyr)
flights<-inner_join(flights, airlines, by = "carrier")
flights<-flights[,-12]            #drop extra copy of 'name'
colnames(flights)[19]<-"name"     #simplify variable name
#Calculating the total delay for each flight
flights$total_delay <- flights$dep_delay + flights$arr_delay
#Creating a density plot of the total delay
ggplot(flights, aes(x = total_delay)) + geom_density() + labs(title = "Density of Total Flight Delay", x = "Total Delay (minutes)", y = "Density")

#Question (b)
#Extracting relevant flights between December 15th and January 5th
holiday_flights <- flights[(flights$month == 12 & flights$day >= 15) | (flights$month == 1 & flights$day <= 5), ]
#Creating a data visualization using ggplot2 and boxplots
ggplot(holiday_flights, aes(x = name, y = dep_delay)) + geom_boxplot(fill = "skyblue", color = "blue") + labs(title = "Delays by Carrier Around Holidays", x = "Carrier", y = "Departure Delay (minutes)") + theme_minimal()
#Based on the boxplot, it appears that Delta Air Lines (DL) has the best record for on-time performance during the holidays. 
#Delta has the lowest median delay (13 minutes) & the narrowest interquartile range (IQR) of the carriers shown. 
#This suggests that Delta flights are less likely to be delayed than flights on other carriers, & that when Delta flights are delayed, they are typically only delayed by a small amount of time.

#Question (c)
#The "distance" column is not available in the dataset then it's impossible to directly plot the relationship between distance and delay.
#However, one can still investigate the relationship between other variables and delay. 
#For example, one might want to explore how the departure time or scheduled departure time correlates with delay

#Creating a scatter plot with "dep_time" and "dep_delay"
ggplot(flights, aes(x = dep_time, y = dep_delay)) + geom_point(alpha = 0.5, color = "blue") + labs(title = "Relationship Between Departure Time and Delay", x = "Departure Time", y = "Departure Delay (minutes)") + theme_minimal()
#From the plot, there's a positive relationship (correlation). 
#This means that as departure time increases, so does delay, likely because flights that depart later in the day are more likely to be delayed due to factors such as air traffic congestion, weather delays, or mechanical problems.

#Question (d)
#Creating a visualization to describe the hypothesis
ggplot(flights, aes(x = hour, y = dep_delay)) + geom_point(alpha = 0.5, color = "blue") + labs(title = "Departure Delays by Departure Hour", x = "Departure Hour", y = "Departure Delay (minutes)") + theme_minimal()
#From the plot, it's evident that there's a positive correlation between departure time and delay therefore the data supports the hypothesis.
#Meaning that flights that depart later in the day are more likely to be delayed. 
#The highest delays occur around the evening rush hour (17:00-19:00), with a slight increase in delays in the morning rush hour (07:00-09:00). 
#This delays are generally lower during the middle of the day (10:00-16:00). 
#Also, there are a few outliers with very long delays, likely due to severe weather or mechanical problems.