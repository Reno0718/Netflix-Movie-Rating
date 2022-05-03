# Group 19: Reno Chen, Jimin Huh, Wei Ching Lim, Yuqing Luo
# 12/10/2021

rm(list = ls())

################## Webscraping from IMDb ##################
#install.packages("rvest") 
library('rvest')
library('xml2')

seq <- seq(0,9951,50)
titles <- character(0)
years <- character(0)
genres <- character(0)
ratings <- character(0)
durations <- character(0)

for (i in seq){
  #print(i)
  url <- paste("https://www.imdb.com/search/title/?companies=co0144901&start=",i,sep="")
  page <- read_html(url)
  
  title1 <- xml_text(xml_find_all(page,"//h3[@class = 'lister-item-header']/a[1]"))
  titles <- c(titles,title1)
  
  year1 <- xml_text(xml_find_all(page,"//h3[@class='lister-item-header']/span[@class='lister-item-year text-muted unbold'][1]"))
  years <- c(years,year1)
  
  genre1 <- html_text(html_node(html_nodes(page, '.lister-item-content'), '.genre'))
  genres <- c(genres,genre1)
  
  rating1 <- html_text(html_node(html_nodes(page, '.lister-item-content'),'.ratings-imdb-rating strong'))
  ratings <- c(ratings,rating1)
  
  duration1 <- html_text(html_node(html_nodes(page, '.lister-item-content'),'.runtime'))
  durations <- c(durations,duration1)
} 

################## Create Dataframe for scraped data ##################

imdb_netflix <- data.frame(title = titles,
                           year = years,
                           genre = genres,
                           rating = ratings,
                           duration = durations)

################## Kaggle dataset ####################

Netflix <- read.csv("netflix_titles.csv",header = T, na.strings=(""))

################## Integrating data ##################
setdiff(imdb_netflix,Netflix)

Combine <- merge(Netflix,imdb_netflix,by.x = "title",by.y = "title",all.x = T)

sum(is.na(Combine$rating.y))

netflix<- Combine[!is.na(Combine$rating.y) & Combine$type == "Movie",]

netflix$year <- NULL
netflix$genre <- NULL
netflix$duration.y <- NULL
netflix$description <- NULL
netflix$cast <- NULL
netflix$director <- NULL
netflix$duration.x <- as.numeric(gsub(" min", "", netflix$duration.x))
str(netflix )

names(netflix)[names(netflix)=="rating.x"] <- "age_rating"
names(netflix)[names(netflix)=="rating.y"] <- "star_rating"
netflix$star_rating <- as.numeric(netflix$star_rating)

################## Clean the data ##################
str(netflix)

library(dplyr)
library(stringr)
library(sjmisc)

netflix$country2<- if_else(grepl(",",netflix$country), "International", netflix$country)

netflix$genre2<- sapply(strsplit(netflix$listed_in, ","), "[", 1)

netflix$X<- NULL
netflix$type<- NULL
netflix$country<- NULL
netflix$listed_in<- NULL

names(netflix)[names(netflix) == "duration.x"]<-"duration"
names(netflix)[names(netflix) == "genre2"]<-"genre"
names(netflix)[names(netflix) == "country2"]<-"country"

netflix$date_added<- as.Date(netflix$date_added, "%B %d, %Y")
netflix$duration<- as.numeric(netflix$duration)
netflix$genre<- as.factor(netflix$genre)

netflix<- netflix[netflix$age_rating!="74 min",]

######### Changing values of genres
str(netflix)

unique(netflix$genre)
change<- subset(netflix, netflix$genre=="Movies", )
change
netflix[netflix$show_id=="s4492", "genre"] <- "Thrillers"
netflix[netflix$show_id=="s5877", "genre"] <- "Dramas"
netflix[netflix$show_id=="s2965", "genre"] <- "Documentaries"

netflix$genre<- as.factor(netflix$genre)

################################ Research Question 1 ################################
# Is there a relationship between star ratings and durations (in minutes)?

netflix$movie_timeFrame <- ifelse(netflix$release_year < 2018, "Recent Movie","New Movie") 

library(dplyr)
library(reshape2)

##### Find the difference in timeFrame of movies

movie_group <- group_by(netflix,movie_timeFrame)
movie_summary <- summarise(movie_group,
                           Total = n(),
                           Average = round(mean(star_rating,na.rm = T),2),
                           Median = median(star_rating,na.rm = T),
                           Min = min(star_rating,na.rm = T),
                           Max = max(star_rating,na.rm = T))
movie_summary

##### qqplot & scatter plot 
library(ggplot2)

plot(netflix$duration,netflix$star_rating, main="Scatterplot Example",
     xlab="Duration in minutes", ylab="Star Rating")

qqnorm(netflix$duration)
qqline(netflix$duration,col = "red")
# Points deviate from reference line at ends so it may not be normally distributed

shapiro.test(netflix$duration)
# p-value < 2.2e-16. Thus we can reject the null hypothesis 
# that duration is normally distributed

################################ Research Question 2 ################################
# Which features; genre, duration, year released and age rating, 
# impact the star ratings of movies more or less?

# Star rating is dependent variable 
# Dependent var = Independent var + error
# Independent var = country, release_year, age_rating, duration, genre

########################### ANOVA Test ###################################
# Are there any significant differences in features by star rating?
# ANOVA test for categorical variables: age_rating, genre, country
# H0: They all have the same mean

########################### Age rating ###################################
age<-aov(star_rating~age_rating, data=netflix)
summary(age)
# p-value = 0.0805 means no significant difference
# P-value > 0.05. Thus we cannot reject the null hypothesis 
# that mean of star rating is equal for all age ratings

########################### Genre ###################################
genre<-aov(star_rating~genre, data=netflix)
summary(genre)
# p-value is <2e-16 means there is significant difference
# P-value < 0.05. Thus we can reject the null hypothesis 
# that mean of star rating is equal for all genres

# Test the assumptions of the ANOVA
#########  Normality:
shapiro.test(netflix$star_rating[which(netflix$genre=="Comedies")])
# p-values = 0.02473 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Comedies genre.
 
shapiro.test(netflix$star_rating[which(netflix$genre=="Dramas")])
# p-values = 1.831e-07 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Dramas genre.

shapiro.test(netflix$star_rating[which(netflix$genre=="Horror Movies")])
# p-values = 0.4246 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Horror Movies genre 
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Documentaries")])
# p-values = 2.908e-08 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Documentaries genre.

shapiro.test(netflix$star_rating[which(netflix$genre=="Action & Adventure")])
# p-values = 0.09159 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Action & Adventure genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Children & Family Movies")])
# p-values = 0.5403 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Children & Family Movies genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="International Movies")])
# p-values = 0.487 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within International Movies genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Anime Features")])
# p-values = 0.2722 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Anime Features genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Stand-Up Comedy")])
# p-values = 2.201e-06 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Stand-Up Comedy genre.

shapiro.test(netflix$star_rating[which(netflix$genre=="Independent Movies")])
# p-values = 0.01769 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Independent Movies genre.

shapiro.test(netflix$star_rating[which(netflix$genre=="Music & Musicals")])
# p-values = 0.1102 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Music & Musicals genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Cult Movies")])
# Sample size too small to test

shapiro.test(netflix$star_rating[which(netflix$genre=="Thrillers")])
# p-values = 0.5166 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Thrillers genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Classic Movies")])
# p-values = 0.5572 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Classic Movies genre
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$genre=="Romantic Movies")])
# Sample size too small to test

shapiro.test(netflix$star_rating[which(netflix$genre=="Sci-Fi & Fantasy")])
# Sample size too small to test

shapiro.test(netflix$star_rating[which(netflix$genre=="LGBTQ Movies")])
# Sample size too small to test

######### Equal variance
# H0: Var1=var2=var3=var4 / They all have the same variance

### Remove genre that have less than 2 categories
temp_genre<- group_by(netflix, genre)
genre_summary<- summarise(temp_genre,
                        TotalGenre = n()) 
genre_summary #LGBTQ Movies and Sci-Fi & Fantasy

genre_removed<-netflix[netflix$genre!="LGBTQ Movies" & netflix$genre!="Sci-Fi & Fantasy" ,]
head(genre_removed)

bartlett.test(star_rating~genre, data=genre_removed)
# P-value = 0.001703 < 0.05. Thus we can reject the null hypothesis 
# of equal variance by group.
# At least one of them has different variance than the rest

###### Both assumptions are violated

# Follow up test using non-parametric test like Kruskal-Wallis:
kruskal.test(star_rating~genre, data=genre_removed)
kruskal.test(star_rating~genre, data=netflix)
# Both P-values =< 2.2e-16 < 0.05. Thus we can reject the null hypothesis
# that median star rating is equal for all genres.

######################### Country #####################################
country<-aov(star_rating~country, data=netflix)
summary(country)
# p-value = 8.88e-09 means there is significant difference
# P-value < 0.05. Thus we can reject the null hypothesis 
# that mean of star rating is equal for all countries.

### Check which country has less than 3 movies
unique(netflix$country) 

country_freq<- data.frame(table(netflix$country)) 
country_freq<- country_freq[order(country_freq$Freq, decreasing = TRUE),]
country_freq

# Test the assumptions of the ANOVA
#########  Normality:
# We will be testing normality assumptions on countries that have more than three movies

shapiro.test(netflix$star_rating[which(netflix$country=="United States")])
# p-values = 2.832e-07 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within United States.

shapiro.test(netflix$star_rating[which(netflix$country=="International")])
# p-values = 0.0003104 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within International category.

shapiro.test(netflix$star_rating[which(netflix$country=="India")])
# p-values = 9.79e-05 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within India.

shapiro.test(netflix$star_rating[which(netflix$country=="United Kingdom")])
# p-values = 0.01259 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within United Kingdom.

shapiro.test(netflix$star_rating[which(netflix$country=="Spain")])
# p-values = 0.9885 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Spain
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="France")])
# p-values = 0.3896 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within France
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Canada")])
# p-values = 0.2009 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Canada
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Mexico")])
# p-values = 0.08052 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Mexico
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Nigeria")])
# p-values = 0.137 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Nigeria
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="South Korea")])
# p-values = 0.4004 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within South Korea
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Indonesia")])
# p-values = 0.01996 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Indonesia.

shapiro.test(netflix$star_rating[which(netflix$country=="Italy")])
# p-values = 0.04657 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Italy.

shapiro.test(netflix$star_rating[which(netflix$country=="Egypt")])
# p-values = 0.08804 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Egypt
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Germany")])
# p-values = 0.3471 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Germany
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Brazil")])
# p-values = 0.04292 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Brazil.

shapiro.test(netflix$star_rating[which(netflix$country=="Turkey")])
# p-values = 0.544 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Turkey
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Argentina")])
# p-values = 0.4133 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Argentina
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Japan")])
# p-values = 0.6751 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Japan
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Thailand")])
# p-values = 0.1413 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Thailand
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Netherlands")])
# p-values = 0.5805 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Netherlands
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Australia")])
# p-values = 0.3827 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Australia
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Poland")])
# p-values = 0.562 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Poland
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="South Africa")])
# p-values = 0.4099 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within South Africa
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Taiwan")])
# p-values = 0.02275 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Taiwan.

shapiro.test(netflix$star_rating[which(netflix$country=="Colombia")])
# p-values = 0.8327 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Colombia
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Malaysia")])
# p-values = 0.04123 < 0.05. Thus we can reject the null hypothesis 
# that star rating is normally distributed within Malaysia.

shapiro.test(netflix$star_rating[which(netflix$country=="China")])
# p-values = 0.5148 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within China
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Chile")])
# p-values = 0.5789 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Chile
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Austria")])
# p-values = 0.7391 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Austria
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Philippines")])
# p-values = 0.08934 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Philippines
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Singapore")])
# p-values = 0.09721 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Singapore
# at confidence level of 0.05.

shapiro.test(netflix$star_rating[which(netflix$country=="Sweden")])
# p-values = 0.2196 > 0.05. Thus we cannot reject the null hypothesis 
# that star rating is normally distributed within Sweden
# at confidence level of 0.05.

######### Equal variance
# H0: Var1=var2=var3=var4 / They all have the same variance

##### Removing country that has less than 3 observations
netflix2<- data.frame(netflix) # create new df to keep original clean
country_removed <-  netflix2 %>% group_by(country) %>% filter(n()>=3)
head(country_removed)

country_freq2<- data.frame(table(country_removed$country)) 
country_freq2<- country_freq2[order(country_freq2$Freq, decreasing = TRUE),]
country_freq2 #check

bartlett.test(star_rating~country, data=country_removed)
# P-value = 0.3405 > 0.05. Thus we cannot reject the null hypothesis 
# of equal variance by group.

##### One of the assumption is violated

# Follow up test using non-parametric test like Kruskal-Wallis:
kruskal.test(star_rating~country, data=country_removed) # p-value = 9.448e-09
kruskal.test(star_rating~country, data=netflix) # p-value = 1.145e-07
# Both P-values < 0.05. Thus we can reject the null hypothesis
# that median star rating is equal for all countries.

################################# Duration ######################################
# Is there a linear relationship between the duration and star rating of movie?
duration_starRating<-cor(netflix$star_rating, netflix$duration)
duration_starRating
# 0.02864826
# Weak, positive correlation (increasing duration of movies is associated with higher star ratings)

######################### Linear Regression Test #####################################

star_rating<- lm(netflix$star_rating~netflix$genre+netflix$duration+
                   netflix$country+netflix$release_year+netflix$age_rating, data=netflix) 
                      #if reject then put in regression
summary(star_rating)
# We add age_rating in reg model to parse out their effects

######################### Interpretation of Results ######################### 

# p-value: < 2.2e-16 of F stat is very low, we are rejecting H0: intercept only model is the same as our model
# H0: our model is the same as intercept only model
# star_rating=beta_0 (intercept only model)

# Adjusted R-squared:  0.2587, means 26% of variation in star rating is explained by this model that is created

##### Release year
# H0: The estimate is equal to zero. 
# p-value = 2.13e-05, thus, we reject HO that the estimate is equal to zero. (estimate significantly different than 0)
# If release year increase by one unit, star rating decreases by 0.021365

##### Duration
# H0: The estimate is equal to zero. 
# p-value = 1.13e-14, thus, we reject HO that the estimate is equal to zero.
# If duration increase by one unit, star rating increases by 0.009569
### Can infer that people would enjoy movies with longer duration and therefore give them better rating

##### Country # intercept: Argentina
# For most countries, results show that p-value > 0.05, therefore we cannot reject H0 
# and there are no significant differences.

## Countries with p-value < 0.05 and have significant differences shows that the star ratings decrease
# Colombia - p-value = 0.03445 
# If country changes from Argentina (intercept) to Colombia, the star rating decreases by 0.935603

# Egypt - p-value = 0.03596
# If country changes from Argentina (intercept) to Egypt, the star rating decreases by 0.710976

# United Arab Emirates  - p-value = 0.00103 
# If country changes from Argentina (intercept) to United Arab Emirates , the star rating decreases by 3.278328

##### Genre intercept: Action & Adventure
# For most genres, results show that p-value > 0.05, therefore we cannot reject H0 
# and there are no significant differences.

##### Genres with p-value < 0.05 and have significant differences shows that the star ratings increase
# Comedies - p-value = 0.04257
# If genre changes from Action & Adventure (intercept) to Comedies, 
# the star rating increases by 0.200005

# Documentaries - p-value = < 2e-16
# If genre changes from Action & Adventure (intercept) to Documentaries, 
# the star rating increases by 1.381646

# Dramas - p-value = 6.09E-06
# If genre changes from Action & Adventure (intercept) to Dramas, 
# the star rating increases by 0.408419

# Music & Musicals - p-value = 1.47e-07
# If genre changes from Action & Adventure (intercept) to usic & Musicals, 
# the star rating increases by 1.965064

# Stand-Up Comedy - p-value = < 2e-16
# If genre changes from Action & Adventure (intercept) to Stand-Up Comedy, 
# the star rating increases by 1.341767

## Genres with p-value < 0.05 and have significant differences shows that the star ratings decrease
# Horror Movies - p-value = 7.63e-05
# If genre changes from Action & Adventure (intercept) to Horror Movies 
# the star rating decreases by 0.515191

# Thrillers - p-value = 0.02012
# If genre changes from Action & Adventure (intercept) to Horror Movies 
# the star rating decreases by 0.590298

######### Check Assumptions:
# 1) Linearity
plot(netflix$duration, netflix$star_rating) # No trend = violation of linearity
plot(netflix$release_year, netflix$star_rating) # No trend = violation of linearity

# 2) Normality
qqnorm(star_rating$residuals)
qqline(star_rating$residuals, col='red')
shapiro.test(star_rating$residuals)
# H0: It follows normal distribution
# p-value = 1.505e-14 < 0.05
# We can reject H0. It does not follow a normal distribution

# 3) Homoscedasticity (must not have any trend, if have trend is wrong)
plot(star_rating$fitted.values, star_rating$residuals, 
      main = "Homoscedasticity of Linear Regression Model", 
      xlab = "Fitted Values",
      ylab = "Residuals") #residual is error term
# Our model is not suffering from homoscedasticity

# 4) Multicollinearity
# install.packages("car")
library(car)
vif(star_rating)

#genre        14.819237 # VIF>10 so model is suffering from multicollinearity, 
                        #    so reg coefficient is poorly estimate 
#duration     1.838962  # VIF<10 so model is not suffering from multicollinearity 
#country      4.847813  # VIF<10 so model is not suffering from multicollinearity 
#release_year 1.656278  # VIF<10 so model is not suffering from multicollinearity
#age_rating   6.055044  # VIF<10 so model is not suffering from multicollinearity

################################ Research Question 3 ################################
# Is there any difference in average star ratings between 
# the recent movies (released before 2018) and the new movies (released after 2018)?

netflix$time <- ifelse(netflix$release_year < 2018, "recent", "new" )

movies_new <- netflix[netflix$time=="new",
                        "star_rating"]
movies_recent <- netflix[netflix$time=="recent",
                     "star_rating"]
time_test <- t.test(movies_new,movies_recent)
time_test
# p-value = 0.0001992
# P < 0.05. Thus we reject the null hypothesis 
# that star ratings differs by new and recent movies

temp <- group_by(netflix, release_year)
totalMovies_byYear <- summarise(temp,
                                total_movies=n())
qplot(release_year,
      total_movies,
      data = totalMovies_byYear,
      geom="line",
      main = "Total movies group by years")
