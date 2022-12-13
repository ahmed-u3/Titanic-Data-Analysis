# ----- Loading packages ---------

## tidyverse loads dplyr and readr
library(tidyverse)

## To have different color maps
library(viridis)

## ggplot2 to produce different plots
library(ggplot2)

## uses ggplot2 to produce a correlation matrix -- the data must be in the correct form
library(ggcorrplot)

## Gives us better themes
library(hrbrthemes)

## to use skewness fun. to calculate skewness of the distribution
library(e1071)

## Multivariate imputation using chained equations -- to impute the missing values in our data
library(mice)

## Loads different statistical functions
library(statsr)

## To produce interactive plot
library(plotly)


# Loading Training Data
train <- read_csv("data/train.csv")
train <- train %>%
  dplyr::mutate(Sex = as_factor(Sex))
# Loading Testing Data
test <- read_csv("data/test.csv")

# Binding them into a full data frame
df <- bind_rows(train,test)

# Identifying our features
colnames(train)

# Number of missing values in each feature
colSums(is.na(train))

# We can observe that the cabin feature has the most missing values in our training data, and it's most probably not going to be useful initially, which means that we can drop it


#-------Summary of Data----------
summary(train)

missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")

missing_values

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red") +
  coord_flip() + # to flip the graph
  xlab("Features") +
  ylab("Percentage of missing values")+
  scale_y_continuous(labels=scales::percent) +
  theme_ipsum()

sapply(train , function(x) {sum(is.na(x))})

# Viewing the first six rows of the data
head(train)

## Correlation Matrix
correlationMatrix <- train %>%
  filter(!is.na(Age)) %>%
  select(Survived, Pclass,Age,SibSp,Parch,Fare) %>%
  cor() %>%
  ggcorrplot(lab = T,
             ggtheme =theme_ipsum_rc(grid = F),
             title="Correlation Matrix",hc.order=T,
             colors =rev(viridis(3,alpha=0.7)),
             digits = 2)

correlationMatrix

## Pclass Against Survived
gPclassSurvived <- train %>%
  select(Pclass,Survived) %>%
  ggplot(aes(as_factor(Pclass),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(labels=scales::percent) +
  theme_ipsum_rc() + 
  labs(x = "Classes",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived"))

## SibSp Against Survived
gSibSpSurvived <- train %>%
  select(SibSp,Survived) %>%
  ggplot(aes(as_factor(SibSp),fill=as_factor(Survived))) +
  geom_bar(position = "fill") + 
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Siblings and Spouses",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum()

## Parch against Survived
gParchSurvived <- train %>%
  select(Parch,Survived) %>%
  ggplot(aes(as_factor(Parch),fill=as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent)+
  labs(x = "Number of parents/children",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum_rc()

## Sex against Survived
gSexSurvived <- train %>%
  select(Sex,Survived) %>%
  ggplot(aes(as_factor(Sex),fill = as_factor(Survived))) + 
  geom_bar(position = "fill") +
  scale_y_continuous(label = scales::percent) + 
  labs(x = "Sex",y = "Survival Rate")+
  scale_fill_discrete(name = "Survived", labels = c("Didn't Survive","Survived")) +
  theme_ipsum_rc()

gridExtra::grid.arrange(gPclassSurvived,
                        gSibSpSurvived,
                        gSexSurvived,
                        gParchSurvived,
                        nrow=2)

train %>%
  group_by(Sex) %>%
  summarise(Age_mean = mean(Age,na.rm=TRUE),
            age_sd = sd(Age,na.rm=T),
            surival_mean = mean(Survived,na.rm =T),
            surival_sd = sd(Survived,na.rm = T))


# Density Graphs
gAgeDensity <- train %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 20,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  annotate(
    "text",
    x = 70,
    y = 0.04,
    label = paste("Skewness:",skewness(train$Age,na.rm = T)),
    colour = inferno(1,begin = 0.1),
    size = 4
  ) + 
  theme_ipsum_rc()

gAgeDensity

gFareDensity <- train %>%
  select(Fare) %>%
  ggplot(aes(Fare, y = ..density..)) +
  geom_histogram(bins = 20,binwidth = 1,color=viridis(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = viridis(1,begin=0)) + 
  scale_y_continuous(limits = c(0,0.05))+
  theme_ipsum_rc() + 
  annotate(
    "text",
    x = 200,
    y = 0.05,
    label = paste("Skewness",skewness(train$Fare)),
    colour = "black",
    size = 4
  )

gFareDensity 

train_log <- train %>%
  select(Fare) %>%
  mutate(Fare = log(Fare))

colSums(is.na(train_log))

FareDensity <- train_log %>%
  ggplot(aes(Fare, y = ..density..)) +
  geom_histogram(binwidth = 0.1,color = "black",fill=inferno(1,begin=0.8,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0.2)) + 
  theme_ipsum_rc()

FareDensity 
#---------------MICE--------------
set.seed(129)
mice_mod <- mice(train[,!names(train) %in% c('PassengerId','Name','Ticket','Cabin','Survived')],method = 'rf')
mice_output <- complete(mice_mod)

gdistrOriginalData <- train %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) +
  ggtitle("Distribution of original data") +
  theme_ipsum_rc()
gdistrMICEData <- mice_output %>%
  select(Age) %>%
  ggplot(aes(Age, y = ..density..)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) + 
  ggtitle("Distribution of mice output") +
  theme_ipsum_rc()

gridExtra::grid.arrange(gdistrOriginalData,gdistrMICEData,nrow = 1)


colSums(is.na(mice_output))
colSums(is.na(train))

train$Age <- mice_output$Age

colSums(is.na(train))
# ---- ggploat ----
numOfSamples <- seq(50,1000,50)

smplngDstrbtnRpsChng <- tibble()

for(i in numOfSamples){
  
  for(y in 1:i){
    nsample <- sample_n(train,size=50,replace=T) %>%
      select(Age)
    newRow <- nrow(smplngDstrbtnRpsChng) + 1
    smplngDstrbtnRpsChng[newRow,"reps"] <- i
    smplngDstrbtnRpsChng[newRow,"x_bar"] <- mean(nsample$Age,na.rm = T)
  }
  
}

gSamplingReps <- smplngDstrbtnRpsChng %>%
  plot_ly(
    x = ~x_bar,
    frame = ~reps,
    type = "histogram"
  )
gSamplingReps

sizes <- seq(20,260,20)
smplngDstrbtnSzChng <- tibble()
for(i in sizes){
  for(y in 1:1500){
    nsample <- sample_n(train,size=i,replace=T) %>%
      select(Age)
    newRow <- nrow(smplngDstrbtonSzChng) + 1
    smplngDstrbtonSzChng[newRow,"sizes"] <- i
    smplngDstrbtonSzChng[newRow,"x_bar"] <- mean(nsample$Age,na.rm = T)
  }
}
gSamplingSize <- smplngDstrbtonSzChng %>%
  plot_ly(
    x = ~x_bar,
    frame = ~sizes,
    type = "histogram"
  )
gSamplingSize

sizes <- seq(20,260,20)
smplngDstrbtnSzChngVariance <- tibble()
for(i in sizes){
  for(y in 1:1500){
    nsample <- sample_n(train,size=i,replace=T) %>%
      select(Age)
    newRow <- nrow(smplngDstrbtnSzChngVariance) + 1
    smplngDstrbtnSzChngVariance[newRow,"sizes"] <- i
    smplngDstrbtnSzChngVariance[newRow,"variance"] <- sd(nsample$Age,na.rm = T)**2
  }
}
gSamplingSizeVariance <- smplngDstrbtnSzChngVariance %>%
  plot_ly(
    x = ~variance,
    frame = ~sizes,
    type = "histogram"
  )
gSamplingSizeVariance
#-------Additional Graphs-----
gSurvivalAgeDensity <- ggplot(df[(!is.na(df$Survived) & !is.na(df$Age)),],
            aes(x = Age,
                fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=as_factor(Survived))) +
  labs(title="Survival density and Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme_ipsum()
gSurvivalAgeDensity


ggplot(train[!is.na(train$Survived),], aes(x = as_factor(Survived), fill = as_factor(Survived)),) +
  geom_bar(stat='count') +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..)) +
  scale_fill_discrete(name = "", labels = c("Didn't Survive","Survived")) + 
  theme_ipsum() 

p1 <- ggplot(train, aes(x = as_factor(Sex), fill = as_factor(Sex))) +
  geom_bar(stat='count', position='dodge') + theme_ipsum() +
  labs(x = 'All data') +
  geom_label(stat='count', aes(label=..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))

p2 <- ggplot(train[!is.na(train$Survived),], aes(x = as_factor(Sex), fill = as_factor(Survived))) +
  geom_bar(stat='count', position='dodge') + theme_ipsum() +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))

gridExtra::grid.arrange(p1,p2, nrow=1)

#-----------Q21-22---------------

# Q-21
age_male <- train %>%
  select(Age,Sex) %>%
  mutate(Sex = as_factor(Sex)) %>%
  filter(Sex == "male")
age_female <- train %>%
  select(Age,Sex) %>%
  mutate(Sex = as_factor(Sex)) %>%
  filter(Sex == "female")

sample_age_male_50 <- age_male %>%
  rep_sample_n(size = 50,reps = 15000,replace = T) %>%
  summarise(age_male_bar = mean(Age,na.rm = T))
sample_age_female_50 <- age_female %>%
  rep_sample_n(size = 50,reps = 15000,replace = T) %>%
  summarise(age_female_bar = mean(Age,na.rm = T))

samplediff_means <- sample_age_male_50$age_male_bar -  sample_age_female_50$age_female_bar %>%
  as_tibble()

gsamplediff_means <- samplediff_means %>%
  ggplot(aes(value, y = ..density..)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  geom_density(fill=inferno(1,begin = 0.5,alpha = 0.5),color = inferno(1,begin=0)) +
  ggtitle("Distribution") +
  theme_ipsum_rc()
gsamplediff_means

# Q22
survived_male <- train %>%
  select(Survived,Sex) %>%
  filter(Sex == "male")

survived_female <- train %>%
  select(Survived,Sex) %>%
  filter(Sex == "female")

sample_survive_male_50 <- survived_male %>%
  rep_sample_n(size = 50,reps = 15000,replace = T)
sample_survive_female_50 <- survived_female %>%
  rep_sample_n(size = 50,reps = 15000,replace = T)
samplediff_survived <- sample_survive_male_50$Survived - sample_survive_female_50$Survived %>% 
  as_tibble()

gsamplediff_survived <- samplediff_survived %>%
  ggplot(aes(value)) +
  geom_histogram(bins = 25,binwidth = 1,color=inferno(1,alpha=1)) + 
  ggtitle("Distribution") +
  theme_ipsum_rc()
gsamplediff_survived
# 0 - 1 = - 1 means that female survived
# 0 - 0 =   0 means that neighter surived
# 1 - 0 =   1 means that male survived
#--------Q23,24--------
# Taking Sample of Size 10 to Calculate The Mean Interval
n_size = 10
nsample <- sample_n(train,size= n_size,replace=T)

# Calculate The Mean
mean_est1=mean(nsample$Age,na.rm = T)

# Calculate The Standard Deviation
sd_est1=sd(nsample$Age,na.rm = T)

# Computing The Error Using the qnorm() Function to Calculate The Normal Distribution 
error=qnorm(0.975)*(sd_est1/sqrt(n_size))

#Determining The Mean Interval[]
mean_est1-error
mean_est1+error

# Taking Sample of Size 50 to Calculate The Mean Interval
n_size = 50
nsample <- sample_n(train,size=n_size,replace=T)

# Calculate The Mean
mean_est2=mean(nsample$Age,na.rm = T)

# Calculate The Standard Deviation
sd_est2=sd(nsample$Age,na.rm = T)

# Computing The Error Using the qnorm() Function to Calculate The Normal Distribution 
error=qnorm(0.975)*(sd_est1/sqrt(n_size))

#Determining The Mean Interval[]
mean_est2-error
mean_est2+error
  