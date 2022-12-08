# ----- Loading packages ---------
library(tidyverse)
library(viridis)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(hrbrthemes)
library(e1071)
library(mice)
library(statsr)
library(plotly)


# Loading Training Data
train <- read_csv("data/train.csv")

# Loading Testing Data
test <- read_csv("data/test.csv")

# Binding them into a full data frame
df <- bind_rows(train,test)

# Identifying our features
colnames(train)

# Number of missing values in each feature
colSums(is.na(train))

# We can observe that the cabin feature has the most missing values in our traning data, and it's most probably not going to be useful initially, which means that we can drop it


# Summary of Data
summary(train)

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


# Desnity Graphs
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
smplngDstrbtonRpsChng <- tibble()
for(i in numOfSamples){
  for(y in 1:i){
    nsample <- sample_n(train,size=50,replace=T) %>%
      select(Age)
    newRow <- nrow(smplngDstrbtonRpsChng) + 1
    smplngDstrbtonRpsChng[newRow,"reps"] <- i
    smplngDstrbtonRpsChng[newRow,"x_bar"] <- mean(nsample$Age,na.rm = T)
  }
}

gSamplingReps <- smplngDstrbtonRpsChng %>%
  plot_ly(
    x = ~x_bar,
    frame = ~reps,
    type = "histogram"
  )
gSamplingReps

sizes <- seq(20,260,20)
smplngDstrbtonSzChng <- tibble()
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

g

#-------Additional Graphs-----
a <- ggplot(df[(!is.na(df$Survived) & !is.na(df$Age)),],
            aes(x = Age,
                fill = Survived)) +
  geom_density(alpha=0.5, aes(fill=as_factor(Survived))) +
  labs(title="Survival density and Age") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + theme_ipsum()
a

missing_values <- titanic %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")

missing_values

missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red") +
  coord_flip() + # to flip the graph
  theme_bw() + # modify the theme (grid) 
  xlab("Fraction of missing values") +
  theme(axis.title.y = element_blank())

sapply(titanic , function(x) {sum(is.na(x))})

ggplot(titanic[!is.na(titanic$Survived),], aes(x = Survived, fill = Survived),) +
  geom_bar(stat='count',) +
  labs(x = 'How many people died and survived on the Titanic?') +
  geom_label(stat='count',aes(label=..count..), size=7) +
  theme_grey(base_size = 18)

p1 <- ggplot(titanic, aes(x = Sex, fill = Sex)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'All data') +
  geom_label(stat='count', aes(label=..count..)) +
  scale_fill_manual("legend", values = c("female" = "pink", "male" = "green"))

p2 <- ggplot(titanic[!is.na(titanic$Survived),], aes(x = Sex, fill = Survived)) +
  geom_bar(stat='count', position='dodge') + theme_grey() +
  labs(x = 'Training data only') +
  geom_label(stat='count', aes(label=..count..))

grid.arrange(p1,p2, nrow=1)