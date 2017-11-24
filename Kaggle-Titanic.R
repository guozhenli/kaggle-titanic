# Load packages
library(tidyverse)
library(mice)
library(randomForest)
library(stringr)
library(scales)


# Load data
train <- read_csv('./input/train.csv')
test <- read_csv('./input/test.csv')
full <- bind_rows(train, test)

# Feature Engineering
## Extract title from names
full <- full %>% mutate(Title=str_replace_all(Name, '(.*, )|(\\..*)', ''))
table(full$Sex, full$Title)
## Some titles mean the same thing, like Miss = Ms = Mlle, and Mme = Mrs
full <- full %>% 
    mutate_at(.vars = vars(Title), 
              .funs = funs(ifelse(. %in% c('Mlle','Ms'), 'Miss', .))) %>% 
    mutate_at(.vars = vars(Title),
              .funs = funs(ifelse(. %in% c('Mme'), 'Mrs', .)))
## Combine rare titles to one label 'Rare Title'
title_counts <- table(full$Title)
rare_title <- names(title_counts)[title_counts<10]
full <- full %>% mutate_at(.vars = vars(Title),
                           .funs = funs(ifelse(. %in% rare_title, 'Rare Title', .)))
table(full$Sex, full$Title)
## Extract Surname from Name
full$Surname <- full$Name %>% str_split('[,.]') %>% lapply('[[', 1) %>% unlist() %>% trimws()

## Get family size from numbers of sibling, spouse, parents, children
full <- full %>% 
    mutate(Fsize = SibSp + Parch +1) %>% 
    mutate(Family = paste(Surname, Fsize, sep='_'))
ggplot(data = subset(full, !is.na(Survived)),
       aes(x = Fsize, fill=factor(Survived))) +
    geom_bar(stat = 'count', position = 'dodge') +
    scale_x_continuous(breaks = 1:11) +
    labs(x = 'Family Size') 
## Categorize family sizes into 'singleton', 'small', and 'large'
full <- full %>% mutate(FsizeD = case_when(
    Fsize==1 ~ 'singleton',
    Fsize>4 ~ 'large',
    TRUE ~ 'small'
))
mosaicplot(table(full$FsizeD, full$Survived), main='Family Size vs Survival', shade = TRUE)

## Extract Deck from Cabin
full$Deck <-  full$Cabin %>% substr(1,1) %>% as.factor()


# Deal with Missing Values
## Count NA's
full %>% is.na() %>% colSums()
## Embarked: 2 NA's
selector <- full$Embarked %>% is.na()
View(full[selector, ])
### We can see that these two passengers with missing Embarked variables were in 1st class (Pclass=1), and both paid $80
### Lets' take a look at what fare passengers from different embark point and different class paid
ggplot(data = subset(full, !selector),
       aes(x=Embarked, y=Fare, fill=factor(Pclass))) +
    geom_boxplot() +
    scale_y_continuous()
full %>% filter(!selector, Embarked=='C', Pclass==1) %>% select(Fare) %>% apply(2, median)
### We can see that the fare these passengers paid ($80) is close to the median of what 1st class passengers from C paid, so we infer that they embarded from C (Cherbourg)
full$Embarked[selector] <- 'C'
full %>% is.na() %>% colSums()

## There is a passenger with missing Fare
selector <- full$Fare %>% is.na()
View(full[selector,])
### This is a 3rd class passenger embarked form S
group_selector <- (!selector) & (full$Embarked=='S') & (full$Pclass==3)
ggplot(data = filter(full, group_selector),
       aes(x=Fare)) +
    geom_density(fill='deepskyblue')
### Use median fare of this class group to fill the missing fare
full$Fare[selector] <- full$Fare[group_selector] %>% median()
full %>% is.na() %>% colSums()
