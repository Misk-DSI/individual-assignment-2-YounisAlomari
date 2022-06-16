# Loading packages
library(tidyverse)
library(here) # side note 1: I don't use Docker, and hence, the getwd() will not 
              #              give the same directory as in the instructions
              # side note 2: I think it is redundant to use here() as Rproj is doing the same job
library(janitor)
library(GGally)
library(reactable)
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())

# init here()
i_am("individual-assignment-2-YounisAlomari.Rproj")

# loading the data from the Data folder, obviously on my machine:$
redwine <- read.csv(here("data/winequality-red.csv"))

# Visualizing our loaded data
# We can use head() to do that, but glimpse() has the advantage of reporting the type of values and the dimensions
glimpse(redwine)

# Here we start with summary() to illustrate the results prior to cleaning the data
# glimpse() showed that the variable "quality" is of <int> type
# which indicate that it is a discrete (categorical) variable, however, we will include it 
# in the summary() and deal with its nature later, stay tuned! 
summary(redwine)

# Cleaning the names of the variables
# The clean data will e assigned to the "credwine" which stands for clean red wine
credwine <- redwine %>%
  clean_names()
glimpse(credwine)

# It is obvious that the pH column 9 has been separated to 2 words, which shouldn't be the case, 
# a modification is needed names() is used to assign the new name for column 9, aka pH
names(redwine)[9] <- "ph" 
credwine <- redwine %>%
  clean_names()
glimpse(credwine)

# Second to naming, we have the questions of duplication in the data
# For duplication, get_dupes() is used
credwine %>% 
  get_dupes() %>%
  filter(dupe_count > 2)
# We have 460 observations that are duplicated, 52 of which have more than 2 duplication

# To clean the data, distinct() is used
credwine <- distinct(credwine)

# To check, re-run the get_dupes() again 
credwine %>% 
  get_dupes() %>%
  filter(dupe_count > 2)
# Zero duplication, as should be!
# Or we can alternatively us dim() to see the actual changes in dimensions
dim(redwine)
dim(credwine)
# We can see the reduction in dimension, which is intuitive!

# For curiosity, let's compare summary() results for both the clean and messy data
# by using all.equal() to see the change
# We enforce 3 digits to have exactly the same number of characters as summary() returns a table of characters
# 3 digits enforcement will either round-off or truncate the numbers, but we will have a real sense of the change
all.equal(summary(redwine, digits = 3), summary(credwine, digits = 3))
# We have 11 mismatches, which mean we have AT LEAST 11 parameters that have been changed after cleaning!
# We said "AT LEAST" as the 3 digits enforcement will oppress any change in the 4th digit or further
# which might happen to some variables

# Now we show compare()'s ACTUAL results to see the actual change
summary(redwine)
summary(credwine)

# Now if you remember, we said in the beginning that the variable "quality" is discrete
# We might have interpreted the mean given by previous summary() as the most probable category of the red wine
# but this isn't necessarily true, alas from being illogical as we don't have 5.636 category,hence, we need to find it.
# The basic definition of probability is to divide the number of observations of a category 
# over the whole number of observations
# count() will give us the number of occurrence of each value
# mutate() will be used to aggregate the probability
# The probability will be stored in pcredwine, were p stands for "Probability"
credwine %>% 
  count(quality) %>% 
  mutate(probability = n / nrow(credwine)) -> pcredwine

# We use ggplot() with geom_bar() to illustrate the probability from the previous step
ggplot(pcredwine, aes(quality, probability)) +
  geom_bar(stat="identity") 

# We now will start correlating our variables with each other using ggcorr() method and plot a heatmap
ggcorr(credwine, label = TRUE, hjust = 0.75, size = 2.5, color = "black")

# Based on the correlation map above, there aren't any strong correlations between the variables
# meaning, all the variable can convey a significantly non-redundant meaning to the wine quality 
# and here we will be examining all of them.
# Using ggarrange() from ggpubr library, we combined all the fits together in one graph
# but first we have to store each graph in a variable to use it
# the naming is the first 3 letters if one word (except pH "ph")
# preceded by q2 to denote it is plotted with respect to quality


# First the variables of positive correlations
q2alc <- ggplot(credwine, aes(quality, alcohol)) +
           geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
           geom_smooth(method=lm, se=FALSE, colour = "red")

q2sul <- ggplot(credwine, aes(quality, sulphates)) +
           geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
           geom_smooth(method=lm, se=FALSE, colour = "red")

q2fa <- ggplot(credwine, aes(quality, fixed_acidity )) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")

q2ca <- ggplot(credwine, aes(quality, citric_acid)) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")

positive_corr <- ggarrange(q2alc, q2sul, q2ca, q2fa,
                    ncol = 2, nrow = 2)
positive_corr

#Second the variables of negative correlations
q2ph <- ggplot(credwine, aes(quality, ph)) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")

q2den <-  ggplot(credwine, aes(quality, density)) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")

q2fsd <- ggplot(credwine, aes(quality, free_sulfur_dioxide)) +
           geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
           geom_smooth(method=lm, se=FALSE, colour = "red")

q2tsd <- ggplot(credwine, aes(quality, total_sulfur_dioxide)) +
           geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
           geom_smooth(method=lm, se=FALSE, colour = "red")

q2chl <- ggplot(credwine, aes(quality, chlorides)) +
           geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
           geom_smooth(method=lm, se=FALSE, colour = "red")

q2rs <- ggplot(credwine, aes(quality, residual_sugar)) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")
        
q2va <- ggplot(credwine, aes(quality, volatile_acidity)) +
          geom_jitter(width = 0.25, alpha = 0.5, colour = "blue") +
          geom_smooth(method=lm, se=FALSE, colour = "red")
        

negative_corr <- ggarrange(q2ph, q2den, q2fsd, q2tsd, q2chl, q2va,
                    ncol = 2, nrow = 3)
negative_corr