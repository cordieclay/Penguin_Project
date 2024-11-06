library(here)
library(tidyverse)
library(janitor)
library(palmerpenguins)
library(ggplot2)

source(here("functions", "cleaning.R"))
source(here("functions", "plotting.R"))

#instead of setting working directory, use the here command
here::here()
#this will return our path, but means we can send our code to someone else and it will return their own code
#now laod the penguin data in 
data("penguins")

#look at first six rows
head(penguins_raw)

#the columns are poorly formatted, this is what we are going to try to change. 
#they have mixed capitals, and lower case letters, there are space in between words
colnames(penguins_raw)

#as best practice, before we do anything, we save this file as penguins_raw.csv and think of it as read only
#this means it is preserved as it is before we start meddling with it
#to do this we save penguins_raw as a csv file
#make a file in the files tab, within this project, called data and keep it in there
write.csv(penguins_raw, here("data","penguins_raw.csv"))

#this means that we are turning the penguins_raw data into a csv, and storing it where we are right now, and then in data, and keeping it under this name penguins_raw.csv

#we now want to remove columns, or comments or whatever
#we can do this by using select and a hyphen to remove it
#to remove comments the code would be penguins_clean <- select(penguins_raw, -Comments)
#we could then remove things which start with delta, by doing penguins_clean <- select(penguins_clean, -starts_with("Delta))
#BUT this would overwrite the original data frame, and then overwrite it again. 
#if you were to run this twice you would get an error
#instead we use pipes to do this in one go

penguins_clean <- penguins_raw %>% 
  select(-Comments) %>%  
  select(-starts_with("Delta")) %>% 
  clean_names()

#then look at the column names of penguins_raw and penguins_clean to compare

colnames(penguins_raw)
colnames(penguins_clean)

#save penguins_clean as its own csv
write.csv(penguins_clean, here("data", "penguins_clean.csv"))
penguins_clean <- read.csv(here("data", "penguins_clean.csv"))

#we can then make functions which mean we don't need to copy and paste the same code each time 
#turn the pipe into a function like this:
cleaning_penguins_columns <- function(raw_data){
  print("cleaned names, removed comments, removed empty rows and cols, removed delta. ")
  raw_data %>% 
    clean_names() %>% 
    remove_empty(c("rows", "cols")) %>% 
    select(-starts_with("Delta")) %>% 
    select(-comments) %>% 
    remove_NA() %>% 
    shorten_species()
}
#here we have said, we are making a function, into which we input data (raw_data)
#we are calling this function cleaning_penguins_columns() so when we put our data in after this, it will run this function under that name instead of typing it all out
#in this function we are saying
#1. we say print to say what it is which each of these things are doing and it will tell us after we've run it if its someone else code
#2. we are cleaning names
#3. then we are removing empty rows and columns from the data
#4. then we are selecting and removing things which start with a delta
#5. then we are selecting and removing comments
#6. then we are removing NAs
#7. then we are shortening species names
#8. finally we are removing columns
#all these functions have been coded elsewhere, in cleaning R
#make a new file in the project called functions, and add cleaning R into that
file.create(here("functions", "cleaning.R"))

#then we can run this function we made, which is made up of functions from cleaning R, on our data

penguins_clean_pipeline <- cleaning_penguins_columns(penguins_raw)

view(penguins_clean_pipeline)

#look at the column headers now after this code
colnames(penguins_clean_pipeline)

#now save this version done through the pipeline as a csv in data
write.csv(penguins_clean_pipeline, here("data", "penguins_clean_pipeline.csv"))

renv::init()

#start with a box plot, exploratory figure, raw data, no assumption, or statistics over the top
#box plot, histograms, violin plot etc
#not putting models on top, no lines on it etc, useful when you start your analysis because you can see your data 
#make sure it looks how you expect
penguins_clean <- read_csv(here("data", "penguins_clean.csv"))

#need to make box plot, looking at flipper data
flipper_boxplot <- ggplot(
  data = penguins_clean,
  aes(x = species,
      y = flipper_length_mm)) +
  geom_boxplot()
flipper_boxplot
#need to clean our data because we got a warning message, with 2 rows with NA
#only remove missing values from the columns we're interested in
#big column of data, missing values everywhere but only change the ones we're interested in, because it will delete a bunch of data
#that we're not even looking at
#going to subset the data, making new variable called penguins flippers
#here we make a new data set, with just species and flipper length, then we want to pipe this and say with this subset remove NAs 
#then we shortened the species names, using the function which we were given last time, this is in cleaning R
#call that penguins_flippers

penguins_flippers <- penguins_clean %>% 
  select(species, flipper_length_mm) %>% 
  drop_na() %>% 
  shorten_species()

head(penguins_flippers)

#do a boxplot on new data, without NAs
flipper_boxplot_2 <- ggplot(
  data = penguins_flippers,
  aes(x = species,
      y = flipper_length_mm)) +
  geom_boxplot()
flipper_boxplot_2

#now its just very ugly, we want to make it better
#don't want to change everything, but add colours firstly, in the geom boxplot bit, then remove legend, then change the width, then as a separate thing we will add the jitter, which shows the variance
#jitter gives a scatter over the top, according for each of the species, this means that each catter point gets a random x value within the species width to make the markers more visible
#we dont want jitter to change the disribution of the points each time, so we change the position of the dots, and we make the seed = 0 so they don't change each time
#add colour and remove the legend for this again
#this makes the randomness the same each time
#make the alpha, transparency 0.3, which will make the scatter points more transparent
#want to make the colours pruple, green and orange so we need a new variable to do this

species_colours <- c("Adelie" = "darkorange",
                     "Chinstrap" = "purple",
                     "Gentoo" = "cyan4")

flipper_boxplot <- ggplot(
  data = penguins_flippers,
  aes(x = species,
      y = flipper_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3,
        show.legend = FALSE) +
  geom_jitter(aes(color = species),
              alpha = 0.3,
              show.legend = FALSE,
              position = position_jitter(
                width = 0.2,
                seed = 0)) + 
  scale_color_manual(values = species_colours) +
  labs(x = "Penguin species",
       y = "Flipper length (mm)") +
  theme_bw()
flipper_boxplot

#want to define these changes as a function, so we can change every plot into this format

plot_boxplot <- function(data, 
                         x_column,
                         y_column,
                         x_label,
                         y_label,
                         colour_mapping) {
  #drop the NAs
  data <- data %>% 
    drop_na({{y_column}})
  #now make the plot
  flipper_boxplot <- ggplot(
    data = data,
    aes(x = {{x_column}},
        y = {{y_column}},
        colour = {{x_column}})) +
    geom_boxplot(aes(color = species),
                 width = 0.3,
                 show.legend = FALSE) +
    geom_jitter(aes(color = species),
                alpha = 0.3,
                show.legend = FALSE,
                position = position_jitter(
                  width = 0.2,
                  seed = 0)) + 
    scale_color_manual(values = colour_mapping) +
    labs(x = x_label,
         y = y_label) +
    theme_bw()
  flipper_boxplot
}

species_colours <- c("Adelie" = "darkorange",
                     "Chinstrap" = "purple",
                     "Gentoo" = "cyan4")

plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper length (mm)")
