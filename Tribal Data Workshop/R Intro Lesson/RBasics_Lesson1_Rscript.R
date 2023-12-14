#### Top material --------------------------------

# Tribal Data Academy 2023
# R Basics: Lesson 1
# 27 June 2023

# Course materials repository: https://drive.google.com/drive/folders/10So8tm4EoefZZbmCOnKHhmJC7fY0Vu9y?usp=sharing
  
# Data file for lesson: "RBasics_Lesson1_Data"

  
#### 1. Working with R Scripts --------------------

###### 1.1 Commenting & running code --------------

# Hashtags indicate comments, they will not be treated as code



# Place your cursor anywhere in the line of code that you want to run
# Then hit 'CTRL + Enter' (Windows/Linux) or 'Cmd + Enter' (Mac) to run that line of code


# Remove hashtags from code that's been 'commented out' if you want to run it

# 3+3


###### 1.2 Navigating R Scripts: headings ---------

# Adding hashtags on either side of a title/heading, or trailing dashes/hashtags, allows you to divide your script into subsections


# Example heading 1 ----

# Example heading 2 ####

#### Example heading 3 ----

#### Example heading 4 ####


###### Example subheading 1 ----

###### Example subheading 2 ####


# Use headings to collapse code sections and to navigate quickly (using menus at the bottom or on the side)

# There's also a keyboard shortcut for headings: 'CTRL + Shift + r'


###### 1.3 Saving R Scripts -----------------------

# Rule zero of coding: save your code frequently



#### 2. Coding Basics ---------------------------

###### 2.1 Basic operators ----------------------

# plus: +
# minus: -
# multiply: *
# divide: /
# power: ^

# 2*5

# 4^2

# R will auto-complete parentheses



# R will auto-wrap parentheses around highlighted code, too

# 2+6 *2


###### 2.2 Assigning variables -------------------

a <- 2
# a


longer <- (2+6)*2
# longer


combo <- a + longer
# combo

# Variables will show up in the 'Environment' pane

# In R, variable names are case-sensitive

# Variable names that are commonly avoided because they show up in functions: c, i, x, y


###### 2.3 Function syntax ----------------------

# Syntax for how functions are written: function( inputs/arguments )

round(pi, 5)

round(digits = 12,x=pi) #or you can also express this this way 

?round #putting a question mark in front of a function will open the function in the help window

# You can apply functions to outputs from other functions

seq(1:20)
seq(from=0, to=30, by=3)

some_numbers <- c(seq(1:20), seq(from=0, to=30, by=3))
some_numbers


# For longer functions, you can separate portions onto different lines in an R script for ease of reading

easier_numbers <- sort(c(seq(1:20), seq(from=0, to=30, by=3)))




#### 3. Importing CSV Data ------------------------

###### 3.1 Basic function & Full file paths -------

# Function syntax: VariableName <- read.csv( "File Path", etc. )

# Full file paths start with a root/base drive on your computer
# On Windows, this is usually 'Local Disk (C:)'

data <- read.csv("/Users/khadijahhomolka/Documents/Tribal Data Workshop/RBasics_Lesson1_Data.csv")


###### 3.2 Working directory & Relative file paths -----

# A working directory is a folder that R will use as the base/starting folder for file paths in our code

getwd()


# Set your working directory automatically by clicking directly on your R script to open it (versus launching RStudio first)

# To change your working directory, use 'setwd()' or select the folder in the 'Files' pane and click on the Gear icon to choose 'Set As Working Directory'



# Relative file path syntax: LessDat <- read.csv("./ ")
# "LessDat" = short for "Lesson Data"
LessDat <- read.csv("./RBasics_Lesson1_Data.csv")

# Use 'TAB' (key on keyboard) to complete file paths quickly and accurately (it also works for function inputs!)
                           

# CSV data will be imported as a data frame by default

# The imported data is a copy of the data in the original CSV spreadsheet; it is not the original CSV itself

# The newly imported data should be visible in the 'Environment' pane of RStudio

# Note the "Import Dataset" feature in that same pane (lists other document types that can be imported, but it's not a comprehensive list)


#### 4.0 Exploring Data -----------------------------

###### 4.1 Basic syntax & functions ------------------

# View the first few rows and the last few rows

head(LessDat)

tail(LessDat)

# Notice the indexing numbers on the far left



# View the column names

colnames(LessDat)



# Get a general overview of the structure of the dataset

str(LessDat)

summary(LessDat)



# Syntax for accessing individual columns: Dataset$Column

LessDat$Dia_cm

LessDat$Species #trying another variable

max(LessDat$Dia_cm) #this returns an NA, see below


###### 4.2 Missing data -----------------------------

# Missing data (values) are represented by "NA"

# "NA's" hold the place of missing values in a data frame (like maintaining the empty cells in a spreadsheet)


# Example of plant heights by year:

plant_heights <- c(2, 4, 6, 8, NA, 12)
plant_heights

length(plant_heights) #this will tell you have many values are present, i.e. here it would represent how many years trees were sampled

mean(plant_heights)

mean(plant_heights, na.rm=TRUE) #include na.rm=TRUE to disregard/ignore the NAs (it is not removing or treating NA as 0)


# Does our "Dia_cm" column have "NA" values?

summary(LessDat$Dia_cm) #yes, it has 1 NA

max(LessDat$Dia_cm, na.rm=TRUE)


###### 4.3 Data types -------------------------------

# One method to view data types is to revisit 'str()'

str(LessDat)
                               

# Some common data types:

# integer ("int")
# numeric ("num")
# character ("chr")
# logical/boolean (TRUE or FALSE, 1 or 0)


# Examining data types can help with error-checking

# Notice that the height column ("Ht_m") is being listed as a character type, not a numeric type

# This indicates one or more typos somewhere in the column


# When a column contains multiple data types, R converts them all to the lowest common denominator

# In this case, because a number is a type of character, the height column (accidentally a mix of numbers and characters) has been converted to a character data type


#### 5. Error Checking ---------------------------

# Some portion of errors can be prevented by using a formal database (which will only accept specific types and formats of data for a given column)

# But, sometimes you end up needing to check for errors in a dataset that's already been made


###### 5.1 Spelling errors ------------------------

# Use the 'unique()' function to check for spelling errors

unique(LessDat$Species)



###### 5.2 Subsetting & Indexing -----------------

# Sometimes we want to visually review the data that has errors before deciding what correction to make


# Subsetting: extracting a subset of data from a larger set

# Syntax for subsets: dataset[ subset requirements ]

# Syntax for subset requirements:[row, column]


# Practice pulling subsets of data from the dataset:

head(LessDat) #finding the first row of our data

LessDat[1,] #returns the first row

LessDat[1,3] #returns the first row, third column

LessDat[c(3,6),] # c(), the concatenate commmand, allows you to pull multiple rows and/or columns

# The numbers used to represent the rows and columns are called 'index' values. 


# Instead of using index values, we can also use mathematical descriptions based on conditional operators

# greater than, less than: >, <
# greater than or equal to: >=
# equal to: == (works like the assignment operator "<-")
# not equal to: !=
# AND, both conditions are true: &
# OR, at least one of the conditions is true: |


# Spellcheck example: using conditional operators, pull the rows that have the spelling error

LessDat[LessDat$Species == "ABCO ",] #pulling out all the rows that have the spelling error 



# Syntax for subset function: 'subset(dataset, row conditions)' 
# or 'subset(dataset, row conditions, select=c(column names))'

# Out of the first 10 rows in the dataset, pick the rows where the trees are alive ('Live' is "Y")

LessDat[1:10,]

subset(LessDat[1:10,], Live == "Y")


###### 5.3 Replacing values ----------------------

# To subset just an individual column: dataset[column index or "column name"]

# Syntax for replacing an individual value:
#   dataset[column index or "column name"][row, ] <- new value


# Correct the spelling error

unique(LessDat$Species)

LessDat[LessDat$Species == "ABCO ",] #like above, checking for the rows that have the spelling error

LessDat["Species"][LessDat$Species == "ABCO ",] <- "ABCO" #function for replacing the spelling error

unique(LessDat$Species) #checking if the correction worked


#### 6. Other Basic Data Operations (optional) ------

###### 6.1 Adding & removing columns ----------------

# To add a new column: use the '$' syntax, name the column, and fill in the values

head(LessDat)

LessDat$NewColumn <- "values" #naming the new column

LessDat$Radius <- LessDat$Dia_cm/2 #wanting to add a coumn for tree radius, can use a previous column to do that calculation and put it into a new row

head(LessDat) #see if now gives the radius column


# To remove a column, assign it the value 'NULL'

LessDat$NewColumn <- NULL #will remove a column by assigning that column name a null operator

head(LessDat)


###### 6.2 Workflow ------------------------------

# Test your code on small portions of your dataset first
# i.e., use small subsets of your data as "test" datasets when you're trying to code something new

# If your code only applies to certain columns, etc.:

large_trees <- LessDat[LessDat$Dia_cm > 50,][1:20,]

large_trees
# testdat<- subset(LessDat[1:20,], Dia_cm > 50, select=c(Species, Dia_cm, Live))

large_trees <- subset(large_trees, select=(c(Species, Dia_cm, Live)))
large_trees

# testdat

# Break your code down into sub-components and work up to full complexity

# LessDat[1:20]

# LessDat$Dia > 50

# subset(LessDat[1:20,], select=(Species, Dia_cm, Live))

# subset(LessDat[1:20,], Dia_cm > 50, select=c(Species, Dia_cm, Live))


###### 6.3 Revisiting character vs. number error -------

str(LessDat)
# The column 'Ht_m' (height in meters) should be a numeric data type, but it is reading as character

# How to figure out what the error is?
# How to convert the data type from character to numeric?

# This is one solution I came up with, there's inevitably something more elegant out there
# See if you can spot some of the things we've covered today!

# Add a test column that's a copy of the height column
LessDat$chr_test <- LessDat$Ht_m
head(LessDat)

# Try converting the characters to numbers
# Values that can be converted will be, values that have an issue will return 'NA' (and those are the errors we're looking for)
LessDat$chr_test <- as.numeric(LessDat$chr_test)
head(LessDat)

# Pull (subset) all the rows with 'NA' values in the 'chr_test' column using the function 'is.na()'
LessDat[is.na(LessDat$chr_test),]

# In the row(s) identified as having an error, determine the correction(s) and replace the height column value accordingly
LessDat$Ht_m[1]
LessDat$Ht_m[1] <- 7.5

LessDat$Ht_m <- as.numeric(LessDat$Ht_m)

# Double check the results
str(LessDat)

# Remove the test column 
LessDat$chr_test <- NULL


#### 7. Libraries & packages -----------------------

###### 7.1 Installing packages ---------------------

# Libraries are collections of (additional) functions, and you obtain them by installing them as 'packages'

# Syntax for installing packages: install.packages("package name")
# Often it's best to type this directly into the console


# Try installing the package "dplyr"
# install.packages("dplyr")


# There's a built-in function in R ('aggregate') that allows you to apply a function to a dataset, but split up by some category

# Example: calculate the average (mean) diameter of trees by species
# aggregate(LessDat$Dia_cm, by=list(LessDat$Species), FUN=mean, na.rm=TRUE)


# In the 'dplyr' library, there's a version of this procedure called "group by" that can be easier to read

# 'dplyr' and libraries like it utilize a function called a 'pipe' (symbolized by '%>%') to pass the outputs from one step onto the next step

# This kind of work flow makes it easier to pass data through a longer series of functions without having to write it as an ever-expanding onion of nested functions & parentheses

LessDat %>%
  group_by(Species) %>%
    summarise(
      Avg.Dia = mean(Dia_cm, na.rm=TRUE)
      )

# Did it work for you?
# If not, it means that RStudio doesn't have access to your newest library just yet

# Load the 'dplyr' library first, then try running the code again
# library(dplyr)


# It's not uncommon to list the necessary libraries at the top of your R scripts so that readers can load them upfront if necessary 

# If you open a script and don't have the necessary libraries, RStudio will ask automatically if you want to install packages


###### 7.2 Set-up for Day 2 ------------------------------------

install.packages(c("knitr", "plotly", "lubridate", "tidyverse", "dataRetrieval"))

library(knitr)
library(plotly)
library(tidyverse)
library(dataRetrieval)
library(lubridate)

  
#### 8. Finishing Work Sessions --------------------

###### 8.1 Exporting data to CSV ---------------------

# Syntax for function: write.csv(data frame, file = "file path, including name-of-new-CSV.csv")

# Optional: if you don't want the index values for all the rows to be included, then include the input: row.names = FALSE

write.csv(LessDat, file = "./ExportedData.csv", row.names = FALSE)



###### 8.2 Save your R script! ---------------------

# Good job!


###### 8.3 Workspace images: to save or not to save? ------

# A workspace image is a snapshot or save-in-progress of your current work session in RStudio

# When you are shutting down RStudio, if you choose to save your workspace image, then you will be saving all of your variable names and their values

# You can set your preference for whether or not to save by going to Tools --> Global Options --> Workspace --> Save workspace to .RData on exit
# My suggestion: set your preference to "Ask"

# If you choose not to save your workspace image, then your Environment will be cleared, which reduces the likelihood of code/calculation errors in the future


#### 9. Future resources ---------------------------

# Here's a set of suggestions for continuing to learn about R

# 1. The Data Carpentry Intro to R lesson
# Free to access online and covers basic R coding in more detail (it would be a good refresher after this course, plus a good chance to practice)

# https://datacarpentry.org/R-ecology-lesson/index.html


# 2. Stack Overflow
# A place to crowd-source answers to your programming questions (search first to see if an answer's already there, and if not, try posting something yourself)

# https://stackoverflow.com/questions


# 3. R for Excel Users
# There are some nice resources specifically geared around how to learn R if you have a background in Excel

# webpage and book: https://rforexcelusers.com/

# free online course (completely separate authors from the book above): https://rstudio-conf-2020.github.io/r-for-excel/


# 4. Search engines are your friend!
# People who code regularly will (just as regularly) be using the internet to look up information on functions, etc. Knowing how to search for the information you need is a skill in its own right.

# Search engine results will often include posts from Stack Overflow, as well as a lot of webpages put together by individual people in order to teach something.

# When it comes to learning how to do something off of the internet, programming has some of the best resources available.


