#
# ------------ Mastering R: Best Practices and Essential Tools ----------- #
#
# This script:
# - Teaches you how to use purrr to iterate over lists
# - Compares for loop vs purrr::map()
# - Use of anonymous functions
# ------------------------------------------------------------------------ #

# 1. Load packages --------------------------------------------------------

pak::pak("ranger")

library(tidyverse)
library(ranger)

# 2. Load data ------------------------------------------------------------

iris_tbl <- as_tibble(iris)

# 3. Simple example -------------------------------------------------------

## Iterate over the list to create the message "This fruit is a <fruit_name>"
objects_list <- list("peach", "pear", "cherry", "strawberry", "blackberry")

## 3.1. Using for loop ---------------------------

## Create an empty list
messages_list <- list()

for (i in 1:length(objects_list)){
    
    messages_list[[i]] <- str_glue("This is a {objects_list[[i]]}")
}

## 3.2. Using functional programming -------------

messages_list <- map(
    objects_list,
    \(fruit) str_glue("This fruit is a {fruit}")
)


# 4. Model by species -----------------------------------------------------

## Create a different linear model for each Iris species

## 4.1. Using for loop ----------------------------

## Unique Species
species_vec <- unique(iris_tbl$Species)

## Create empty list
filtered_lst <- list()

## Iterate to filter each species
for (species in 1:length(species_vec)){
    ## Filter observations
    iris_filtered <- iris_tbl |> 
        filter(
            Species == species_vec[species]
        )
    ## Calculate linear model
    iris_filtered_lm <- lm(Petal.Length ~ Sepal.Length, data = iris_filtered)
    ## Get summary
    filtered_lst[[species]] <- summary(iris_filtered_lm)
}


## 4.2. Using functional programming --------------

## Create a function


## Create list


## Iterate over the list


## 4.3. In one step ---------------------------------


## 4.4. Using anonymous function --------------------


# 5. Iterating 2 inputs --------------------------------------------------

## 5.1. Simple example ----------------------------

## Sum over the lists
fruits <- list("peach", "pear", "cherry", "strawberry", "blackberry")
colors <- list("orange", "green", "red", "red", "black")

## Iterate to create the sentence


## 5.2. A bigger example --------------------------

## Create a grid of parameters


## Create a Random Forest model for each parameter


## Extract r.squared


## Add as a new column


