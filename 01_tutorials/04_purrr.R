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

filtered_lst

## 4.2. Using functional programming --------------

## Create a function
calculate_iris_lm <- function(data) {
    
    lm(Petal.Length ~ Sepal.Length, data = data) |> 
        summary()
}

## Create list
iris_species_list <- iris_tbl |> 
    split(iris_tbl$Species)

## Iterate over the list
map(
    iris_species_list,
    calculate_iris_lm
)

## 4.3. In one step ---------------------------------
iris_tbl |> 
    split(iris_tbl$Species) |> 
    map(calculate_iris_lm)

## 4.4. Using anonymous function --------------------
iris_tbl |> 
    split(iris_tbl$Species) |> 
    map(
        \(data) lm(Petal.Length ~ Sepal.Length, data = data) |> 
            summary()
    )

# 5. Iterating 2 inputs --------------------------------------------------

## 5.1. Simple example ----------------------------

## Sum over the lists
fruits <- c("peach", "pear", "cherry", "strawberry", "blackberry") #can also
#be a vector using c() instead of list()
colors <- c("orange", "green", "red", "red", "black")
rounded <- c("rounded", "not rounded", "rounded", "not rounded", "rounded")

## Iterate to create the sentence
map2_chr(
    .x = fruits,
    .y = colors,
    \(fruit, color) str_glue("The color of the {fruit} is {color}")
)

## Iterate over 3 vectors
pmap(
    .l = list(
        fruits,
        colors,
        rounded
    ),
    \(fruit, color, rounded) str_glue("The color of the {fruit} is {color} and {rounded}")
)

## 5.2. A bigger example --------------------------

## Create a grid of parameters
params_tbl <- expand_grid(
    ntree = c(100, 200, 500, 1000),
    mtry = 1:4 #this is number of variables
)

## Create a Random Forest model for each parameter
iris_rf_list <- map2(
    .x = params_tbl$ntree,
    .y = params_tbl$mtry,
    \(ntree, mtry) ranger(
        formula   = Sepal.Length ~ .,
        data      = iris_tbl,
        num.trees = ntree,
        mtry      = mtry  
    )
)

## Extract r.squared
map_dbl(
    iris_rf_list,
    \(rf_model) rf_model$r.squared
)

## Add as a new column
params_tbl |> 
    mutate(
        rsq = map_dbl(
            iris_rf_list,
            \(rf_model) rf_model$r.squared
        )
    ) |> 
    arrange(desc(rsq))

