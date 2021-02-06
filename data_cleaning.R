library(tidyverse)


#starting by knowing our data

menu = read_csv("menu.csv", )

glimpse(menu)
# looks like we have 260 raws and 24 columns 


# looks like we have 3 character variables: 
# Category: the category of each item such as Breakfast - Beef & Pork - Desserts - Salads - etc.
# Item: the name of the item
# Sercing Size: the size of the item in oz and g ex. 10.7 oz (302 g)

# looks like that the rest 21 variables are numeric: 
# Calories: how much energy you get - recommended 2000 cal per day - unit: cal
# Calories from fat: energy you get from fat - unit: cal
# Total Fat - unit: g
# Saturated Fat - unit: g
# Trans fat - unit: g
# Cholestrol - unit: mg
# Sodium - unit: mg
# carbohydrates - unit: g
# Dietary Fiber - unit: g
# Sugars - unit: g
# Protein - unit: g
# Vitamin A - %DV
# Vitamin C - %DV
# Calcium - %DV
# Iron - %DV




# Data cleaning

# convert spaces in column names into _
names(menu) = str_replace_all(names(menu)," ", "_")
names(menu)

# check if any column contains na
sum(is.na(menu)) # = 0 meaning that we don't have any na in our dataset 
# and by observing the data we don't have any special character represeting the na like "?", "-", etc.

# drop "Calories_From_Fat" as it's already included in "Calories"
menu = menu %>%
            select(-Calories_from_Fat)

glimpse(menu)

# now let's save the cleaned data into "cleaned.csv"
write_csv(menu, "cleaned.csv")
