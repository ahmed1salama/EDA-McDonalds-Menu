library(tidyverse)

menu = read_csv("cleaned.csv")

glimpse(menu) # 260 x 23


# "Category" and "Item" needs to be factor
menu = menu %>%
  mutate(Category = as.factor(Category),
         Item = as.factor(Item))



# Questions that need to be answered:
# How many calories does the average McDonald's value meal contain?
# How much do beverages, like soda or coffee, contribute to the overall caloric intake? 
# Does ordered grilled chicken instead of crispy increase a sandwich's nutritional value? 
# What about ordering egg whites instead of whole eggs?
# What is the least number of items could you order from the menu to meet one day's nutritional requirements?


# EDA

############################################ Question 1 ####################################################
# How many calories does the average McDonald's value meal contain?

# let's first see how many Items in each category 
menu %>%
  count(Category) %>%
  ggplot(aes(x = fct_rev(fct_reorder(Category,n)), y = n)) +
  geom_col(fill = "deepskyblue4") +
  ggtitle("Number of Items in Each Category") +
  labs(y= "Count", x = "Category") +
  geom_text(aes(label=n), vjust = -0.5) 


  

# looks like that "Coffee&Tea" has the most items and "Salad", "Desserts"  have the least items 

# now let's explore the average calories from each category
mean_cal = round(mean(menu$Calories),2) # average calories in the whole menu 

menu %>%
  group_by(Category) %>%
  summarize(mean_cat_cal = round(mean(Calories), 2)) %>%
  ggplot(aes(fct_rev(fct_reorder(Category, mean_cat_cal)), mean_cat_cal)) +
  geom_col(fill = "deepskyblue4") +
  geom_hline(yintercept = mean_cal, linetype = "dashed")+
  ggtitle("Average Calories in Each Category") +
  labs(y= "Avg. Cal", x = "Category") +
  geom_text(aes(label=mean_cat_cal), vjust = -0.5) +
  geom_text(aes(8, mean_cal, label=str_c("Menu Average = ", as.character(mean_cal)), vjust = -0.5),color = "darkred")

# looks like that the Categories having the most calories are: 
# "Chicken & Fish" - "Smoothies & Shakes" - "Breakfast" - "Beef & Pork"

# looks like that the Categories having the least calories are: 
# "Coffee & Tea" - "Salads" - "Snacks & Sides" - "Desserts" - "Beverages


# now let's explore calories in each item per category

Items_Calories_Draw <- function(Cat_name){
      menu %>%
      filter(Category == Cat_name) %>%
      ggplot(aes(x = fct_reorder(Item, Calories),y = Calories)) +
      geom_col(fill = "deepskyblue4") +
      coord_flip() +
      ggtitle(str_c("Calories of Each Item in Categoty ", Cat_name))+
      xlab("Item")
}
lapply(levels(menu$Category), Items_Calories_Draw)

# from the above figures we can identify the highest items and lowest items in each category w.r.t their Calories


# Category "Beef & Pork":
# highest calories: Double Quarter Pounder with Cheese - Bacon Clubhouse Burger - Quarter Pounder with Bacon Habanero Ranch 
# lowest Calories: McDouble - Cheeseburger - Hamburger


# Category "Beverages":
# highest calories: Any "large" size non-Diet sparkling water like "sprite" and "Coca-Cola" and Minute Maid Orange Juice(Large)
# lowest Calories: Any Diet sparkling water like "Diet Dr Pepper" and "Diet Coke" in all sizes and Dasani Water Bottle (0 Calories)


# Category "Breakfast":
# highest calories: Big Breakfast with Hotcakes with and without Egg Whites in all sizes of Biscuit
# lowest Calories: Sausage Burrito - Egg McMuffin - Fruit & Maple Oatmeal - Egg White Delight - Hash Brown


# Category "Chicken & Fish":
# highest calories: Chicken McNuggets 40 pcs has the highest calories followed by the 20 pcs and the Bacon Clubhouse Crispy Chicken Sandwich
# lowest Calories: All remaining items have almost the same calories and the least is Chicken McNuggets 4 pcs


# Category "Coffee & Tea":
# highest calories: Frappe Chocolate chip - Frappe Mocha - Frappe Caramel have the most calories in all sizes
# lowest Calories: Ice Tea - Coffee in all sizes (0 Calories)


# Category "Salads":
# highest calories: Premium SouthWest Salad with Crispy Chicken - Premium Bacon Rnach Salad with Crispy Chicken - Premium SouthWest Salad with Grilled Chicken 
# lowest Calories:  Premium Bacon Rnach Salad with Grilled Chicken - Premium SouthWest Salad without Chicken - Premium Bacon Rnach Salad without Chicken 


# Category "Smoothies & Shakes":
# highest calories: McFlurry with M&M's Candles(Medium) and all the Large Shakes 
# lowest Calories: All Smoothies in Medium and Small sizes 


# Category "Snacks & Sides":
# highest calories: French Fries(Large and Medium) - Ranch Snack Wrap(Crispy Chicken)
# lowest Calories: Fruit'n Yogurt Parfait - Kids French Fries - Side Salad - Apple Slices



############################################ Question 2 ####################################################
# How much do beverages, like soda or coffee, contribute to the overall caloric intake?

# let's get the total calories in the whole menu then see how much do "Beverages" and "Coffee & Tea" contribute
menu_tot_cal = sum(menu$Calories)

# now let's see by how much "Soda" contribute to the overall caloric intake 
Soda_total_cal = menu %>%
                      filter(Category == "Beverages") %>%
                      select(Calories) %>%
                      slice(1:20) %>%
                      sum()
Soda_percent = Soda_total_cal / menu_tot_cal * 100

# now let's see by how much "Coffee" contribute to the overall caloric intake 
Coffee_total_cal = menu %>%
                      filter(Category == "Coffee & Tea") %>%
                      select(Calories) %>%
                      slice(9:n()) %>%
                      sum()
Coffee_percent = Coffee_total_cal / menu_tot_cal * 100

# Soda contributes by 2 percent and Coffee contributes by 27.5 percent 
# one can wonder how can coffee contibute more than soda and by that huge difference 
# but here coffee does not mean black coffee but all kinds of drinks that are made from coffee like "Frappe Mocha" which includes hight calories 



############################################ Question 3 ####################################################
# Does ordered grilled chicken instead of crispy increase a sandwich's nutritional value? 


Chicken_menu = menu %>%
                  filter(Category == "Chicken & Fish", Item != "Filet-O-Fish") 

# now let's add a column which tells if the chicken is grilled or not
Chicken_menu = Chicken_menu %>%
                  mutate(type = case_when(str_detect(Item, "Grilled") == 1 ~ "Grilled",
                                             TRUE ~ "Crispy"),
                         type = as.factor(type))
# now let's compare Grilled vs Crispy chicken w.r.t Calories
ggplot(Chicken_menu, aes(x = type, y = Calories, fill = type)) +
  geom_boxplot()

# from here we can see that the range of calories of grilled chicken sandwiches is smaller than the crispy
# chicken ones, also the average caloies is less 


# now let's make a similar comparison but now w.r.t Total Fat
ggplot(Chicken_menu, aes(x = type, y = Total_Fat, fill = type)) +
  geom_boxplot()
# here also we can see that the range of total fat of grilled chicken sandwiches is smaller than the crispy
# chicken ones, also the average total fat is less 


# now let's make a similar comparison but now w.r.t Protein
ggplot(Chicken_menu, aes(x = type, y = Protein, fill = type)) +
  geom_boxplot()
# here also we can see that the range of Protein of grilled chicken sandwiches is smaller than the crispy
# but the protein values are higher and the average protein value is higher

# so over all grilled chicken is higher in protein but lower in calories and total fat



############################################ Question 4 ####################################################
# What about ordering egg whites instead of whole eggs?
Eggs_menu = menu %>% 
  filter(str_detect(str_to_lower(Item), "egg")) %>%
  mutate(Type = case_when(str_detect(str_to_lower(Item), "white") ~ "Eggs Whites",
                          TRUE ~ "Whole Eggs"),
         Type = as.factor(Type))


# now let's compare Eggs Whites vs Whole Eggs w.r.t Calories
ggplot(Eggs_menu, aes(x = Type, y = Calories, fill = Type)) +
  geom_boxplot()

# from here we can see that the average calories for eggs whites is less than the whole eggs


# now let's compare Eggs Whites vs Whole Eggs w.r.t Total Fat
ggplot(Eggs_menu, aes(x = Type, y = Total_Fat, fill = Type)) +
  geom_boxplot()

# from here we can see that the average total fat for eggs whites is less than the whole eggs


# now let's compare Eggs Whites vs Whole Eggs w.r.t Protein
ggplot(Eggs_menu, aes(x = Type, y = Protein, fill = Type)) +
  geom_boxplot()

# from here we can see that the average protein for eggs whites is more than the whole eggs


# now let's compare Eggs Whites vs Whole Eggs w.r.t Cholesterol
ggplot(Eggs_menu, aes(x = Type, y = Cholesterol, fill = Type)) +
  geom_boxplot()

# from here we can see that the average cholestrol for eggs whites way less than the whole eggs
# looks like that eggs yolk contains very much cholesterol



############################################ Question 5 ####################################################
# What is the least number of items could you order from the menu to meet one day's nutritional requirements?


# Some nutrition facts conducted from --> https://www.fda.gov/food/new-nutrition-facts-label/how-understand-and-use-nutrition-facts-label


# 2,000 calories a day is used as a general guide for nutrition advice
# Nutrients to get less of: Trans fat - SaturatedFat - Sodium - Cholestrol - Added Sugars
# Nutrients to get more of: Carbohydrates - Dietay Fiber - Protein - Vitamins - Calcium - Iron 


# %DV means how much a nutrient contributes to a total daily diet
# 5% DV or less --> low 
# 20% DV or more --> high
# choose foods that are:
#                       higher for Dietary Fiber - Vitamins - Calcium - Iron - Pottasium 
#                       lower for Satureated Fat - Sodium - Added Sugars

# assuming that "meeting one day's nutrational requirements" means 2,000 calories and 100% daily value 
# of the nutrients we want to get more of.
# adn based on the previous information we are gonna sort descendingly the Calories then find the min 
# number of items to sum up tp 2,000 calories then we will analyze those items w.r.t the nutrients we want to 
# get more of like Carbohydrates - Dietay Fiber - Protein - Vitamins - Calcium - Iron
# and the nutrients we want to get less of like saturated fat - trans fat - cholesterol and so on 


meet_calories = menu %>%
                   arrange(desc(Calories))
min_number_items = 0
min_calories = 0
for(i in 1:nrow(meet_calories)){
  min_calories = min_calories + meet_calories[i, "Calories"]
  min_number_items = min_number_items + 1
  if(min_calories >= 2000)
    break
}

meet_calories = meet_calories[1:min_number_items,]

# looks like we can get the 2,000 calories from only two items, let's analyze those items in terms of the 
# the %DV we will get w.r.t the remaining nutritions 

summary_meet_cal = meet_calories %>%
  summarize(sum_total_fat = sum(`Total_Fat_(%_Daily_Value)`),
            sum_saturated_fat = sum(`Saturated_Fat_(%_Daily_Value)`),
            sum_cholesterol = sum(`Cholesterol_(%_Daily_Value)`),
            sum_sodium = sum(`Sodium_(%_Daily_Value)`),
            sum_Carb = sum(`Carbohydrates_(%_Daily_Value)`),
            sum_dietary_fiber = sum(`Dietary_Fiber_(%_Daily_Value)`),
            sum_vitamin_A = sum(`Vitamin_A_(%_Daily_Value)`),
            sum_vitamin_C = sum(`Vitamin_C_(%_Daily_Value)`),
            sum_calcium = sum(`Calcium_(%_Daily_Value)`),
            sum_Iron = sum(`Iron_(%_Daily_Value)`))

values = as.numeric(summary_meet_cal[1,])
nutrients = names(summary_meet_cal)

summary_meet_cal = data.frame(nutrients, values)
summary_meet_cal

# from here we can conclude that even we have met the 2,000 from those two items, those items can affect 
# our health badly as they will provide us with double the % DV of the nutrients we want to get less of
# like Total Fat - Saturated Fat - Cholesterol - Sodium 
# and will not give us the 100% DV we need from the nutrients we want to get more of 
# like carb - vitamins - fibers and so on 
