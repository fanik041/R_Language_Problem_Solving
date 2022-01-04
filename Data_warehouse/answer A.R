storeLocation_table <- 
  data.frame(loc_key=sample((x=1:20), 10, replace = T),
             street =c("2067 shinn street",
                       "3375 elm drive",
                       "1885 chenoweth drive",
                       "539 reppert coal road",
                       "1075 Benson Street",
                       "4704 Garafraxa-St", 
                       "4869 Bellwood Acres-Rd",
                       "3295 Old Spallumcheen Rd",
                       "1806 Galts Ave",
                       "392 Haxforth Drive"
             ),
             city_key = sample((x= 21:40), 10, replace = T))

address_table <- 
  data.frame(city_key = storeLocation_table$city_key,
             city = c("Montreal","Toronto","Vancouver","New York","Washington","Ottawa","California",
                      "San Francisco","Boston","Quebec City"),
             country = c("Canada","Canada","Canada","USA","USA","Canada","USA","USA","USA","Canada"))

pizzaSize_table <- 
  data.frame(pizza_key=41:45,
             pizza_size=c("small",
                          "medium",
                          "large",
                          "xlarge",
                          "Xxlarge"
             ),
             pizza_size_value=c("8 inch",
                                "12 inch",
                                "16 inch",
                                "20 inch",
                                "24 inch"
             ))

doughType_table <- 
  data.frame(doughType_key=51:60,
             dough_vendor=c("Dominos",
                            "Pizza hut",
                            "Metro",
                            "Lamonicas",
                            "Panartisan",
                            "Teti Bakery",
                            "The Upper Crust Pizza", 
                            "Metro",
                            "Lamonicas",
                            "Panartisan"
                            
             ),
             dough_blend=c("whole wheat",
                           "wheat regular",
                           "self rising flour",
                           "bread flour",
                           "semolina",
                           "graham flour",
                           "pastry flour",
                           "high-gluten flour",
                           "white regular",
                           "crushed wheat"
             ))

dough_table <- 
  data.frame(dough_key=61:70,
             doughType_key= doughType_table$doughType_key,
             dough_type=c("whole wheat thin",
                          "white regular",
                          "stuffed crust",
                          "unstuffed crust",
                          "whole wheat thick",
                          "brown regular",
                          "stuffed crust-medium",
                          "whole wheat medium",
                          "regular",
                          "wheat thin"
             ))

cheeseType_table <- 
  data.frame(cheeseType_key= 71:80,
             cheese_type=c("Mozzarella",
                           "Feta",
                           "Cheddar",
                           "Tex Mex",
                           "Swiss",
                           "Cream Cheese",
                           "parmagiano-reggiano",
                           "Gorgonzola",
                           "Traditional Danish Blue",
                           "Brie"
             ),
             cheese_vendor=c("Arla foods",
                             "Fonterra",
                             "FrieslandCampina",
                             "Savencia",
                             "The Lactalis Group",
                             "The Kraft Heinz Company",
                             "Danone",
                             "Dairy Farmers of America",
                             "Yili Group",
                             "Saputo Inc"
             ))

topping_table <- 
  data.frame(topping_key = 81:90,
             topping_type = c("tomatoes",
                              "pepper",
                              "onions",
                              "pepperoni",
                              "pickles",
                              "olives",
                              "Sausage",
                              "pineapple",
                              "garlic",
                              "Basil"
             ))
library(generator)

storeLocation_dim <- write.csv(storeLocation_table, file = "Question 1/Pizza Store_store_location - storeLocation Dimension.csv")
cheeseType_dim <- write.csv(cheeseType_table, file = "Question 1/cheese type dimension - Sheet1.csv")
dough_dim <- write.csv(dough_table, file = "Question 1/dough dimension - Sheet1.csv")
doughType_dim <- write.csv(doughType_table, file = "Question 1/dough type dimension - Sheet1.csv")
pizzaSize_dim <- write.csv(pizzaSize_table, file = "Question 1/pizza size dimension - Sheet1.csv")
topping_dim <- write.csv(topping_table, file = "Question 1/topping dimension - Sheet1.csv")
address_dim <- write.csv(address_table, file = "Question 1/address dimension - Sheet1.csv")
orders_fact_dim <- write.csv(orders_fact_table, file = "Question 1/Orders Fact Table - Sheet1.csv")

storeLocation_table <- read.csv("Question 1/Pizza Store_store_location - storeLocation Dimension.csv")
cheeseType_table <- read.csv("Question 1/cheese type dimension - Sheet1.csv")
dough_table <- read.csv("Question 1/dough dimension - Sheet1.csv")
doughType_table <- read.csv("Question 1/dough type dimension - Sheet1.csv")
pizzaSize_table <- read.csv("Question 1/pizza size dimension - Sheet1.csv")
topping_table <- read.csv("Question 1/topping dimension - Sheet1.csv")
address_table <- .csv("Question 1/address dimension - Sheet1.csv")

# Function to generate the orders fact table
gen_fact <- function(no_of_row) {
  orders_key = sample((x=100:2000), no_of_row, replace =T)
  loc_key = storeLocation_table$loc_key
  date = r_date_of_births(no_of_row, start = as.Date("2017-01-01"), end = Sys.Date())
  pizza_key = pizzaSize_table$pizza_key
  dough_key = dough_table$dough_key
  cheese_key = cheeseType_table$cheeseType_key
  topping_key = topping_table$topping_key
  quantity = sample((x= 1:5), no_of_row, replace = T, prob = c(1,1,1,1,1))
  pizza_size = sample(pizzaSize_table$pizza_size_value, no_of_row, replace = T, prob = c(1,1,1,10,0.5))
  pizza_type = pizzaSize_table$pizza_size
  profit = sample((x= 24:120), no_of_row, replace = T)
  year_var = sample((x=2017:2021), no_of_row, replace = T)
  month_var = sample((x=1:12), no_of_row, replace = T)
  
  orders_fact <-
    data.frame(orders_key,
               loc_key,
               date,
               pizza_key,
               dough_key,
               cheese_key,
               topping_key,
               quantity,
               profit)
  # orders_fact <- orders_fact[order(orders_fact$date_pizza)]
  row.names(orders_fact) <- NULL
  return(orders_fact)
}

orders_fact_table <- gen_fact(1200)

head(orders_fact_table)

orders_fact_table <- read.csv("Question 1/Orders Fact Table - Sheet1.csv")

orders_fact1 <- join(orders_fact_table, pizzaSize_table, by= "pizza_key", type = "full", match= "all") 

address_final <- join(storeLocation_table, address_table, by = "city_key", type = "full", match= "all")

orders_fact_final <- join(orders_fact1, address_final, by= "loc_key", type = "full", match = "all")

orders_fact_final$year <- strftime(orders_fact_final$date, "%Y")
orders_fact_final$month <- strftime(orders_fact_final$date, "%m")

revenue_cube <- 
  tapply(orders_fact_final$profit, 
         orders_fact_final[,c("pizza_size", "pizza_size_value","month", "year", "city")], 
         FUN=function(x){return(sum(x))})
head(revenue_cube)
revenue_cube

revenue_cube <- write.csv(revenue_cube, file = "Question 1/Revenue Cube.csv")

unique(revenue_cube)

apply(revenue_cube, c("year", "pizza_size_value"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("year", "month", "pizza_size_value"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

