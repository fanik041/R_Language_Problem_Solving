

Address_Dimension <- write.csv(Address_table, file = 'data/Address-Table.csv')

stri_rand_strings()

storeLocation <- read.csv("Question 1/Pizza Store_store_location - storeLocation Dimension.csv")
cheeseTypeDimension <- read.csv("Question 1/cheese type dimension - Sheet1.csv")
doughDimension <- read.csv("Question 1/dough dimension - Sheet1.csv")
doughTypeDimension <- read.csv("Question 1/dough type dimension - Sheet1.csv")
pizzaSizeDimension <- read.csv("Question 1/pizza size dimension - Sheet1.csv")
toppingDimension <- read.csv("Question 1/topping dimension - Sheet1.csv")
addressDimension <- read.csv("Question 1/address dimension - Sheet1.csv")

Orders_Fact_Table <- read.csv("Question 1/Orders Fact Table - Sheet1.csv")

dimnames(storeLocation)
dimnames(addressDimension)
install.packages("tidyverse")
library(tidyverse)

revenue_cube <- 
  tapply(Orders_Fact_Table$Profit, 
         Orders_Fact_Table[,c("pizza_key", "date", "location_key")], 
         FUN=function(x){return(sum(x))})

install.packages("plyr")
library(plyr)
orders_fact_new <- join(Orders_Fact_Table, pizzaSizeDimension, by = "pizza_key", type = "full", match = "all")
orders_fact_new

merged_orders_pizzaSize <- merge(Orders_Fact_Table, pizzaSizeDimension)

merge(orders_fact_new, addressDimension)
orders_fact_one <- join(storeLocation, addressDimension, by = "city_key", type = "full", match = "all")
orders_fact_one
orders_fact_final <- join(orders_fact_new, orders_fact_one, by = "location_key")

oft_Snowflake <- write.csv(orders_fact_final, file = "Question 1/orders_fact_snowflake.csv")



orders_fact_final$year <- strftime(orders_fact_final$date, "%Y")
orders_fact_final$month <- strftime(orders_fact_final$date, "%m")

colnames(orders_fact_final)

typeof(orders_fact_table_Snowflake)

revenue_cube <- 
  tapply(orders_fact_final$Profit, 
         orders_fact_final[,c("pizza_size", "pizza_size_value", "year", "month", "City")], 
         FUN=function(x){return(sum(x))})
revenue_cube

rev_cube <- write.csv(revenue_cube, file = "Question 1/Revenue Cube.csv")


apply(revenue_cube, c("year", "pizza_size"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

apply(revenue_cube, c("year", "month", "pizza_size"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

