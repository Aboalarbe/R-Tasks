install.packages("writexl")
install.packages("readxl")
library("writexl")
library("readxl")
Cheese <- read_excel("tables/Cheese.xlsx")
Date <- read_excel("tables/Date.xlsx")
Days <- read_excel("tables/Days.xlsx")
Dough <- read_excel("tables/Dough.xlsx")
Location <- read_excel("tables/Location.xlsx")
Months <- read_excel("tables/Months.xlsx")
Orders <- read_excel("tables/Orders.xlsx")
PizzaSize <- read_excel("tables/PizzaSize.xlsx")
Topping <- read_excel("tables/Topping.xlsx")
Years <- read_excel("tables/Years.xlsx")


# Function to generate the Date table
gen_date <- function(no_of_recs) {
  # Generate transaction data randomly
  # fill date_id start from 1 to number of sequences
  s_date_id <- 1:no_of_recs
  # fill the day_id from the dimension table Days
  s_day_id <- sample(Days$day_id, no_of_recs, 
                replace=T)
  # fill the month_id from the dimension table Months
  s_month_id <- sample(Months$month_id, no_of_recs, replace=T)
  # fill the year_id from the dimension table Years
  s_year_id <- sample(Years$year_id, no_of_recs, replace=T)
  # create data frame of all generated attributes to represent our Date table
  date_table <- data.frame(date_id= s_date_id,
                           day_id=s_day_id,
                           month_id=s_month_id,
                           year_id=s_year_id)
  
   return (date_table)
}


# Function to generate the Orders table
gen_orders <- function(no_of_recs) {
  # Generate transaction data randomly
  
  # fill the topping_id from the dimension table Topping
  s_topping_id <- sample(Topping$topping_id, no_of_recs, 
                     replace=T)
  # fill the cheese_id from the dimension table Cheese
  s_cheese_id <- sample(Cheese$chesse_id, no_of_recs, replace=T)
  # fill the dough_id from the dimension table Dough
  s_dough_id <- sample(Dough$dough_id, no_of_recs, replace=T)
  # fill the size_id from the dimension table PizzaSize
  s_size_id <- sample(PizzaSize$size_id, no_of_recs, replace=T)
  # fill the date_id from the dimension table Date
  s_date_id <- sample(Date$date_id, no_of_recs, replace=T)
  # fill the location_id from the dimension table Location
  s_location_id <- sample(Location$location_id, no_of_recs, replace=T)
  # fill the dough_id from the dimension table Dough
  s_dough_id <- sample(Dough$dough_id, no_of_recs, replace=T)
  s_quantity <- sample(c(1,2,3,4,5,6,7), no_of_recs, replace=T)
  s_profit <- s_quantity * (Cheese[s_cheese_id,]$price + Topping[s_topping_id,]$price
                            + Dough[s_dough_id,]$price + PizzaSize[s_size_id,]$price)
  # create data frame of all generated attributes to represent our Date table
  orders_table <- data.frame(topping_id= s_topping_id,
                           cheese_id=s_cheese_id,
                           dough_id=s_dough_id,
                           size_id=s_size_id,
                           date_id=s_date_id,
                           location_id=s_location_id,
                           quantity=s_quantity,
                           profit=s_profit)
  
  return (orders_table)
}

# Now create the date fact table and save the generated data in the Date table
Date <- gen_date(5000)
# Now create the orders fact table save the generated data in the Orders table
Orders <- gen_orders(5000)

View(Date)
View(Orders)

# then save date into the excel file "Date"
write_xlsx(Date,"tables/Date.xlsx")
# then save orders into the excel file "Orders"
write_xlsx(Orders,"tables/Orders.xlsx")

#Build an OLAP cube for profit and show the cells of a subset of the cells
profit_cube <- 
  tapply(Orders$profit, 
         Orders[,c("size_id", "quantity", "dough_id", "cheese_id")], 
         FUN=function(x){return(sum(x))})
profit_cube
dimnames(profit_cube)

# Slice
# cube data with xlarge size and Mozzarella cheese
profit_cube["5", , ,"3"]

# Dice 
# cube data with (medium large) size and (2,4) quantity
profit_cube[c(3,4), c(2,4), ,]

# Rollup
apply(profit_cube, c("size_id", "dough_id"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})


apply(profit_cube, c("size_id", "cheese_id"),
      FUN=function(x) {return(sum(x, na.rm=TRUE))})

#Drill Down
apply(profit_cube, c("size_id", "dough_id", "cheese_id"), 
      FUN=function(x) {return(sum(x, na.rm=TRUE))})
