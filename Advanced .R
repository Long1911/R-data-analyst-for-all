
# SESSION 2: R Advanced #
# Outcome:
# - Conditional instruction
# - Functions
# - Loop
# - Apply function
# - Library
# - Aggregation
# - Order/sort
# - Write data
# - Dplyr library
# - Working with Excel files
############################################
# Clean environment
rm(list = ls())

# Conditional instructions ----
# if(condition) instructions
x <- 5
if (x < 8) x <- x + 1
x

# if(condition) instruction_TRUE else instruction_FALSE
x <- 5
if (x < 8) {x <- x + 1} else {x <- x - 1}
x

# Function ----
my_func <- function(x){
  out <- if (x < 8) {out <- x + 1} else {out <- x - 1}
  return(out)
}
my_func(5)
my_func(9)

# Exercise: a function return TRUE if x is pair otherwise FALSE
pair_check <- function(x){
  out <- if (x %% 2 == 0) out <- TRUE else out <- FALSE
  return(out)
}
pair_check(5)
pair_check(8)

# For loop ----
sum <- 0
for(i in 1:100){
  print(i)
  sum <- sum + i^2
}
sum

# Exercise: check if a number is prime
is_prime <- function(x){
  if (x < 2) return(FALSE)
  if( x == 2) return(TRUE)
  out <- TRUE
    for (i in 2:(x-1)){
      if (x %% i == 0){
        out <- FALSE
        break
      }
    }
  return(out)
}
is_prime(2)
is_prime(7)
is_prime(8)
is_prime(17)

# Exercise: Find all prime numbers between 2 and 1000
x <- 2:1000
# Using for
all_primes <- c()
for (i in x) if (is_prime(i)) all_primes <- c(all_primes, i)
all_primes
length(all_primes)

# Using apply
all_primes <- x[apply(matrix(x,nrow=length(x)), 1, is_prime)]

# More example with apply
y <- matrix(1:9,nrow = 3)
y
apply(y,1,sum)
apply(y,2,sum)

# Data Wrangling----
# Clean environment
rm(list = ls())
# Reload datasets
sale <- read.csv("data/train_sample_2.csv")
dim(sale)
head(sale)
stores <- read.csv("data/stores.csv")
head(stores)
sale2 <- merge(sale,stores,by.x="store_nbr",by.y = "store_nbr",all.x=TRUE)
head(sale2)

# Dataframe structure
str(sale2)  # structure(): columns type

# Date transformation
sale2$date <- as.Date(sale2$date)
str(sale2)

# Dataframe summary
summary(sale2)

# More selection
# How many transaction, how many unit_sale in Quito in 2015?
df1 <- sale2[(sale2$city == "Quito") & (sale2$date >= as.Date("2015-01-01")) &
               sale2$date <= as.Date("2015-12-31"), ]

# To install: uncomment and run, then comment again after finish
#install.packages("lubridate")
library("lubridate")

# Using library to get more date variables
sale2$year <- year(sale2$date)
head(sale2)

df1 <- sale2[(sale2$city == "Quito") & (sale2$year == 2015),]
nrow(df1)
sum(df1$unit_sales)

# Aggregration----
# How many transaction each year in Quito
temp <- sale2[(sale2$city == "Quito"), ]
aggregate(unit_sales ~ year, temp, length)  #transaction
# How many total unit sold each year in Quito
aggregate(unit_sales ~ year, temp, sum)  #total unit sale

# Order/Sort----
# What are the top sold item in Quito?
temp2 <- aggregate(unit_sales ~ item_nbr, temp, sum)  
head(temp2)
?order
temp2 <- temp2[order(temp2$unit_sales, decreasing = TRUE),]
head(temp2,10)

# Export to file----
write.csv(head(temp2,10), "top_10_item_Quito.csv")

# Export multi dataframes - Automation
# Write all top 10 item for each city?
city_list <- unique(sale2$city)
city_list
for (city in city_list){
  temp <- sale2[(sale2$city == city), ]
  temp2 <- aggregate(unit_sales ~ item_nbr, temp, sum) 
  temp2 <- temp2[order(temp2$unit_sales, decreasing = TRUE),]
  write.csv(head(temp2,10), paste("top_10_item_",city,".csv",sep=""))
}

# "dplyr" library ----
#install.packages("dplyr")
library("dplyr")
temp <- sale2[(sale2$city == "Quito"), ]
# Pipeline summary
temp2 <- temp %>% group_by(item_nbr) %>% 
  summarise(Total_unit_sales = sum(unit_sales),
            Max_unit_sales = max(unit_sales),
            Min_unit_sales = min(unit_sales),
            Nb_transaction = length(unit_sales)) %>% 
  arrange(desc(Total_unit_sales), .by_group = TRUE)
temp2

# Exercise sheets 2 & 3
# Exercise 2.1
rm(ls = list())  # Clean environment
# Reload datasets
sale <- read.csv("data/train_sample_2.csv")
stores <- read.csv("data/stores.csv")
sale2 <- merge(sale,stores,by.x="store_nbr",by.y = "store_nbr",all.x=TRUE)
head(sale2)
# 1.
sum(sale2$unit_sales)
min(sale2$unit_sales)
max(sale2$unit_sales)
# 2.
df1 <- sale2[sale2$unit_sales > 10000,]
df1  # 2 transactions
# 3.
sale2$city[sale2$unit_sales == max(sale2$unit_sales)]
#sale2$city[max(sale2$unit_sales)]  # wrong
# 4.
unique(sale2$city) ## 22 cities
# 5.
aggregate(unit_sales ~ city, sale2, length)
library("dplyr")
sale2 %>% group_by(city) %>% summarise(Nb_trans = length(unit_sales))
# 6.
temp <- sale2 %>% group_by(city) %>% 
  summarise(Nb_trans = length(unit_sales),
            Total_unit_sales = sum(unit_sales),
            Max_unit_sales = max(unit_sales),
            Min_unit_sales = min(unit_sales))

# Working with Excel files
#install.packages("openxlsx")
library('openxlsx')
new_file <- "sale_by_city.xlsx"
wb = createWorkbook(new_file)  #loadWorkbook("filename")
addWorksheet(wb, "sheet1")
writeData(wb,sheet = "sheet1",x = temp)
saveWorkbook(wb,new_file,overwrite = TRUE)

# Exercise 2.2
custdata <- read.table("data/custdata.tsv", header = TRUE, sep = "\t")
head(custdata)
# 8.
aggregate(custid ~ housing.type, custdata, length)
# 9.
aggregate(custid ~ housing.type + health.ins, custdata, length)
# 10.
df2 <- custdata[is.na(custdata$housing.type),]
dim(df2) # 56 customers missing housing.type
df3 <- custdata[!is.na(custdata$housing.type),]
dim(df3) # 944 customers
# 11.
custdata %>% group_by(housing.type) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income))
# 12.a
custdata %>% filter(sex == "M") %>% group_by(housing.type) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income))
# 12.b
custdata %>% filter(sex == "F") %>% group_by(housing.type) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income))
# 13.
temp <- custdata %>% group_by(state.of.res) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income)) %>% 
  arrange(desc(Avg_income),.by_group = TRUE)
head(temp,10)
# 14.
temp <- custdata %>% group_by(state.of.res) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Diff_income_M_F = mean(income[sex == "M"])- mean(income[sex == "F"]),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income)) %>% 
  arrange(desc(Diff_income_M_F),.by_group = TRUE)
head(temp,10)
# check
temp <- custdata %>% group_by(state.of.res,sex) %>% 
  summarise(Nb_customers = length(custid),
            Avg_age = mean(age),
            Avg_income = mean(income),
            Max_income = max(income),
            Min_income = min(income))
temp

