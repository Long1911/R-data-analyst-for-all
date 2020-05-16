

# SESSION 1: R Basics #
# Outcome:
# - Get used to R and RStudio Interface
# - Basic R
# - Vector
# - Dataframe
# - Real dataset practices
############################################

# Basics ----

# Clean environment
rm(list = ls())

3+4
# Ctr+Enter to execute
# This is a comment

# Arithmetic: +, -, *, /, ^
2*6
5/3

# Variables ----
x <- 5
x # view x's value
y <- 7
y
x + y 

# Character variables
a <- "hello"
b <- "world"
a
b
a + b  # error
paste(a, b, sep = " ")   # this is your first function

# Logical variables
u <- x == y
u
v <- x < y
v
# Boolean operators
u & v # FALSE
u | v # TRUE
!u
?xor  # HELP HELP  
xor(u,v)  # Not u or v

# Vector ----
x <- c(2,3,8,4,6)
x
length(x)

# Vector Selection
x[1]            # 1st element
x[1:3]          # Elements from 1 to 3
x[c(1,3)]       # Elements 1 and 3
x[3:length(x)]  # Elements from 3 to the end

# Conditional selection
# Elements > 5
x[x > 5]  # 2 values
x > 5     # 5 values
# Index
which(x > 5)

# Dataframe (table data) ----
df <- data.frame(Name = c("A", "B", "C", "D"),
                 Age = c(25,30,35,28),
                 Height = c(1.70, 1.65, 1.72, 1.68))
df

# Dataframe survey
dim(df)    # dimension
dim(df)[1]  # rows number
nrow(df)    # rows number
ncol(df)    # Columns number
names(df)   # Columns names

# Dataframe Selection
# Age of the 3rd customer
df[3,2]
df[3,c("Age")]
# Age of the 2nd and 3rd customer
df[2:3, c("Age")]
# Age and Height of the 2nd and 3rd customer
df[2:3, c("Age", "Height")]
# Age of the 3rd customer
df$Age[3]
# Customers equal or older than 30
df[df$Age >= 30,]
# Name of Customers equal or older than 30
df[df$Age >= 30,c("Name")]
df$Name[df$Age >= 30]
df[df$Age >= 30,]$Name

# Real datasets ----
sale <- read.csv("data/train_sample_2.csv")  # 160MB
dim(sale)
head(sale)
names(sale)

# Selection practice

# Count how many transactions in store 1
df1 <- sale[sale$store_nbr == 1, ]
nrow(df1)  #85456

# Count how many transactions of item 414421 in store 1
df2 <- sale[(sale$store_nbr == 1) & (sale$item_nbr == 414421), ]
nrow(df2)  #44

# Count total unit sale of item 414421 in store 1
sum(df2$unit_sales)  # 163

# Store data
stores <- read.csv("data/stores.csv")
head(stores)

# Merge store variables in stores.csv to train_sample_2.csv (like vlookup in Excel)
sale2 <- merge(sale,stores,by.x="store_nbr",by.y = "store_nbr",all.x=TRUE)
head(sale2)
dim(sale2)

# Count total unit sale of item 414421 in Quito (3 ways)
sum(sale2[(sale2$city == "Quito") & (sale2$item_nbr == 414421), c("unit_sales")])
sum(sale2[(sale2$city == "Quito") & (sale2$item_nbr == 414421), ]$unit_sales)
sum(sale2$unit_sales[(sale2$city == "Quito") & (sale2$item_nbr == 414421)])

# How many (unique) city in the dataset?
unique(sale2$city)
length(sale2$city)

# Exercise sheet 1 ----
# 1
x <- c(20,10,15,3,6,18,12,5)
x[4:length(x)] #1a
x[x>15] #1b
y <- x
y[y>15] <- 15  #1c
y
length(y[y==15]) #1d
x^2  #1e (vectorization)
# 2
u <- c(1,2,3)
v <- c(6,5,4)
typeof(u)
typeof(v)
length(u)
length(v)
u + v #
# 3
x <- seq(1,999,by=2)
sum(x) #3a
sum(x^3)  #3b
length(x[x %% 5 == 0])  #3c: 100
length(x[grepl(5,x)])  #3d: 176
matrix(x,nrow = 100)   #3e
# 4
P <- 1000000000
M <- 15000000
i <- 0.012
n <- log(i/((M/P)-i)+1)/log(1+i)
n
M <- seq(12000000,20000000,2000000)
M
log(i/((M/P)-i)+1)/log(1+i)   #4b