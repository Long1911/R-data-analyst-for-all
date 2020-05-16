

# SESSION 3: DEA - DATA EXPLORATORY ANAYSIS #
# Part of this section is taken from Chapter 3 in 
# the reference book Nina Zumel, John Mount - Practical Data Science with R-Manning Publications (2014

# Outcome:
# - Single variable summary and exploration: distribution
#     + 1 catagorical variable EDA
#     + 1 continous variable EDA
# - Multiple variables summary and exploration: relationship
#     + 2 catagorical variables EDA
#     + 2 catagorical + 1 continous variables EDA
#     + 2 continous variables EDA
#     + 2 continous + 1 categorical variables EDA
#     + 2 continous + 2 categorical variables EDA
############################################
# Clean environment
rm(list = ls())

# Load data ----
custdata <- read.table("data/custdata.tsv", header = TRUE, sep = "\t")
head(custdata)
summary(custdata) # There are some missing data

# EDA 1 categorical ----
table(custdata$health.ins, useNA = c("always"))
prop.table(table(custdata$health.ins, useNA = c("always")))
temp <- data.frame(table(custdata$housing.type, useNA = c("always")))
temp
temp$Perc <- prop.table(table(custdata$housing.type, useNA = c("always")))
temp
barplot(temp$Freq)

# Graph
#install.packages("ggplot2")
library("ggplot2")
ggplot(custdata)+geom_bar(aes(x=housing.type),fill="darkred")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(custdata)+geom_bar(aes(x=housing.type,fill=housing.type))+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# Save plot to image
dev.copy(png,'myfirstplot.png')
dev.off()  

# EDA 2 categoricals ----
table(custdata$housing.type,custdata$health.ins,useNA = c("always"))
prop.table(table(custdata$housing.type,custdata$health.ins,useNA = c("always")),1)
# Graph
ggplot(custdata)+geom_bar(aes(x=housing.type,fill=health.ins))+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(custdata)+geom_bar(aes(x=housing.type,fill=health.ins),
                          position = "dodge")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggplot(custdata)+geom_bar(aes(x=housing.type,fill=health.ins),
                          position = "fill")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

# EDA 1 continuous ----
summary(custdata$age)
var(custdata$age)
sd(custdata$age)

# Histogram
hist(custdata$age)
hist(custdata$age,40)
# Get the histogram data
temp <- hist(custdata$age,40)
temp$counts
temp$breaks

# You can change the bins manually
temp <- hist(custdata$age, breaks = c(0, 20, 50, 70,150))
temp$counts

# With ggplot
ggplot(data = custdata, aes(x = age)) +
  geom_histogram(bins = 50, alpha = 0.4, fill = "blue")
# Plot density
ggplot(data = custdata, aes(x = age)) +
  geom_density(bins = 50, alpha = 0.4, fill = "blue")
# Add meanline and density
ggplot(data = custdata, aes(x = age)) + 
  geom_histogram(aes(y=..density..), color = "black", fill = "grey", alpha=.3) +
  geom_vline(aes(xintercept=mean(age)),
             color="blue", linetype="dashed", size=1) + 
  geom_density(alpha=.2, fill="#FF6666", kernel = "gaussian")

# EDA 1 cont. + 1 cat. ----
ggplot(custdata)+geom_density(aes(x=age,fill=health.ins),
                              alpha = 0.4)
# Using Boxplot
ggplot(data = custdata, aes(y = age)) +
  geom_boxplot()
ggplot(data = custdata, aes(x = housing.type, y = age)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle=45,hjust=1,family = "Times"))

# EDA 2 continous ----
# Correlation coefficients
cor(custdata$age, custdata$income)
# Graph
ggplot(data = custdata, aes(x = age, y = income)) +
  geom_point(alpha = 0.3, size = 1, color="blue")
# remove age = 0 and age > 100
custdata2 <- custdata[(custdata$age > 0) & (custdata$age < 100),]
# or
custdata2 <- subset(custdata, (age > 0) & (age < 100))
# or
library("dplyr")
custdata2 <- custdata %>% filter((age > 0) & (age < 100))
# Plot cleaner data
ggplot(data = custdata2, aes(x = age, y = income)) +
  geom_point(alpha = 0.3, size = 1, color="blue")

# EDA 2 continous + 1 categorical ----
ggplot(custdata)+geom_point(aes(x=age,y=income,color=health.ins),
                            alpha=0.3)

# EDA 2 continous + 2 categorical ----
ggplot(custdata)+geom_point(aes(x=age,y=income,color=health.ins),
                            alpha=0.3)+
  facet_wrap(~housing.type)

