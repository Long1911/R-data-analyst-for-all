
# SESSION 9: UNSUPERVISED MODELs #
# Outcome:
# - K-mean clustering
# - Hierarchical clustering
# - Association rules/Baskets analysis
############################################

# K-mean clustering ----
protein <- read.csv("data/protein.csv")
head(protein)

# EDA: RedMeat, WhiteMeat
library("ggplot2")
ggplot(protein)+geom_point(aes(x=RedMeat,y=WhiteMeat),
                           color="red")
# How many cluster?
data <- protein[,c("RedMeat","WhiteMeat")]
# Scaling?
data <- scale(data)
k <- 2
output <- kmeans(data,centers = k, nstart = 20)
ggplot(protein)+geom_point(aes(x=RedMeat,y=WhiteMeat),
                           color=output$cluster)
# With Country name
ggplot(protein,aes(x=RedMeat,y=WhiteMeat)) + 
  geom_point(color = output$cluster)+
  geom_text(aes(label = Country))

# Re-cluster with Fish
data <- protein[,c("RedMeat","WhiteMeat","Fish")]
# Scaling?
data <- scale(data)
k <- 4
output <- kmeans(data,centers = k, nstart = 20)
ggplot(protein)+geom_point(aes(x=RedMeat,y=WhiteMeat),
                           color=output$cluster)
# Country name
ggplot(protein,aes(x=RedMeat,y=WhiteMeat)) + 
  geom_point(color = output$cluster)+
  geom_text(aes(label = Country))
# Overlap?

# 3d grah
#install.packages("scatterplot3d")
library("scatterplot3d")
scatterplot3d(protein[,c("RedMeat","WhiteMeat","Fish")],
              color = output$cluster)
output$cluster
protein$k_mean_cluster4 <- output$cluster
write.csv(protein, file = "protein_kmean.csv", row.names=FALSE)

# With country name
s3d <- scatterplot3d(protein[,c("RedMeat","WhiteMeat","Fish")],
                     color = output$cluster)
scatterplot3d(protein[,c("RedMeat","WhiteMeat","Fish")],
              color = output$cluster,pch = 20)
text(s3d$xyz.convert(protein[,c("RedMeat","WhiteMeat","Fish")]),
     labels = protein$Country, cex = 0.5)

# Find optimal k?
# Run a loop, measure: output$tot.withinss
all_tw <- c()
for(i in 1:10){
  output <- kmeans(data,centers = i, nstart = 20)
  all_tw <- c(all_tw,output$tot.withinss)
}
plot(all_tw, type = 'b', col = 'red') # 

# Hierarchical Clustering (Outliers sensitive) ----
hc <- hclust(dist(data), method = "average")
plot(hc)
cluster <- cutree(hc,5)
# Country name
ggplot(protein,aes(x=RedMeat,y=WhiteMeat)) + 
  geom_point(color = cluster)+
  geom_text(aes(label = Country))
scatterplot3d(protein[,c("RedMeat","WhiteMeat","Fish")],
              color = cluster)

# Association rules ----
data <- read.csv('data/d2.csv')
head(data)
# product X,Y: Support/Confidence/Lift
# X: AG0446 - SPORT STRAPPING TAPE 4.5mx38mm
# Y: NG0073 - DIGITAL BABY THERMOMETER

# How many unique invoice?
length(unique(data$Invoice))  # 3965
# List all invoices with AG0446
unique(data$Invoice[data$BarCode == "AG0446"])
length(unique(data$Invoice[data$BarCode == "AG0446"]))
# List all invoices with NG0073
unique(data$Invoice[data$BarCode == "NG0073"])
length(unique(data$Invoice[data$BarCode == "NG0073"]))
# List all invoices with both AG0446 and NG0073
intersect(unique(data$Invoice[data$BarCode == "AG0446"]),
          unique(data$Invoice[data$BarCode == "NG0073"])) # 2 invoices
2/3965 # support = 0.0005
2/10   # confidence = 0.2
2/10/(6/3965)  # lift = 132

# How many products?
length(unique(data$BarCode))  # 1514
1514*1513/2  # 1145341

#install.packages("arules")
library("arules")  # Apriori algorithm
shoplist <- split(x=data$BarCode, f = data$Invoice)
rules <- apriori(shoplist,parameter = list(support = 0.001,
                                           confidence = 0.5))
inspect(rules)
# Rule 1
data$Description[data$BarCode=="BKC00008"][1] #Keo que tron lon
data$Description[data$BarCode=="BKC00050"][1] #Keo tim nho
# Rule 3
data$Description[data$BarCode=="TN0169"][1] #GLOW SWORD/WAND ASST COL
data$Description[data$BarCode=="AJ0017"][1] #GLOW NECKLACE 2pk 55cm
#
rules <- sort(rules,decreasing = TRUE, by="confidence")
inspect(rules)

# Exercise: Lastfm dataset
music <- read.csv("data/lastfm.csv")
head(music)
# How many unique users?
length(unique(music$user))  #15000
# How many unique artist?
length(unique(music$artist))  #1004
shoplist <- split(x=music$artist, f = music$user)
rules <- apriori(shoplist,parameter = list(support = 0.01,
                                           confidence = 0.5))
inspect(rules)
# Find all rules with support > 0.01, sort by support
rules <- sort(rules,decreasing = TRUE, by="support")
inspect(rules)
# Find all rules with support > 0.01, sort by confidence
rules <- sort(rules,decreasing = TRUE, by="confidence")
inspect(rules)
# Find all rules with support > 0.01, sort by lift
rules <- sort(rules,decreasing = TRUE, by="lift")
inspect(rules)
