# Load the required library
library(readr)

# Read the CSV files
filename <- file.choose()
nyt1 <- read.csv(filename)

filename <- file.choose()
nyt2 <- read.csv(filename)

filename <- file.choose()
nyt3 <- read.csv(filename)

# Replace age 0 with NA to avoid confusion
nyt1$Age[nyt1$Age == 0] <- NA 
nyt2$Age[nyt2$Age == 0] <- NA 
nyt3$Age[nyt3$Age == 0] <- NA 

# Create a new variable, age_group, that categorizes users
breaks <- c(-Inf, 20, 30, 40, 50, 60, 70, 120)
labels <- c("<20", "20-29", "30-39", "40-49", "50-59", "60-69", "70+")

nyt1$Age_Group <- cut(nyt1$Age, breaks = breaks, labels = labels, right=FALSE)
nyt2$Age_Group <- cut(nyt2$Age, breaks = breaks, labels = labels, right=FALSE)
nyt3$Age_Group <- cut(nyt3$Age, breaks = breaks, labels = labels, right=FALSE)

# Display the structure of nyt1
glimpse(nyt1)

# Calculate Click Through Rate for each dataset
nyt1$Click_Through_Rate <- (nyt1$Clicks/nyt1$Impressions)
nyt2$Click_Through_Rate <- (nyt2$Clicks/nyt2$Impressions)
nyt3$Click_Through_Rate <- (nyt3$Clicks/nyt3$Impressions)

# Plotting distribution of click-through-rate
plot(nyt1$Age_Group, nyt1$Click_Through_Rate, main="Click Through Rate & Age groups. Day 1",
     xlab="Age group", ylab="Click Through Rate", pch=20)

plot(nyt2$Age_Group, nyt2$Click_Through_Rate, main="Click Through Rate & Age groups. Day 2",
     xlab="Age group", ylab="Click Through Rate", pch=20)

plot(nyt3$Age_Group, nyt3$Click_Through_Rate, main="Click Through Rate & Age groups. Day 3",
     xlab="Age group", ylab="Click Through Rate", pch=20)

# Plotting distribution of impressions
plot(nyt1$Age_Group, nyt1$Impressions, main="Impressions & Age groups. Day 1",
     xlab="Age group", ylab="Impressions", pch=20)

plot(nyt2$Age_Group, nyt2$Impressions, main="Impressions & Age groups. Day 2",
     xlab="Age group", ylab="Impressions", pch=20)

plot(nyt3$Age_Group, nyt3$Impressions, main="Impressions & Age groups. Day 3",
     xlab="Age group", ylab="Impressions", pch=20)

# Categorizing users based on their click behavior
breaks_clicks <- c(0, 1, 2, 3, 4, 5)
labels_clicks <- c("Passive", "Somewhat passive","Moderately active", "Intensly active", "Extremely active")

summary(nyt1$Clicks)
unique(nyt1$Clicks)

nyt1$User_Type <- cut(nyt1$Clicks,breaks = breaks_clicks,
                      labels = labels_clicks,right=FALSE)

nyt2$User_Type <- cut(nyt2$Clicks,breaks = breaks_clicks,
                      labels = labels_clicks,right=FALSE)

nyt3$User_Type <- cut(nyt3$Clicks,breaks = breaks_clicks,
                      labels = labels_clicks,right=FALSE)

# Exploring the data and making comparisons

# Day 1
femaleusers <- subset(nyt1, Gender==0, select=Age:Age_Group)
maleusers <- subset(nyt1, Gender==1, select = Age:Age_Group)


# Plotting Age and Impressions for Day 1
scatterplot(Impressions ~ Age, data = femaleusers, main="Female users' Age and Impressions D1")
scatterplot(Impressions ~ Age, data = maleusers, main="Male users' Age and Impressions D1")

 # Summary of Impressions for Day 1
summary(femaleusers$Impressions)
MaxImpressions_F_D1 <- subset(femaleusers, Impressions > 13)
plot(MaxImpressions_F_D1$Age, MaxImpressions_F_D1$Impressions,
     main = "Female Maximum Impressions Age D1",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

summary(maleusers$Impressions)
MaxImpressions_M_D1 <- subset(maleusers, Impressions > 13)
plot(MaxImpressions_M_D1$Age, MaxImpressions_M_D1$Impressions,
     main = "Male Maximum Impressions Age D1",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

# Day 2
femaleusers2 <- subset(nyt2, Gender==0, select=Age:Age_Group)
maleusers2 <- subset(nyt2, Gender==1, select = Age:Age_Group)

# Plotting Age and Impressions for Day 2
scatterplot(Impressions ~ Age, data = femaleusers2, main="Female users' Age and Impressions D2")
scatterplot(Impressions ~ Age, data = maleusers2, main="Male users' Age and Impressions D2")

MaxImpressions_F_D2 <- subset(femaleusers2, Impressions > 13)
plot(MaxImpressions_F_D2$Age, MaxImpressions_F_D2$Impressions,
     main = "Female Maximum Impressions Age D2",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

MaxImpressions_M_D2 <- subset(maleusers2, Impressions > 13)
plot(MaxImpressions_M_D2$Age, MaxImpressions_M_D2$Impressions,
     main = "Male Maximum Impressions Age D2",
     xlab = "Age", ylab = "Impressions",
     pch = 19, frame = FALSE)

# Day 3
femaleusers3 <- subset(nyt3, Gender==0, select=Age:Age_Group)
maleusers3 <- subset(nyt3, Gender==1, select = Age:Age_Group)

# Plotting Age and Impressions for Day 3
scatterplot(Impressions ~ Age,
            data = femaleusers3,
            main="Female users' Age and Impressions D3")
scatterplot(Impressions ~ Age,
            data = maleusers3,
            main="Male users' Age and Impressions D3")

MaxImpressions_F_D3 <- subset(femaleusers3,
                              Impressions > 13)
plot(MaxImpressions_F_D3$Age,
     MaxImpressions_F_D3$Impressions,
     main = "Female Maximum Impressions Age D3",
     xlab = "Age",
     ylab = "Impressions",
     pch = 19,
     frame = FALSE)

MaxImpressions_M_D3 <- subset(maleusers3,
                              Impressions > 13)
plot(MaxImpressions_M_D3$Age,
     MaxImpressions_M_D3$Impressions,
     main = "Male Maximum Impressions Age D3",
     xlab = "Age",
     ylab = "Impressions",
     pch = 19,
     frame = FALSE)

