#Code from the R-training day 1

#R basics -------------------------------------------------------------

#Vectors
x <- 5
y <- c("a", "b")
f <- factor(y)
i <- c(TRUE, FALSE)

#Matrix
X <- matrix(1:4, ncol = 2, nrow = 2)

#Data frame
df <- data.frame(col1 = 5:8,
                 col2 = rep(f, 2))

#Export
setwd("C:/Users/morgstro/Desktop/R Training") #Set working directory
write.csv(df, file = "df.csv", 
          na = "", row.names = FALSE)

#Import
df2 <- read.csv("df.csv")

all.equal(df, df2)


#Subset
df[1, ]
df[, 1]
df[1, 1]
df[, "col2"]
df$col2
df[df$col2 == "b", ]

#Logical operations
a <- (df$col1 %in% c(7,8))
b <- (df$col2 != "b")
a & b

#Regular expressions
grepl("1", names(df))
grepl("col", names(df))

#Functions ----------------------------------------------------------

#Step function example

f <- function(x, c) {
  if (x >= c) {
    return(1)
  } else {
    return(0)
  }
}

f(x = 400, c = 10)

#For loop function
my_sum <- function(x) {
  s <- 0 #Initialize sum at 0
  n <- length(x) #Number of values to sum
  
  for (i in 1:n) {
    s <- s + x[i]
  }
  
  return(s)
}

my_sum(x = c(4,5,8,11))

#Merging data using set operations -------------------------------------


#Read spreadsheets
rm(list = ls()) #Removes everything from environment!

df1 <- read.csv("wg1.csv", stringsAsFactors = FALSE)
df2 <- read.csv("wg2.csv", stringsAsFactors = FALSE)

#Columns identical
all.equal(names(df1), names(df2))

#Merging tables
library(dplyr)

df3 <- dplyr::setdiff(df1, df2)
df3 <- dplyr::setdiff(df2, df1)
df3 <- dplyr::intersect(df1, df2)
df3 <- dplyr::union(df1, df2)
nrow(df3) #7000 - 153 = 6847


#Create norms-----------------------------------------------------

#Data from previous exercise: df3

#1. Look at dataframe structure
names(df3)
names(df3)[10] #Very long name
names(df3)[10] <- "Disability" #Replace with a shorter name
names(df3) #Better

str(df3)

#Manage date variable
df3$EndTime <- as.POSIXct(df3$EndTime, format = "%Y-%m-%d %H:%M:%S")
#Create factors
df3$Sex <- factor(df3$Sex)
df3$PD_Education <- factor(df3$PD_Education)
df3$PD_Occupation <- factor(df3$PD_Occupation)
df3$PD_Reason <- factor(df3$PD_Reason)
df3$Ethnic.group <- factor(df3$Ethnic.group)


#2. Summarise dataframe
summary(df3)

#Focus on theta score distribution
library(psych)
psych::describe(df3$wgcta.theta.score) #Looks good!

#Summarise whatever you want
library(dplyr)
summarise(df3, n = n(),
            mean_theta = mean(wgcta.theta.score, na.rm = TRUE))


#3. Graphs

#Theta score distribution
hist(df3$wgcta.theta.score) #Looks good!

#Theta vs time taken
plot(wgcta.theta.score ~ TimeTaken, df3) #What?


#Compare subgroups
boxplot(df3$wgcta.theta.score ~ df3$Sex) #No difference
boxplot(df3$wgcta.theta.score ~ df3$PD_Education) #A bit messy!

#Create new Education levels
levels(df3$PD_Education)
levels(df3$PD_Education) <- c("Not specified", "A-level", rep("Bachelor", 4),
                           "Doctoral", "GCSE", "GCSE", "Higher Ed Diploma",
                           "Master", "Master", "No formal education", 
                           "Not specified", "Not specified")

boxplot(df3$wgcta.theta.score ~ df3$PD_Education) #Better!
  

#Do norm group filtering
graduates <- df3 %>%
  filter(!is.na(wgcta.theta.score)) %>% #Filter out NA scores
  filter(TimeTaken > 0 & TimeTaken <= 2000) %>% #Filter out wierd times
  filter(PD_Education %in% c("Doctoral", "Master", "Bachelor")) 

legal_professionals <- df3 %>%
  filter(!is.na(wgcta.theta.score)) %>% #Filter out NA scores
  filter(TimeTaken > 0 & TimeTaken <= 2000) %>% #Filter out wierd times
  filter(PD_Occupation == "Legal Professional") 

#Do grouped summaries with dplyr
legal_professionals %>%
  group_by(PD_Education) %>%
  summarise(n = n(), 
            mean_theta = mean(wgcta.theta.score, na.rm = TRUE),
            prop_female = sum(Sex == "Female") / n(),
            prop_white = sum(grepl("White", Ethnic.group)) / n()) %>%
  arrange(desc(mean_theta))


#Look at app!
shiny::runApp("wg_norm_app")
#https://morgan.shinyapps.io/wg_norm

#Try: 
##candidate theta = -0.6
##job title = barrister
## --> percentile 5

##occupation = medical professional
##education = a-level, gcse, higher education diploma or no formal education
## --> percentile 57


#Joining tables ----------------------------------------------------------
athena <- read.csv("athena.csv")
cubiks <- read.csv("cubiks.csv")

study <- full_join(athena, cubiks, by = ("ID" = "ID"))
study <- inner_join(athena, cubiks, by = c("ID" = "ID"))
study <- left_join(cubiks, athena, by = c("ID" = "ID", "Group" = "Group"))

#Sneak peek at tomorrow's material!
#Pearson correlation with significance test
cor.test(study$Athena.Raw, study$Cubiks.Raw, method = "pearson")
#Bivariate scatterplot
plot(study$Athena.Raw, study$Cubiks.Raw, pch = 20, col = "steelblue")
#Add simple linear regression model to the plot
abline(lm(Cubiks.Raw ~ Athena.Raw, study), lty = 2, col = "black")