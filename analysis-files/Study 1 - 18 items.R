#----------------------------------------------------
# Item Analysis - Support for Inequality
# Study 1
# Analyses by Dylan Wiwad
#----------------------------------------------------

# Get the file
Data <- read.csv("Study 1 18 items.csv", header = TRUE)

# Recode function to turn the 1-7 scale into 0-6 for the IRT
recode <- function(x){
  x[x == 1] <- 0
  x[x == 2] <- 1
  x[x == 3] <- 2
  x[x == 4] <- 3
  x[x == 5] <- 4
  x[x == 6] <- 5
  x[x == 7] <- 6
  return(x)
}

# Using the recode function on all 18 questions
Data$tol1.r <- recode(Data$tol1.r)
Data$tol2 <- recode(Data$tol2)
Data$tol3 <- recode(Data$tol3)
Data$tol4 <- recode(Data$tol4)
Data$tol5.r <- recode(Data$tol5.r)
Data$tol6.r <- recode(Data$tol6.r)
Data$tol7 <- recode(Data$tol7)
Data$tol8.r <- recode(Data$tol8.r)
Data$tol9 <- recode(Data$tol9)
Data$tol10 <- recode(Data$tol10)
Data$tol11 <- recode(Data$tol11)
Data$tol12 <- recode(Data$tol12)
Data$tol13 <- recode(Data$tol13)
Data$tol14 <- recode(Data$tol14)
Data$tol15.r <- recode(Data$tol15.r)
Data$tol16.r <- recode(Data$tol16.r)
Data$tol17 <- recode(Data$tol17)
Data$tol18.r <- recode(Data$tol18.r)

# Histograms and descriptives for each item
library(ggplot2)
library(pastecs)

# Item 1
item1.hist <- ggplot(Data, aes(Data$tol1.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 1", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol1.r, na.rm = TRUE), sd = sd(Data$tol1.r, na.rm = TRUE)), colour = "black", size = 1)
item1.hist
stat.desc(Data$tol1.r, basic = TRUE, norm = TRUE)
# Item 2
item2.hist <- ggplot(Data, aes(Data$tol2)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 2", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol2, na.rm = TRUE), sd = sd(Data$tol2, na.rm = TRUE)), colour = "black", size = 1)
item2.hist
stat.desc(Data$tol2, basic = TRUE, norm = TRUE)
# Item 3
item3.hist <- ggplot(Data, aes(Data$tol3)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 3", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol3, na.rm = TRUE), sd = sd(Data$tol3, na.rm = TRUE)), colour = "black", size = 1)
item3.hist
stat.desc(Data$tol3, basic = TRUE, norm = TRUE)
# Item 4
item4.hist <- ggplot(Data, aes(Data$tol4)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 4", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol4, na.rm = TRUE), sd = sd(Data$tol4, na.rm = TRUE)), colour = "black", size = 1)
item4.hist
stat.desc(Data$tol4, basic = TRUE, norm = TRUE)
# Item 5
item5.hist <- ggplot(Data, aes(Data$tol5.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 5", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol5.r, na.rm = TRUE), sd = sd(Data$tol5.r, na.rm = TRUE)), colour = "black", size = 1)
item5.hist
stat.desc(Data$tol5.r, basic = TRUE, norm = TRUE)
# Item 6
item6.hist <- ggplot(Data, aes(Data$tol6.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 6", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol6.r, na.rm = TRUE), sd = sd(Data$tol6.r, na.rm = TRUE)), colour = "black", size = 1)
item6.hist
stat.desc(Data$tol6.r, basic = TRUE, norm = TRUE)
# Item 7
item7.hist <- ggplot(Data, aes(Data$tol7)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 7", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol7, na.rm = TRUE), sd = sd(Data$tol7, na.rm = TRUE)), colour = "black", size = 1)
item7.hist
stat.desc(Data$tol7, basic = TRUE, norm = TRUE)
# Item 8
item8.hist <- ggplot(Data, aes(Data$tol8.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 8", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol8.r, na.rm = TRUE), sd = sd(Data$tol8.r, na.rm = TRUE)), colour = "black", size = 1)
item8.hist
stat.desc(Data$tol8.r, basic = TRUE, norm = TRUE)
# Item 9
item9.hist <- ggplot(Data, aes(Data$tol9)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 9", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol9, na.rm = TRUE), sd = sd(Data$tol9, na.rm = TRUE)), colour = "black", size = 1)
item9.hist
stat.desc(Data$tol9, basic = TRUE, norm = TRUE)
# Item 10
item10.hist <- ggplot(Data, aes(Data$tol10)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 10", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol10, na.rm = TRUE), sd = sd(Data$tol10, na.rm = TRUE)), colour = "black", size = 1)
item10.hist
stat.desc(Data$tol10, basic = TRUE, norm = TRUE)
# Item 11
item11.hist <- ggplot(Data, aes(Data$tol11)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 11", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol11, na.rm = TRUE), sd = sd(Data$tol11, na.rm = TRUE)), colour = "black", size = 1)
item11.hist
stat.desc(Data$tol11, basic = TRUE, norm = TRUE)
# Item 12
item12.hist <- ggplot(Data, aes(Data$tol12)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 12", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol12, na.rm = TRUE), sd = sd(Data$tol12, na.rm = TRUE)), colour = "black", size = 1)
item12.hist
stat.desc(Data$tol12, basic = TRUE, norm = TRUE)
# Item 13
item13.hist <- ggplot(Data, aes(Data$tol13)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 13", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol13, na.rm = TRUE), sd = sd(Data$tol13, na.rm = TRUE)), colour = "black", size = 1)
item13.hist
stat.desc(Data$tol13, basic = TRUE, norm = TRUE)
# Item 14
item14.hist <- ggplot(Data, aes(Data$tol14)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 14", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol14, na.rm = TRUE), sd = sd(Data$tol14, na.rm = TRUE)), colour = "black", size = 1)
item14.hist
stat.desc(Data$tol14, basic = TRUE, norm = TRUE)
# Item 15
item15.hist <- ggplot(Data, aes(Data$tol15.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 15", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol15.r, na.rm = TRUE), sd = sd(Data$tol15.r, na.rm = TRUE)), colour = "black", size = 1)
item15.hist
stat.desc(Data$tol15.r, basic = TRUE, norm = TRUE)
# Item 16
item16.hist <- ggplot(Data, aes(Data$tol16.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 16", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol16.r, na.rm = TRUE), sd = sd(Data$tol16.r, na.rm = TRUE)), colour = "black", size = 1)
item16.hist
stat.desc(Data$tol16.r, basic = TRUE, norm = TRUE)
# Item 17
item17.hist <- ggplot(Data, aes(Data$tol17)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 17", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol17, na.rm = TRUE), sd = sd(Data$tol17, na.rm = TRUE)), colour = "black", size = 1)
item17.hist
stat.desc(Data$tol17, basic = TRUE, norm = TRUE)
# Item 18
item18.hist <- ggplot(Data, aes(Data$tol18.r)) + geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + labs(x = "Item 18", y = "Density") + stat_function(fun = dnorm, args = list(mean = mean(Data$tol18.r, na.rm = TRUE), sd = sd(Data$tol18.r, na.rm = TRUE)), colour = "black", size = 1)
item18.hist
stat.desc(Data$tol18.r, basic = TRUE, norm = TRUE)

# Building the Graded Response Model
library(ltm)
graded.model <- grm(Data)
summary(graded.model)

# Get item information for the whole set of 18 items
information(graded.model, c(-4, 4))

# Information given by each item in the scale, over the whole range
information(graded.model, c(-4, 4), items = 1)
information(graded.model, c(-4, 4), items = 2)
information(graded.model, c(-4, 4), items = 3)
information(graded.model, c(-4, 4), items = 4)
information(graded.model, c(-4, 4), items = 5)
information(graded.model, c(-4, 4), items = 6)
information(graded.model, c(-4, 4), items = 7)
information(graded.model, c(-4, 4), items = 8)
information(graded.model, c(-4, 4), items = 9)
information(graded.model, c(-4, 4), items = 10)
information(graded.model, c(-4, 4), items = 11)
information(graded.model, c(-4, 4), items = 12)
information(graded.model, c(-4, 4), items = 13)
information(graded.model, c(-4, 4), items = 14)
information(graded.model, c(-4, 4), items = 15)
information(graded.model, c(-4, 4), items = 16)
information(graded.model, c(-4, 4), items = 17)
information(graded.model, c(-4, 4), items = 18)

