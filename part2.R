library(datasets)
library(ggplot2)

# Load the dataset
data = ToothGrowth

# Look at data attributes
str(data)

# Look at data summary
summary(data)

# Check for any missing values
sum(complete.cases(data)) == nrow(data)

# How are doses and supplement divided
table(data$dose,data$supp)

# Check number of unique dose values
unique(data$dose)

# Convert the dose to factor since only three different levels exist
data$dose = as.factor(data$dose)

# Tooth Length Variation with supplement
p = ggplot(data,aes(supp,len))
p + geom_boxplot() + xlab('Supplement Type') + ylab('Tooth Length (in mm)') + 
  labs(title='Tooth Length VS Supplement Type')

# Tooth Length Variation with doses
p = ggplot(data,aes(dose,len))
p + geom_boxplot() + xlab('Dose') + ylab('Tooth Length (in mm)') + 
  labs(title='Tooth Length VS Dose')


# Tooth Length Variation with doses
p = ggplot(data,aes(dose,len))
p + geom_boxplot() + xlab('Dose') + ylab('Tooth Length (in mm)') + 
  labs(title='Tooth Length VS Dose And Supplement') + facet_wrap(~supp)


# Does Tooth Length vary with Dose for Orange Juice?  
df = data[data$supp=="OJ" & data$dose %in% c(0.5,1.0),]
t.test(len ~ dose, data = df, var.equal=FALSE, paired = FALSE, alternative="greater")

df = data[data$supp=="OJ" & data$dose %in% c(1.0,2.0),]
t.test(len ~ dose, data = df, var.equal=FALSE, paired = FALSE, alternative="greater")


# Does Tooth Length vary with Dose for Vitamin C?  
df = data[data$supp=="VC" & data$dose %in% c(0.5,1.0),]
t.test(len ~ dose, data = df, var.equal=FALSE, paired = FALSE, alternative="greater")

df = data[data$supp=="VC" & data$dose %in% c(1.0,2.0),]
t.test(len ~ dose, data = df, var.equal=FALSE, paired = FALSE, alternative="greater")

# Does Tooth Length vary with just supplement type?
df = data
t.test(len ~ supp, data = df, var.equal=FALSE, paired = FALSE)

# Does Tooth Length vary with supplement type for Dose of 0.5
df = data[data$dose %in% c(0.5),]
t.test(len ~ supp, data = df, var.equal=FALSE, paired = FALSE,alternative="less")

# Does Tooth Length vary with supplement type for Dose of 1.0
df = data[data$dose %in% c(1.0),]
t.test(len ~ supp, data = df, var.equal=FALSE, paired = FALSE,alternative="less")

# Does Tooth Length vary with supplement type for Dose of 2.0
df = data[data$dose %in% c(2.0),]
t.test(len ~ supp, data = df, var.equal=FALSE, paired = FALSE)









