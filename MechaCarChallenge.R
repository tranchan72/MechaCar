MechaCar_mpg <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F) # Read csv
head(MechaCar_mpg) # see first rows of dataset
lm( mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=MechaCar_mpg) #generate multiple linear regression model
summary(lm( mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance,data=MechaCar_mpg)) # generate summary statistic
Suspension_Coil <- read.csv('Suspension_Coil.csv', stringsAsFactors = F) # Read csv
library(magrittr)
library(dplyr)
library(ggplot2)
head(Suspension_Coil) # see first rows of dataset
statistics_summary <- Suspension_Coil %>%  
  summarise(mean = mean(PSI), median = median(PSI), std_dev = sd(PSI), variance = var(PSI),n = n())
ggplot(Suspension_Coil,aes(x=PSI)) + geom_density() # distribution plot
sample_table <- Suspension_Coil %>% sample_n(50) #randomly sample 50 data points
t.test(log10(sample_table$PSI),mu=mean(log10(Suspension_Coil$PSI))) #compare sample versus population means
