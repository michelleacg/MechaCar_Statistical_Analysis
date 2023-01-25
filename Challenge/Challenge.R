install.packages("tidyverse")
install.packages("jsonlight")
install.packages("dplyr")
library(tidyverse)

df_MechaCar <- read.csv(file="MechaCar_mpg.csv")
lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data=df_MechaCar)
summary(lm(mpg~vehicle_length+vehicle_weight+spoiler_angle+ground_clearance+AWD,data=df_MechaCar))

#
df_Coil <- read.csv(file="Suspension_Coil.csv")
summary_coil <- df_Coil %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),STD=sd(PSI),.groups="keep")
summary_Lot <- df_Coil %>% group_by(Manufacturing_Lot) %>% summarize(Mean=mean(PSI),Median=median(PSI),Variance=var(PSI),STD=sd(PSI),.groups="keep")
#ggplot



#3

#we are going to do a t test in the entire lot and then we are going to do an entire test in the sub set

t.test(df_Coil$PSI,mu=1500)


#3 test with each lot (Summary lot)
t.test(df_Coil$PSI[df_Coil$Manufacturing_Lot == "Lot1"],mu=1500)
t.test(df_Coil$PSI[df_Coil$Manufacturing_Lot == "Lot2"],mu=1500)
t.test(df_Coil$PSI[df_Coil$Manufacturing_Lot == "Lot3"],mu=1500)

