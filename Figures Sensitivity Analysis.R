#Sensitivty analysis and visualisation by S.T Peeters and M.E. Kompier
#Last update 21-07-2021
#This script used the TaT and MLiT measures as a predictor for sleep-related outcome measures
#The dataset used for input should contain the TaT and MLiT metrics as calculated by the Matlab script
#and this should be combined with the corresponding data related to the outcome of interest, e.g. sleep
#This script performs a Linear Mixed Model using the data. However because of the high amount of thresholds
#the analysis is done in a loop, and combines the results in a dataframe. Based on this dataframe, a visualization
#is made, showing the resulting curve, as well as signifcant thresholds. 

#### Preparatory steps ####
library(readxl)

# Load data
# Note: All thresholds should be combined in one file with the dependent variables of interest
df <-  read_excel("YOURDATA.xlsx")
                                   
# Convert datetime to numeric for every threshold (e.g. 13:30 is 13.5)
library(chron) 
df$MLiT_20 <- hours(df$MLiT_rt_20) + minutes(df$MLiT_rt_20)/60
df$MLiT_22 <- hours(df$MLiT_rt_22) + minutes(df$MLiT_rt_22)/60
df$MLiT_25 <- hours(df$MLiT_rt_25) + minutes(df$MLiT_rt_25)/60
df$MLiT_28 <- hours(df$MLiT_rt_28) + minutes(df$MLiT_rt_28)/60
df$MLiT_32 <- hours(df$MLiT_rt_32) + minutes(df$MLiT_rt_32)/60
df$MLiT_35 <- hours(df$MLiT_rt_35) + minutes(df$MLiT_rt_35)/60
df$MLiT_40 <- hours(df$MLiT_rt_40) + minutes(df$MLiT_rt_40)/60
df$MLiT_45 <- hours(df$MLiT_rt_45) + minutes(df$MLiT_rt_45)/60
df$MLiT_50 <- hours(df$MLiT_rt_50) + minutes(df$MLiT_rt_50)/60
df$MLiT_56 <- hours(df$MLiT_rt_56) + minutes(df$MLiT_rt_56)/60
df$MLiT_63 <- hours(df$MLiT_rt_63) + minutes(df$MLiT_rt_63)/60
df$MLiT_71 <- hours(df$MLiT_rt_71) + minutes(df$MLiT_rt_71)/60
df$MLiT_79 <- hours(df$MLiT_rt_79) + minutes(df$MLiT_rt_79)/60
df$MLiT_89 <- hours(df$MLiT_rt_89) + minutes(df$MLiT_rt_89)/60
df$MLiT_112 <- hours(df$MLiT_rt_112) + minutes(df$MLiT_rt_112)/60
df$MLiT_126 <- hours(df$MLiT_rt_126) + minutes(df$MLiT_rt_126)/60
df$MLiT_141 <- hours(df$MLiT_rt_141) + minutes(df$MLiT_rt_141)/60
df$MLiT_158 <- hours(df$MLiT_rt_158) + minutes(df$MLiT_rt_158)/60
df$MLiT_178 <- hours(df$MLiT_rt_178) + minutes(df$MLiT_rt_178)/60
df$MLiT_200 <- hours(df$MLiT_rt_200) + minutes(df$MLiT_rt_200)/60
df$MLiT_224 <- hours(df$MLiT_rt_224) + minutes(df$MLiT_rt_224)/60
df$MLiT_251 <- hours(df$MLiT_rt_251) + minutes(df$MLiT_rt_251)/60
df$MLiT_282 <- hours(df$MLiT_rt_282) + minutes(df$MLiT_rt_282)/60
df$MLiT_316 <- hours(df$MLiT_rt_316) + minutes(df$MLiT_rt_316)/60
df$MLiT_355 <- hours(df$MLiT_rt_355) + minutes(df$MLiT_rt_355)/60
df$MLiT_398 <- hours(df$MLiT_rt_398) + minutes(df$MLiT_rt_398)/60
df$MLiT_447 <- hours(df$MLiT_rt_447) + minutes(df$MLiT_rt_447)/60
df$MLiT_501 <- hours(df$MLiT_rt_501) + minutes(df$MLiT_rt_501)/60
df$MLiT_562 <- hours(df$MLiT_rt_562) + minutes(df$MLiT_rt_562)/60
df$MLiT_631 <- hours(df$MLiT_rt_631) + minutes(df$MLiT_rt_631)/60
df$MLiT_708 <- hours(df$MLiT_rt_708) + minutes(df$MLiT_rt_708)/60
df$MLiT_794 <- hours(df$MLiT_rt_794) + minutes(df$MLiT_rt_794)/60
df$MLiT_891 <- hours(df$MLiT_rt_891) + minutes(df$MLiT_rt_891)/60
df$MLiT_100 <- hours(df$MLiT_rt_100) + minutes(df$MLiT_rt_100)/60
df$MLiT_1122 <- hours(df$MLiT_rt_1122) + minutes(df$MLiT_rt_1122)/60
df$MLiT_1259 <- hours(df$MLiT_rt_1259) + minutes(df$MLiT_rt_1259)/60
df$MLiT_1413 <- hours(df$MLiT_rt_1413) + minutes(df$MLiT_rt_1413)/60
df$MLiT_1585 <- hours(df$MLiT_rt_1585) + minutes(df$MLiT_rt_1585)/60
df$MLiT_1778 <- hours(df$MLiT_rt_1778) + minutes(df$MLiT_rt_1778)/60
df$MLiT_1995 <- hours(df$MLiT_rt_1995) + minutes(df$MLiT_rt_1995)/60
df$MLiT_1000 <- hours(df$MLiT_rt_1000) + minutes(df$MLiT_rt_1000)/60


df$NESTING2<-as.factor(df$NESTING2)
df$NESTING1<-as.factor(df$NESTING1)
df$COVAR1_factor<-as.factor(df$COVAR1_factor)
df$COVAR2_interval <- scale(df$COVAR2_interval, center = TRUE, scale = FALSE)

library(MuMIn)
library(knitr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)
library(plyr)

# Create a variable list of all thresholds for MliT and TaT separately
var_TaT <- c("TaT_20", "TaT_22", "TaT_25", "TaT_28", "TaT_32", "TaT_35", "TaT_40", "TaT_45", "TaT_50", "TaT_56", "TaT_63", "TaT_71", "TaT_79", "TaT_89","TaT_100","TaT_112", "TaT_126", "TaT_141", "TaT_158", "TaT_178", "TaT_200", "TaT_224", "TaT_251", "TaT_282", "TaT_316", "TaT_355", "TaT_398", "TaT_447", "TaT_501", "TaT_562", "TaT_631", "TaT_708", "TaT_794", "TaT_891", "TaT_1000", "TaT_1122", "TaT_1259", "TaT_1413", "TaT_1585", "TaT_1778", "TaT_1995")
var_MLiT <- c("MLiT_20", "MLiT_22", "MLiT_25", "MLiT_28", "MLiT_32", "MLiT_35", "MLiT_40", "MLiT_45", "MLiT_50", "MLiT_56", "MLiT_63", "MLiT_71", "MLiT_79", "MLiT_89","MLiT_100", "MLiT_112", "MLiT_126", "MLiT_141", "MLiT_158", "MLiT_178", "MLiT_200", "MLiT_224", "MLiT_251", "MLiT_282", "MLiT_316", "MLiT_355", "MLiT_398", "MLiT_447", "MLiT_501", "MLiT_562", "MLiT_631", "MLiT_708", "MLiT_794", "MLiT_891", "MLiT_1000", "MLiT_1122", "MLiT_1259", "MLiT_1413", "MLiT_1585", "MLiT_1778", "MLiT_1995")

# Function to create the dataframe with the information needed for the plot
# Note: adjust the model  to your data and likings
makeDataFrame <- function(varlist, DV) {
  sensitivity_estimates <- data.frame("predictor" = varlist)
  
  for (i in varlist) {
    var <- df[[i]]
    dv <- df[[DV]]
    model <- lmer(dv ~ var + COVAR1_factor + COVAR2_interval + (1|NESTING1)+  (1|NESTING1:NESTING2), 
                  data=df, REML= FALSE)

    freqtable_zero <- as.data.frame(table(model@frame[["var"]]))
    perc_zero=100*freqtable_zero$Freq[which(freqtable_zero$Var1 ==0)]/summary(model)[[3]][[2]][1]
    if(length(perc_zero)==0)
    {
      perc_zero=0
    }
    
    perc_low=sum(100*freqtable_zero$Freq[which(as.numeric(levels(freqtable_zero$Var1))[freqtable_zero$Var1] >75)]/summary(model)[[3]][[2]][1]) #For the lower thresholds: If more than 50% of the cases included had a TaTday above 75%, these were marked as not reliable.
    if(length(perc_low)==0)
    {
      perc_low=0
    }
    
    perc_high=sum(100*freqtable_zero$Freq[which(as.numeric(levels(freqtable_zero$Var1))[freqtable_zero$Var1] < (5.88))]/summary(model)[[3]][[2]][1]) #For the higher thresholds: If more than 50% of the cases included had a TaTday below 5.88% (which is equal to 30 minutes of exposure) these were marked as not reliable. 
    if(length(perc_high)==0)
    {
      perc_high=0
    }
    
  #here we first create an empty dataframe containing zeroes  
    sensitivity_estimates$coefficient[which(sensitivity_estimates$predictor == i)] <-0
    sensitivity_estimates$se[which(sensitivity_estimates$predictor == i)] <-0
    sensitivity_estimates$pvalue[which(sensitivity_estimates$predictor == i)] <- 0
    sensitivity_estimates$N[which(sensitivity_estimates$predictor == i)] <- 0
    sensitivity_estimates$R[which(sensitivity_estimates$predictor == i)] <- 0
    
    sensitivity_estimates$zeros[which(sensitivity_estimates$predictor == i)] <- 0
    sensitivity_estimates$higherboundary[which(sensitivity_estimates$predictor == i)] <- 0
    sensitivity_estimates$lowerboundary[which(sensitivity_estimates$predictor == i)] <- 0
    
  #here we save the results of our LMM analysis with the different thresholds to a dataframe  
    sensitivity_estimates$coefficient[which(sensitivity_estimates$predictor == i)] <-summary(model)$coefficients[2,1]
    sensitivity_estimates$se[which(sensitivity_estimates$predictor == i)] <-summary(model)$coefficients[2,2]
    sensitivity_estimates$pvalue[which(sensitivity_estimates$predictor == i)] <- summary(model)$coefficients[2,5]
    sensitivity_estimates$N[which(sensitivity_estimates$predictor == i)] <- summary(model)[[3]][[2]][1]
    sensitivity_estimates$R[which(sensitivity_estimates$predictor == i)] <- r.squaredGLMM(model)[1]
    
    sensitivity_estimates$zeros[which(sensitivity_estimates$predictor == i)] <- perc_zero
    sensitivity_estimates$higherboundary[which(sensitivity_estimates$predictor == i)] <- perc_high
    sensitivity_estimates$lowerboundary[which(sensitivity_estimates$predictor == i)] <- perc_low
  }
  
  sensitivity_estimates$significant <- factor(ifelse(sensitivity_estimates$pvalue <.05, 1, 0), levels = c(1,0))
  sensitivity_estimates$predictor <- factor(sensitivity_estimates$predictor, levels = varlist)
  sensitivity_estimates$TaT <- 10^seq(1.3, 3.3, by=0.05)
     
  
  return(sensitivity_estimates)
}

#### TaT #####

#here we specifically create a dataframe for the TaT with the dependent variable of interest
sensitivity_TaT_DV <- makeDataFrame(var_TaT, "DependentVariableName")
write_xlsx(sensitivity_TaT_DV,"TaT_DV_sensitivity_data.xlsx")

#### PLOTS TAT #### 
#here we make a visualization based on the dataframe for TaT in combination with our dependent variable

sensitivity_plot_TaT_DV <- ggplot(sensitivity_TaT_DV, aes(x = TaT, y = coefficient)) + 
  annotate("rect", xmin = 0, xmax = 10^1.725, ymin = -Inf, ymax = Inf, alpha = .5,fill = "gray75")+ # low boundary 
  annotate("rect", xmin = 10^2.525, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5,fill = "gray75")+ # high boundary 
  geom_hline(yintercept=0) +    
  geom_line(aes(group =1)) + 
  geom_errorbar(width=.05, aes(ymin=coefficient-se, ymax=coefficient+se, colour = significant)) +
  geom_point(aes(colour = significant)) +
  scale_colour_manual(values = c("gray0","gray50"), values = c(face = "bold", face = "regular" ),  name = "Significant",  labels = c("p < .05","NS")) +
  ylab("Estimate ± SE") +
  scale_x_log10()+
  annotation_logticks(sides = "b")  +
  coord_cartesian(ylim = c(-0.1, 0.1))+
  xlab("Threshold Levels in Lux")+
  theme_bw() +
  ggtitle(expression(paste("Dependent Variable by ", TaT[day])))
  
tiff('Sensitivity_plot_TaT_DV.tiff', width = 10, height = 3.75, units = "in" ,res = 600, compression = 'lzw')
sensitivity_plot_TaT_DV
dev.off()


#### MLiT ####
#here we do the same as above but for MLiT

sensitivity_MLiT_DV <- makeDataFrame(var_MLiT, "DependentVariableName")
write_xlsx(sensitivity_MLiT_DV,"MLiT_DV_sensitivity_data.xlsx")

#### PLOTS MLiT ####
#here we do the same as above but for MLiT

sensitivity_plot_MLiT_DV <- ggplot(sensitivity_MLiT_DV, aes(x = MLiT, y = coefficient))+ 
  annotate("rect", xmin = 0, xmax = 10^1.725, ymin = -Inf, ymax = Inf, alpha = .5,fill = "gray75")+ # low boundary 
  annotate("rect", xmin = 10^2.525, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = .5,fill = "gray75")+ # high boundary 
  geom_hline(yintercept=0) +    
  geom_line(aes(group =1)) + 
  geom_errorbar(width=.05, aes(ymin=coefficient-se, ymax=coefficient+se, colour = significant)) +
  geom_point(aes(colour = significant)) +
  scale_colour_manual(values = c("gray0","gray50"), name = "Significant",  labels = c("p < .05","NS")) + 
  ylab("Estimate ± SE") +
  scale_x_log10()+
  annotation_logticks(sides = "b")  +
  coord_cartesian(ylim = c(-1, 1))+
  xlab("Threshold Levels in Lux")+
  theme_bw() +
  ggtitle(paste("Dependent Variable by MLiT"))

tiff('Sensitivity_plot_MLiT_DV.tiff', width = 10, height = 3.75, units = "in" ,res = 600, compression = 'lzw')
sensitivity_plot_MLiT_DV
dev.off()