library(readxl)
library(openxlsx)
library(ggplot2)

JorgeProject <- read_excel("janka.xlsx")

getwd()
setwd("C:\Users\jorge\Desktop\Roosevelt\CST 309 Data Mining\janka.xlsx")
file_path <- "C:\Users\jorge\Desktop\Roosevelt\CST 309 Data Mining\janka.xlsx"


#1
#For Density and Hardness Attribute:
invisible(
            {
                message <- "\n\nFor Density."
                message2 <- "\n\nFor Hardness."
                formatted_message <- sprintf("\033[1m%s\033[0m", message)    #For Bold Text
                formatted_message2 <- sprintf("\033[1m%s\033[0m", message2)  #For Bold Text
                cat(formatted_message)
                cat("\nMean Density----> ", mean(JorgeProject$Density))
                cat("\nVariance Density----> ", var(JorgeProject$Density))
                cat("\nStandard Deviation Density----> ", sd(JorgeProject$Density), "\n")
                cat(formatted_message2)
                cat("\nMean Hardness----> ", mean(JorgeProject$Hardness))
                cat("\nVariance Hardness----> ", var(JorgeProject$Hardness))
                cat("\nStandard Deviation Hardness----> ", sd(JorgeProject$Hardness))
                cat("\n\n")
             }
          )


#2
# Calculate the covariance matrix:
cov_matrix <- cov(JorgeProject)
cov1 <- cov(JorgeProject$Density,JorgeProject$Hardness)

invisible(
            {
              cat("\n\n")
              message3 <- "<<<<<<<<<<The Covariance Matrix Is>>>>>>>>>>\n\n"
              formatted_message3 <- sprintf("\033[1m%s\033[0m", message3)   #For Bold Text
              cat(formatted_message3)
              print(cov_matrix)
              cat("\nThe Covariance is ------> ", cov1)
              cat("\n\n")
            }
          )


#3
# Histograms For Each Attributes:
colors = c("blue", "black", "green", "red", "orange","yellow","pink", "cyan")

hist(JorgeProject$Density,right=FALSE,col=colors,main="Density Histogram")

hist(JorgeProject$Hardness,right=FALSE,col=colors,main="Hardness Histogram")


#4
# Experimental CDF for each attribute and plot it:
Result = ecdf(JorgeProject$Density)
plot(Result, main="Experimental CDF For Density")

Result1 = ecdf(JorgeProject$Hardness)
plot(Result1, main="Experimental CDF For Hardness")


#5
# Percentiles for each attribute with a step 0.1:
q_Density <- quantile(JorgeProject$Density, seq(0, 1, 0.1))
q_Hardness <- quantile(JorgeProject$Hardness, seq(0, 1, 0.1))

invisible (
            {
              cat("\n\n")
              message4 <- "<<<<<<<<<<<<<<<<<  Percentiles For Density  >>>>>>>>>>>>>>>>>>>\n"
              formatted_message4 <- sprintf("\033[1m%s\033[0m", message4)   #For Bold Text
              cat(formatted_message4)
              print(q_Density)
              cat("\n\n")
              message5 <- "<<<<<<<<<<<<<<<<<<<<<<<  Percentiles For Hardness  >>>>>>>>>>>>>>>>>>>>>>>\n"
              formatted_message5 <- sprintf("\033[1m%s\033[0m", message5)   #For Bold Text
              cat(formatted_message5)
              print(q_Hardness)
              cat("\n")
            }
          )

#6
# Quantile-to-quintile plot between attributes:
plot(q_Density, main = "Quantile-To-Quintile For Density")
plot(q_Hardness, main = "Quantile-To-Quintile For Hardness")

