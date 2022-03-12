rm(list=ls())
destfile = "C:/Users/User/OneDrive/FH Köln/Unterricht/Finanzmarktökonometrie/Daten/"
File_Factors = "CRSP_Monthly.csv"

Data_Factors = read.delim(paste(destfile, File_Factors, sep = "", collapse = NULL), header = TRUE, sep = ",", dec = ".")

# Construct January-Dummy
D = ifelse(substr(Data_Factors[,1], 5, 6) %in% "01", 1, 0)

# Transform to continuous
r = log(1+(Data_Factors[,2]+Data_Factors[,5])/100)*100

# Full sample regression
summary(lm(r~D))


# Split sample results
FirstHalf = round(length(r)/2)

# - First half: mild statistical significance
summary(lm(r[1:FirstHalf]~D[1:FirstHalf]))

# - Second half: Reverse sign
summary(lm(r[FirstHalf+1:length(r)]~D[FirstHalf+1:length(r)]))