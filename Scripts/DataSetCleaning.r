library(stringr)
library(dplyr)
library(plyr)
library(ggplot2)
Sys.setlocale("LC_ALL","sv_SE.UTF-8")
######################################################################
##First  thing is defining input and output paths
######################################################################
data_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Data/googleplaystore.csv"
output_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Data/googleplaystoreCleaned.csv"
googleplaystore <- read.csv(data_path)

######################################################################
## Correct is the row with is shifted
######################################################################
row_free_installs <- data.frame(googleplaystore[googleplaystore$Installs == 'Free',]$App ,NA, googleplaystore[googleplaystore$Installs == 'Free',c(2:12)])
names(row_free_installs) <- names(googleplaystore)
googleplaystore[googleplaystore$Installs == 'Free',] <- row_free_installs

######################################################################
## Convert Reviews to numerical values
######################################################################
googleplaystore$Reviews <- as.numeric(as.character(googleplaystore$Reviews))

######################################################################
## Eliminate applications with the same names and leave one with the highest number of Reviews
######################################################################
googleplaystore_one_occurence <- googleplaystore %>% 
                                  group_by(App) %>%
                                  filter(Reviews == max(Reviews)) %>%
                                  filter(!duplicated(App))

######################################################################
# Elimination of the line that contains NA in Category and Genre
######################################################################
googleplaystore_one_occurence <- googleplaystore_one_occurence[-which(is.na(googleplaystore_one_occurence$Category)),]

######################################################################
# Remove commas and '+' symbol from Installs.
######################################################################
googleplaystore_one_occurence$Installs <- str_remove_all(googleplaystore_one_occurence$Installs,"[,+]")
googleplaystore_one_occurence$Installs <- as.factor(googleplaystore_one_occurence$Installs)

######################################################################
# Convert Size Column to numeric mega_bytes
######################################################################
mega_octets <- as.numeric(str_remove_all(googleplaystore_one_occurence$Size, "M"))   
mega_octets[is.na(mega_octets)] <- 0
kilo_octets <- as.numeric(str_remove_all(googleplaystore_one_occurence$Size, "k")) /1024 
kilo_octets[is.na(kilo_octets)] <- 0
size_octets <- kilo_octets + mega_octets
size_octets[size_octets == 0] <- NA
googleplaystore_one_occurence$Size <- size_octets

######################################################################
# Correct App that has NaN on type
######################################################################
googleplaystore_one_occurence[googleplaystore_one_occurence$Type=='NaN',]$Type <- 'Free'

######################################################################
# Convert Price to numeric
######################################################################
googleplaystore_one_occurence$Price <- as.numeric(str_remove_all(googleplaystore_one_occurence$Price, "\\$"))

######################################################################
# Remove the 2 Apps with Content.Rating = 'Unrated'
######################################################################
googleplaystore_one_occurence <- googleplaystore_one_occurence[-which(googleplaystore_one_occurence$Content.Rating=='Unrated'),] 
nrow(googleplaystore_one_occurence)

######################################################################
# Fixe apps whose Android.Ver field is not given.
######################################################################
levels(googleplaystore_one_occurence$Android.Ver) <- c(levels(googleplaystore_one_occurence$Android.Ver), '8.1', '8.1 and 9.0')
googleplaystore_one_occurence[googleplaystore_one_occurence$App == '[substratum] Vacuum: P',]$Android.Ver <- '8.1 and 9.0'
googleplaystore_one_occurence[googleplaystore_one_occurence$App == 'Pi Dark [substratum]',]$Android.Ver <- '8.1'

######################################################################
# Add Last.Updated.Year Column 
######################################################################
dates <- str_split(str_remove_all(googleplaystore_one_occurence$Last.Updated, ",")," ")
years <- sapply(dates, function(t){return (t[3])})
googleplaystore_one_occurence$Last.Updated.Year <- years
googleplaystore_one_occurence$Last.Updated.Year <- as.factor(googleplaystore_one_occurence$Last.Updated.Year)
summary(googleplaystore_one_occurence$Last.Updated.Year)

######################################################################
# Add Size.level column with levels : NA, Lowest, Low, Medium, High
######################################################################
Sice.level <- ifelse(googleplaystore_one_occurence$Size<0, NA, ifelse(googleplaystore_one_occurence$Size<=1 & googleplaystore_one_occurence$Size>=0, "Lowest",
                                                                      ifelse(googleplaystore_one_occurence$Size<=10 & googleplaystore_one_occurence$Size>1,"Low", 
                                                                      ifelse(googleplaystore_one_occurence$Size<=50 & googleplaystore_one_occurence$Size>10,"Medium","High"))))
Sice.level <- factor(Sice.level, levels = c("Lowest","Low","Medium","High"), ordered = TRUE)
googleplaystore_one_occurence$Size.level <- Sice.level
######################################################################
# Add Price.level with levels : "0","0-1","1-5","5-10","10-25","25-50","74-120","140-160","195-215","290-310","370-390","390-400"
######################################################################
Price.level <- ifelse(googleplaystore_one_occurence$Price==0, 0, ifelse(googleplaystore_one_occurence$Price<=1 & googleplaystore_one_occurence$Price>0, "0-1",ifelse(googleplaystore_one_occurence$Price<=5 & googleplaystore_one_occurence$Price>1,"1-5", 
                                                                                       ifelse(googleplaystore_one_occurence$Price<=10 & googleplaystore_one_occurence$Price>5,"5-10",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=25 & googleplaystore_one_occurence$Price>10,"10-25",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=50 & googleplaystore_one_occurence$Price>25,"25-50",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=120 & googleplaystore_one_occurence$Price>=74,"74-120",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=160 & googleplaystore_one_occurence$Price>=140,"140-160",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=215 & googleplaystore_one_occurence$Price>=195,"195-215",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=310 & googleplaystore_one_occurence$Price>=290,"290-310",
                                                                                       ifelse(googleplaystore_one_occurence$Price<=390 & googleplaystore_one_occurence$Price>=370,"370-390","390-400")))))))))))

Price.level <- factor(Price.level, levels=c("0","0-1","1-5","5-10","10-25","25-50","74-120","140-160","195-215","290-310","370-390","390-400"), ordered = TRUE)
googleplaystore_one_occurence$Price.level <- Price.level

######################################################################
# Save the cleaned dataset
######################################################################
write.csv(googleplaystore_one_occurence, file = output_path,row.names = FALSE)



