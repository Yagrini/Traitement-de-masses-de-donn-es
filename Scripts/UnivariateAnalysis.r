library(ggplot2)
library(plyr)
library(stringr)
Sys.setlocale("LC_ALL","sv_SE.UTF-8")
######################################################################
## paths
######################################################################
data_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Data/googleplaystore.csv"
save_plot_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Graphiques/UnivariateAnalysiss/"

######################################################################
## Load Data
######################################################################
googleplaystore = read.csv(data_path)

######################################################################
#Data Identification
######################################################################
str(googleplaystore)
summary(googleplaystore)
head(googleplaystore)

####################################################
#App column
####################################################
str(googleplaystore$App)
App_occurences = table(googleplaystore$App) # Returns table of occurences of a categorical variable
App_occurences_data_frame = data.frame(App_occurences) # convert table of occurences to dataframe
colnames(App_occurences_data_frame)[1] <- "App" # change the first column name to App
App_occurences_data_frame$Freq <- as.factor(App_occurences_data_frame$Freq) # convert Freq to factor 

ggplot(App_occurences_data_frame, aes(x = Freq, fill = Freq)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  xlab("Frequency of appearence") +
  ylab("Number of application associated") +
  guides(fill = FALSE)

ggsave(paste(save_plot_path,"AppFrequency.pdf"), device = "pdf")

####################################################
# Category column
####################################################
str(googleplaystore$Category)
ggplot(googleplaystore, aes(x = Category, fill = Category)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 2.3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title="Number of apps by category", y = "") +
  guides(fill = FALSE) 

ggsave(paste(save_plot_path,"CategoryFrequency.pdf"), device = "pdf")

# inspect the application with the category 1.9
googleplaystore[googleplaystore$Category=='1.9',] 
# Deleting this line, see in the analysis report why
googleplaystore <- googleplaystore[-which(googleplaystore$Category=='1.9'),] 

####################################################
#Rating Column
####################################################
summary(googleplaystore$Rating) ###### 1474 Missing values
## Histogram
ggplot(googleplaystore, aes(x = Rating)) +
  geom_histogram(aes(y=..density..),colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")
ggsave(paste(save_plot_path,"RatingHist.pdf"), device = "pdf")
## BoxPlot
ggplot(googleplaystore, aes(y = Rating)) +
  geom_boxplot()
ggsave(paste(save_plot_path,"RatingBoxPlot.pdf"), device = "pdf")

####################################################
#Reviews Column
####################################################
str(googleplaystore$Reviews)
summary(googleplaystore$Reviews)
summary(as.numeric(as.character(googleplaystore$Reviews))) 
## Histogram of numerical values of Reviews
ggplot(googleplaystore, aes(x = as.numeric(as.character(googleplaystore$Reviews)))) +
  geom_histogram(colour="black", fill="blue", alpha=.5)+
  xlab("Reviews") +
  labs(title="Number of apps by reviews range")
ggsave(paste(save_plot_path,"ReviewsHist.pdf"), device = "pdf")
## BoxPlot of numerical values of Reviews
ggplot(googleplaystore, aes(y = as.numeric(as.character(googleplaystore$Reviews)))) +
  geom_boxplot() +
  ylab("Reviews")
ggsave(paste(save_plot_path,"ReviewsBoxPlot.pdf"), device = "pdf")
## Cumulative distribution function of numerical values of Reviews
ggplot(googleplaystore, aes(x = as.numeric(as.character(Reviews)))) +
  stat_ecdf(color = 'blue') +
  labs(title="Cumulative distribution function of reviews",x="Reviews", y = "")
ggsave(paste(save_plot_path,"ReviewsCDF.pdf"), device = "pdf")

####################################################
#Installs Column
####################################################
summary(googleplaystore$Installs)
str(googleplaystore$Installs)
## Order factor levels
googleplaystore$Installs <- factor(googleplaystore$Installs,levels = c("0","0+","1+","5+","10+","50+","100+",
                                                                       "500+","1,000+","5,000+","10,000+",
                                                                       "50,000+","100,000+","500,000+","1,000,000+","5,000,000+",
                                                                       "10,000,000+","50,000,000+","100,000,000+","500,000,000+",
                                                                       "1,000,000,000+"), order = TRUE)
ggplot(googleplaystore[unique(googleplaystore$App),], aes(Installs)) +
  geom_bar() +
  geom_bar(fill = '#FECBA9', color = 'black') +
  geom_text(stat='count', aes(label=..count..), vjust=-1,size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #+ coord_flip() +
  labs(title = "Number of apps by Install")
ggsave(paste(save_plot_path,"Installs.pdf"), device = "pdf")

## Check the existence of applications with fewer installations than the number of Reviews
googleplaystore[as.numeric(as.character(googleplaystore$Reviews)) > as.numeric(as.character(str_remove_all(googleplaystore$Installs,"[,+]"))),]

####################################################
#Size Column
####################################################
summary(googleplaystore$Size)
str(googleplaystore$Size) 
## Conversion to mega_bytes
mega_bytes <- as.numeric(str_remove_all(googleplaystore$Size, "M"))  
mega_bytes[is.na(mega_bytes)] <- 0
kilo_bytes <- as.numeric(str_remove_all(googleplaystore$Size, "k")) / 1024
kilo_bytes[is.na(kilo_bytes)] <- 0
size_bytes <- kilo_bytes + mega_bytes
## Change the Size of apps with size equal to -2 to NA values
size_bytes[size_bytes==0] <- NA
summary(size_bytes) 
googleplaystore$Size <- size_bytes
## Histogram
ggplot(googleplaystore,aes(x = Size )) +
  geom_histogram(color='black', fill='white') +
  xlab("Size in MegaBytes") +
  labs(title="Number of apps by Size range")
ggsave(paste(save_plot_path,"SizeHist.pdf"), device = "pdf")
## BoxPlot
ggplot(googleplaystore,aes(y = Size)) +
  geom_boxplot(color='black', fill='white') +
  ylab("Size in megabytes")
ggsave(paste(save_plot_path,"SizeBoxPlot.pdf"), device = "pdf")

####################################################
#Type Column
####################################################
str(googleplaystore$Type)
summary(googleplaystore$Type)
## There is one row with NaN value, let's check this row
googleplaystore[googleplaystore$Type=='NaN',]
## As shown, price of this app is equal to 0 which means the App is Free
## change the value of Type to Free
googleplaystore[googleplaystore$Type=='NaN',]$Type <- 'Free'

ggplot(googleplaystore[unique(googleplaystore$App),], aes(x = Type, fill = Type)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  labs(title = "Number of apps by type") +
  guides(fill = FALSE) 
ggsave(paste(save_plot_path,"TypeFrequency.pdf"), device = "pdf")

####################################################
#Price Column
####################################################
summary(test$Price)
## We already know that paid apps represent just 800 row of our dataset, so for good visualization of the histogram 
## we'll plot just price higher than 0
## Convert price to numeric 
prices <- as.numeric(str_remove_all(googleplaystore[googleplaystore$Price != '0',]$Price, "\\$"))
summary(prices)
## Histogram
ggplot(aes(x = prices)) + 
  geom_histogram(binwidth = 2, color='black', fill='white') +
  scale_x_continuous(breaks = seq(0,400,10), labels = seq(0,400,10)) +
  theme(axis.text.x = element_text(angle = 80, hjust = 1)) +
  labs(title = "Number of apps by Price")+
  xlab("Prices")
ggsave(paste(save_plot_path,"PricesHist.pdf"), device = "pdf")

####################################################
#Content.Rating Column
####################################################
str(googleplaystore$Content.Rating)
levels(googleplaystore$Content.Rating)
summary(googleplaystore$Content.Rating)
googleplaystore[googleplaystore$Content.Rating=='Unrated',]
## Bar_Plot
ggplot(googleplaystore, aes(x = Content.Rating, fill = Content.Rating)) +
  geom_bar() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) +
  guides(fill = FALSE) +
  labs(title = "Number of apps by content rating") +
  xlab("Content Rating")
ggsave(paste(save_plot_path,"ContentRatingFreq.pdf"), device = "pdf")

####################################################
#Genres Column
####################################################
str(googleplaystore$Genres)
levels(googleplaystore$Genres)
summary(googleplaystore$Genres)
## Bar_Plot For FAMILY Category
ggplot(googleplaystore[googleplaystore$Category %in% c('FAMILY'),], aes(x = Genres, fill = Genres)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Number of apps by genre in category FAMILY") +
  guides(fill = FALSE)
ggsave(paste(save_plot_path,"FamilyGenres.pdf"), device = "pdf")
## Bar_Plot For GAME Category
ggplot(googleplaystore[googleplaystore$Category %in% c('GAME'),], aes(x = Genres, fill = Genres)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  +
  labs(title = "Number of apps by genre in category GAME") +
  guides(fill = FALSE)
ggsave(paste(save_plot_path,"GameGenres.pdf"), device = "pdf")

####################################################
#Last.Updated Column
####################################################
str(googleplaystore$Last.Updated)
## Split the date into day, month and year.
## With that we could have a good visalization of last updated column
dates <- str_split(str_remove_all(googleplaystore$Last.Updated, ",")," ")
months <- sapply(dates, function(t){return (paste(t[1],t[3],sep = " "))})
days <- sapply(dates, function(t){return (t[2])})
years <- sapply(dates, function(t){return (t[3])})
last_update <- data.frame(months = months, years = years)
## BarPlot for year of Last.Updated 
ggplot(last_update, aes(x = years, fill = years)) +
  geom_bar() +
  guides(fill = FALSE) +
  xlab("Year") +
  labs(title = "Number of apps by year of last update")
ggsave(paste(save_plot_path,"LastUpdatYearFreq.pdf"), device = "pdf")
## BarPlot for month-year of Last.Updated 
ggplot(last_update,aes(x = months, fill = years)) +
  geom_bar() + 
  guides(fill = FALSE) +
  geom_text(stat='count', aes(label=..count..), hjust=-0.5,size=2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "Number of apps by month and year of last update") +
  xlab("month - Year") +
  coord_flip()
ggsave(paste(save_plot_path,"LastUpdatMonthYearFreq.pdf"), device = "pdf",height = 13)

####################################################
#Current.Ver Column
####################################################
str(googleplaystore$Current.Ver)
summary(googleplaystore$Current.Ver)

####################################################
#Android.Ver Column
####################################################
str(googleplaystore$Android.Ver)
summary(googleplaystore$Android.Ver)
## BarPlot
ggplot(googleplaystore, aes(x = Android.Ver, fill = Android.Ver)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_text(stat='count', aes(label=..count..), vjust=0.5, hjust = -0.1, size=3)+
  coord_flip() + labs(title="Number of apps by supported Android Version") + guides(fill = FALSE) +
  ylab("Android Version")
ggsave(paste(save_plot_path,"AndroidVerFreq.pdf"), device = "pdf")
## there is 2 nan apps and 1262 app with version varies with device
googleplaystore[googleplaystore$Android.Ver == 'NaN',]
## Apps with Android.Ver varies with device, Current.Ver varies with device and Size varies with device 
AndVerVaries <- googleplaystore[googleplaystore$Android.Ver=='Varies with device' & googleplaystore$Current.Ver=='Varies with device' & googleplaystore$Size=='Varies with device',]
nrow(AndVerVaries)
nrow(AndVerVaries)/nrow(googleplaystore[googleplaystore$Android.Ver=='Varies with device',])
## 1247 apps have the 3 fields varies with device
