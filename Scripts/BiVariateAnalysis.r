library(corrplot)
library(ggplot2)
library(plyr)
library(dplyr)
Sys.setlocale("LC_ALL","sv_SE.UTF-8")

################################################################################################
# Read files of MultiplePlot and TextMining
################################################################################################
source("/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Scripts/MultiplePlot.r")
source("/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Scripts/TextMining.r")

################################################################################################
# paths
################################################################################################
cleaned_data_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Data/googleplaystoreCleaned.csv"
save_plot_path <- "/Users/mac/Desktop/RPractice/AnalyseExploratoireProject/Graphiques/"

################################################################################################
# Load Data
################################################################################################
googleplaystore <- read.csv(cleaned_data_path)
str(googleplaystore)

################################################################################################
# Convert some columns to Factor
################################################################################################
googleplaystore$Installs <- as.factor(googleplaystore$Installs)
googleplaystore$Last.Updated.Year <- as.factor(googleplaystore$Last.Updated.Year)
googleplaystore$Size.level <- factor(googleplaystore$Size.level,levels = c("Lowest","Low","Medium","High"), ordered = TRUE)
googleplaystore$Price.level <- factor(googleplaystore$Price.level,levels = c("0","0-1","1-5","5-10","10-25","25-50","74-120","140-160","195-215","290-310","370-390","390-400"))

################################################################################################
# Category & Rating
################################################################################################
ggplot(googleplaystore , aes(x = Rating, fill = Category)) + 
  geom_histogram(bins = 50, position = "identity") + xlim(0,5) + 
  facet_wrap(~Category) + guides(fill = FALSE) + 
  ggtitle("Distribution of ratings across categories") + ylab("Count")

ggsave(paste(save_plot_path,"RatingByCategoris.pdf"), device = "pdf")

################################################################################################
# Category & Installs
################################################################################################
installs_by_category <- ddply(googleplaystore, .(Category), summarize, sum_installs = sum(as.numeric(as.character(Installs))),
                              mean_installs = mean(as.numeric(as.character(Installs))),
                              apps_number =   length(as.character(Installs)))
p1 <- ggplot(installs_by_category,aes(x = Category, y = mean_installs, group = 1)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + 
  ylab("Mean of installs") 

p2 <- ggplot(installs_by_category,aes(x = Category, y = sum_installs, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ylab("Sum of installs")

p3 <- ggplot(installs_by_category,aes(x = Category, y = apps_number, group = 1)) + 
  geom_point(color = 'green') +
  geom_line(color = 'green')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of apps")

multiplot(p1,p2,p3)
dev.print(pdf, paste(save_plot_path,"InstallsCategory.pdf"),height = 12)
dev.off()
################################################################################################
# Category & Reviews
################################################################################################
reviews_by_category <- ddply(googleplaystore, .(Category), summarize, sum_reviews = sum(Reviews),
                              mean_reviews = mean(Reviews),
                              apps_number =   length(Reviews))
p1 <- ggplot(reviews_by_category,aes(x = Category, y = mean_reviews, group = 1)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) + 
  ylab("Mean of Reviews") 

p2 <- ggplot(reviews_by_category,aes(x = Category, y = sum_reviews, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  ylab("Sum of Reviews")

p3 <- ggplot(reviews_by_category,aes(x = Category, y = apps_number, group = 1)) + 
  geom_point(color = 'green') +
  geom_line(color = 'green')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of apps")

multiplot(p1,p2,p3)
dev.print(pdf, paste(save_plot_path,"ReviewsCategory.pdf"),height = 12)
dev.off()

################################################################################################
##  Correlations
################################################################################################
no_na_rating <- googleplaystore[!is.na(googleplaystore$Rating) ,]
no_na_rating$Installs <- as.numeric(as.character(no_na_rating$Installs))

cor(no_na_rating[c(3:4,6,8)]) %>%
  corrplot(method="number",col = colorRampPalette(c("blue", "white", "red"))(20))

dev.print(pdf, paste(save_plot_path,"CorrelationMatrixPlot.pdf"),height = 15)
dev.off()


################################################################################################
### Installs & Size
################################################################################################
installs_by_size <- ddply(googleplaystore, .(Size.level), summarize, sum_installs = sum(as.numeric(as.character(Installs))),
                          mean_installs = mean(as.numeric(as.character(Installs))),
                          apps_number =   length(as.character(Installs)))
p1 <- ggplot(installs_by_size,aes(x = Size.level, y = mean_installs, group = 1)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  ylab("Mean of installs") +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) 


p2 <- ggplot(installs_by_size,aes(x = Size.level, y = sum_installs, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_blank(), axis.title.x = element_blank())+
  ylab("Sum of installs")

p3 <- ggplot(installs_by_size,aes(x = Size.level, y = apps_number, group = 1)) + 
  geom_point(color = 'green') +
  geom_line(color = 'green')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of apps")

multiplot(p1,p2,p3)
dev.print(pdf, paste(save_plot_path,"InstallsSize.pdf"),height = 15)
dev.off()

################################################################################################
###  Size & Category
################################################################################################
ggplot(googleplaystore,aes(x = Size)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") +
  facet_wrap(~Category) +
  labs(title  = "Density of Size in Mo by Categories") +
  xlab("Size in Mo") +
  ylab("Density")

ggsave(paste(save_plot_path,"SizeCategory.pdf"), device = "pdf", height = 9, width = 10)

################################################################################################
# Installs by Category & Content.Rating
################################################################################################
installs_by_category__and_content <- ddply(googleplaystore, .(Category,Content.Rating), summarize, 
                                          sum_installs = sum(as.numeric(as.character(Installs))),
                                          mean_installs = mean(as.numeric(as.character(Installs))),
                                          apps_number =   length(as.character(Installs)))
ggplot(installs_by_category__and_content,aes(x = Category, y = mean_installs, group = 1)) + 
  geom_point(color = 'blue') +
  geom_line(color = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Content.Rating)+
  ylab("Mean of installs")
ggsave(paste(save_plot_path,"MeanInstallsCategoryContent.pdf"), device = "pdf", width = 15)
  
  
ggplot(installs_by_category__and_content,aes(x = Category, y = sum_installs, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Content.Rating)+
  ylab("Sum of installs")
ggsave(paste(save_plot_path,"SumInstallsCategoryContent.pdf"), device = "pdf", width = 15)


ggplot(installs_by_category__and_content,aes(x = Category, y = apps_number, group = 1)) + 
  geom_point(color = 'green') +
  geom_line(color = 'green')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  facet_wrap(~Content.Rating)+
  ylab("Number of apps")
ggsave(paste(save_plot_path,"NumberAppsCategoryContent.pdf"), device = "pdf", width = 15)

################################################################################################
## Installs by Category & Price.level
################################################################################################
installs_category_price <- as.data.frame(xtabs(as.numeric(as.character(Installs))~Category+Price.level,googleplaystore))

ggplot(installs_category_price, aes(Category, Price.level, fill = log(Freq))) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 13) +
  labs(title="Logarithm of number of installs by category and price level")+
  ylab("Price in $") +
  guides(fill=guide_legend(title="Log(Sum Of Installs)"))

ggsave(paste(save_plot_path,"InstallsCategoryPrice.pdf"), device = "pdf", width = 15)

## Show the apps with the highest prices
googleplaystore_ordered_price <- arrange(googleplaystore, -Price)[1:50,]
View(googleplaystore_ordered_price)

## WordCloud of these apps names
word_cloud(googleplaystore_ordered_price$App, "WordCloud of most expensive app names")

dev.print(pdf, paste(save_plot_path,"ExpensiveAppsCloudNames.pdf"),height = 12)
dev.off()

################################################################################################
## Installs by Size.level and Price.level
################################################################################################
ggplot(googleplaystore, aes(x = Installs , fill = Price.level)) +
  geom_bar() +
  facet_wrap(~Size.level) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Number of Installs by Size level filled by Price level")

ggsave(paste(save_plot_path,"InstallsSizePrice.pdf"), device = "pdf", width = 15)

################################################################################################
### Top installed app
################################################################################################
googleplaystore_ordered_installs <- arrange(googleplaystore, -as.numeric(as.character(Installs)))[1:50,]

ggplot(googleplaystore_ordered_installs, aes(Installs, fill = Size.level)) + 
  geom_bar(color='black') + 
  facet_grid(Content.Rating~Category) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(na.value="white") +
  labs(title="Top 50 apps")

ggsave(paste(save_plot_path,"Top50Installs.pdf"), device = "pdf", width = 15)

## WordCloud of these top apps names
word_cloud(googleplaystore_ordered_installs$App, "Top 50 installed app names")

dev.print(pdf, paste(save_plot_path,"Top50WordCloudNames.pdf"),height = 12)
dev.off()

################################################################################################
### Size + Installs
################################################################################################
ggplot(googleplaystore,aes(x = Installs, fill = Size.level)) + 
  geom_bar() +
  facet_wrap(~Category) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Installs by Categroy filled by Size levels")
# there is no correlation between size and installations, almost 50% of installed apps have a high size
ggsave(paste(save_plot_path,"InstallsCategorySize.pdf"), device = "pdf", width = 15)


################################################################################################
### Installs of Paid App of Family Category by Genres
################################################################################################
installs_by_category_Type_F <- googleplaystore[googleplaystore$Type == 'Paid' & googleplaystore$Category == 'FAMILY' & googleplaystore$Price > 0,]
installs_by_category_Type_F <- ddply(installs_by_category_Type_F, .(Price.level,Genres), summarize, 
                                     sum_installs = sum(as.numeric(as.character(Installs))),
                                     mean_installs = mean(as.numeric(as.character(Installs))),
                                     apps_number =   length(as.character(Installs)))

ggplot(installs_by_category_Type_F,aes(x = Genres, y = sum_installs, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~Price.level) +
  labs("Number of installs of paid apps of Family Category by Genres and Price Level")

ggsave(paste(save_plot_path,"InstallsPaidFamilyGenre.pdf"), device = "pdf", width = 15)

################################################################################################
### Installs of Paid App of Game Category by Genres
################################################################################################
installs_by_category_Type_G <- googleplaystore[googleplaystore$Type == 'Paid' & googleplaystore$Category == 'GAME' & googleplaystore$Price > 0,]
installs_by_category_Type_G <- ddply(installs_by_category_Type_G, .(Price.level,Genres), summarize, 
                                      sum_installs = sum(as.numeric(as.character(Installs))),
                                      mean_installs = mean(as.numeric(as.character(Installs))),
                                      apps_number =   length(as.character(Installs)))

ggplot(installs_by_category_Type_G,aes(x = Genres, y = sum_installs, group = 1)) + 
  geom_point(color = 'red') +
  geom_line(color = 'red')+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  facet_wrap(~Price.level) +
  labs("Number of installs of paid apps of Game Category by Genres and Price Level")

ggsave(paste(save_plot_path,"InstallsPaidGameGenre.pdf"), device = "pdf", width = 15)

