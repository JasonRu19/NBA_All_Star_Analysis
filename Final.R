library(tidyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(caret)
library(e1071)
library(class)
library(randomForest)
library(rpart)
library(rpart.plot)

# Loading Player Stats from Season 2011-2012 to 2020-2021
S21 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2020-2021 NBA Player Stats Season Avg.csv")
S20 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2019-2020 NBA Player Stats Season Avg.csv")
S19 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2018-2019 NBA Player Stats Season Avg.csv")
S18 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2017-2018 NBA Player Stats Season Avg.csv")
S17 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2016-2017 NBA Player Stats Season Avg.csv")
S16 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2015-2016 NBA Player Stats Season Avg.csv")
S15 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2014-2015 NBA Player Stats Season Avg.csv")
S14 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2013-2014 NBA Player Stats Season Avg.csv")
S13 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2012-2013 NBA Player Stats Season Avg.csv")
S12 <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2011-2012 NBA Player Stats Season Avg.csv")

# Adding Season Columns
S21$Season <- "2020-2021"
S20$Season <- "2019-2020"
S19$Season <- "2018-2019"
S18$Season <- "2017-2018"
S17$Season <- "2016-2017"
S16$Season <- "2015-2016"
S15$Season <- "2014-2015"
S14$Season <- "2013-2014"
S13$Season <- "2012-2013"
S12$Season <- "2011-2012"


# Combing them into one table
PlayerStats <- rbind(S21,S20,S19,S18,S17,S16,S15,S14,S13,S12)

# Removing any observation with the Value of TOT for team as this represents total games played by a player on multiple teams. This will be summed later
PlayerStats <- PlayerStats[PlayerStats$Tm != "TOT", ]

# Viewing the table details and dimensions
dim(PlayerStats)  # 5020 rows 31 columns
str(PlayerStats)

# Loading All_Star Game Stats from Season 2011-2012 to 2020-2021

Star21 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2020-2021 NBA All Star Player Stats.xlsx")
Star20 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2019-2020 NBA All Star Player Stats.xlsx")
Star19 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2018-2019 NBA All Star Player Stats.xlsx")
Star18 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2017-2018 NBA All Star Player Stats.xlsx")
Star17 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2016-2017 NBA All Star Player Stats.xlsx")
Star16 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2015-2016 NBA All Star Player Stats.xlsx")
Star15 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2014-2015 NBA All Star Player Stats.xlsx")
Star14 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2013-2014 NBA All Star Player Stats.xlsx")
Star13 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2012-2013 NBA All Star Player Stats.xlsx")
Star12 <- readxl::read_xlsx("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2011-2012 NBA All Star Player Stats.xlsx")

# Adding Season Columns
Star21$Season <- "2020-2021"
Star20$Season <- "2019-2020"
Star19$Season <- "2018-2019"
Star18$Season <- "2017-2018"
Star17$Season <- "2016-2017"
Star16$Season <- "2015-2016"
Star15$Season <- "2014-2015"
Star14$Season <- "2013-2014"
Star13$Season <- "2012-2013"
Star12$Season <- "2011-2012"


# Combing them into one table
AllStar <- rbind(Star21,Star20,Star19,Star18,Star17,Star16,Star15,Star14,Star13,Star12)

# Viewing the table details and dimensions
dim(AllStar) # 220 rows, 24 Columns
str(AllStar)

# Loading Salary Contract table
Contracts <- read.csv("C:/Users/moonw/Documents/UTSA MSDA Graduate Program/3_Year 2/Fall 2021/DA 6813/Final Project/Data/2021-2022 NBA Player Contract.csv")

# Viewing the table details and dimensions
str(Contracts)
dim(Contracts) # 475 rows 11 Columns

# View the tables throughout
#View(PlayerStats)
#View(AllStar)
#View(Contracts)

# Separating Player Col into First_Name, Last_Name, and Twitter Handle in  PlayerStats and Contracts
PlayerStats <- separate(data = PlayerStats, col = Player, into = c("Player", "Twitter_Handle"), sep = '\\\\')
Contracts <- separate(data = Contracts, col = Player, into = c("Player", "Twitter_Handle"), sep = '\\\\')
 


# Dropping RK and Twitter_Handle from PlayerStats and Contracts
# Dropping Signed.Using from Contracts
PlayerStats <- subset(PlayerStats, select = -c(Rk,Twitter_Handle))
Contracts <- subset(Contracts, select = -c(Rk,Twitter_Handle, Signed.Using))


#Summarizing Season and Player instances like Lamarcus Aldridge who played on multiple teams. Also removing TM variable, doesn't matter.
PlayerStats <- group_by(PlayerStats,Season,Player) %>%  summarise(G = sum(G), 
                                                                  GS = sum(GS),
                                                                  MP = mean(MP,2),
                                                                  FG = mean(FG,2),
                                                                  FGA = mean(FGA,2),
                                                                  FG. = mean(FG.,2),
                                                                  X3P = mean(X3P,2),
                                                                  X3PA = mean(X3PA,2),
                                                                  X3P. = mean(X3P.,2),
                                                                  X2P = mean(X2P,2),
                                                                  X2PA = mean(X2PA,2),
                                                                  X2P. = mean(X2P.,2),
                                                                  eFG. = mean(eFG.,2),
                                                                  FT = mean(FT,2),
                                                                  FTA = mean(FTA,2),
                                                                  FT. = mean(FT.,2),
                                                                  ORB = mean(ORB,2),
                                                                  DRB = mean(DRB,2),
                                                                  TRB = mean(TRB,2),
                                                                  AST = mean(AST,2),
                                                                  STL = mean(STL,2),
                                                                  BLK = mean(BLK,2),
                                                                  TOV = mean(TOV,2),
                                                                  PF = mean(PF,2),
                                                                  PTS = mean(PTS,2)
)



# Dropping # and Team from AllStar
AllStar <- subset(AllStar, select = -c(1, Team))

# Creating new DF of just Allstar player and which season they were allstars

AllStarPlayers <- subset(AllStar, select = c(Player, Season))

#View(AllStarPlayers)

# Adding a Yes column to AllStarPlayers to indicate an Allstar Season when this is merged with Player Stats
AllStarPlayers$"AllStarSeason" <- "Y"

# Joing Allstarplayers to Playerstas to indicate which players had all star seasons
Final <- left_join(PlayerStats,AllStarPlayers, by = c("Player" = "Player", "Season" = "Season"))
View(Final)

# Replacing all Null values with 0, first need to identify columns that have null values
# Replacing null values for AllStarSeason to N
colSums(is.na(Final))
Final$FG.<-Final$FG. %>% replace(is.na(.), 0)
Final$X3P.<-Final$X3P. %>% replace(is.na(.), 0)
Final$X2P.<-Final$X2P. %>% replace(is.na(.), 0)
Final$eFG.<-Final$eFG. %>% replace(is.na(.), 0)
Final$FT.<-Final$FT. %>% replace(is.na(.), 0)
Final$AllStarSeason<-Final$AllStarSeason %>% replace(is.na(.), "N")

#Confirm there no longer any Null values in the dataframe
sum(is.na(Final))

# Changing the order of season to Desc
Final <- Final %>% arrange(desc(Season))
Final$AllStarSeason <- as.factor(Final$AllStarSeason)
Final$G <- as.numeric(Final$G)
Final$GS <- as.numeric(Final$GS)
Final <- as.data.frame(Final)

#DF w/o player name or season
Final2 <- subset(Final, select = -c(Player,Season, G, GS, MP))


#Description of Final Df
dim(Final2)
str(Final2)
summary(Final2)


# EDA
cor<- cor(Final2 [,-23])
corrplot(round(cor,2), type = "lower", method = "circle") # Correllelogram

par(mfrow=c(2,4))

for(i in 1:25) {
  hist(Final2[,i], main=names(Final2)[i])
}

for(i in 1:23
    ) {
  boxplot(Final2[,i], main=names(Final2)[i])
}

pairs(Final2[1:6], col=Final2$AllStarSeason)
pairs(Final2[7:12], col=Final2$AllStarSeason)
pairs(Final2[13:18], col=Final2$AllStarSeason)
pairs(Final2[19:22], col=Final2$AllStarSeason)

dev.off()

# Split data into training and testing
set.seed(1)
row.number = createDataPartition(Final2$AllStarSeason, p=0.7, list=FALSE)
Finaltrain = Final2[row.number,]
finaltest = Final2[-row.number,]

# Logistic Regression w/ train/test split
m1 <- glm(AllStarSeason~.,data = Finaltrain, family = "binomial")
summary(m1) # looking for variables less than.05

round(exp(m1$coefficients),3)

# observe FTA, AST, STL, BLK

glm.pred <- predict(m1,finaltest,type = "response")
glm.prob <- ifelse(glm.pred > 0.5, "Y", "N")
table(glm.prob,finaltest$AllStarSeason)

(4783+121)/(4783+121+83+33) # Accuracy rate of .9721

confusionMatrix(as.factor(finaltest$AllStarSeason), as.factor(glm.prob))


#Logistic Regression w/o train/test split
m2 <- glm(AllStarSeason~.,data = Final2, family = "binomial")
summary(m2) # looking for variables less than.05


glm.pred2 <- predict(m2,type = "response")
glm.prob2 <- ifelse(glm.pred2 > 0.5, "Y", "N")
table(glm.prob2,Final2$AllStarSeason)

(4783+121)/(4783+121+83+33) # Accuracy rate of .9769

confusionMatrix(as.factor(Final2$AllStarSeason), as.factor(glm.prob2))



#KNN

knn.fit<-train(AllStarSeason~.,data=Finaltrain,method='knn',trControl = trainControl(method = "cv"), tuneLength=20)

##Calculate Predictions
pred.knn<-predict(knn.fit,finaltest)
##Estimate Accuracy
confusionMatrix(pred.knn,finaltest$AllStarSeason) #97.54


## Randomforest
rf1 <- randomForest(AllStarSeason ~ ., 
                    data = Finaltrain)

rf1

varImp(rf1)
varImpPlot(rf1)


pred = predict(rf1, finaltest)
confusionMatrix(finaltest$AllStarSeason, pred) #.9781

#Decision Tree
fit <- rpart(AllStarSeason~., data = Finaltrain, method = 'class')
pred.DT<-predict(fit,finaltest, type = "class")
rpart.plot(fit, digits = 4)
confusionMatrix(pred.DT,finaltest$AllStarSeason)
