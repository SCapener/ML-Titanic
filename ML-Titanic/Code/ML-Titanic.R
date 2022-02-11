
# ---
# Title: Survival Prediction using R - tutorial using Titanic data
# Aim: Build a predictive model that answers the question: "what sorts of people
# ...were more likely to survive?" using passenger data (ie name, age, gender etc)
# Link to tutorial: https://rstudio-pubs-static.s3.amazonaws.com/151051_5b082e3b8fbd4faaa34cbb352bafb815.html
# ---

#####
# Import Data + inspect it

train <- read.csv('Documents/Documents/r-projects/titanic/train.csv',stringsAsFactors = FALSE)
test <- read.csv('Documents/Documents/r-projects/titanic/test.csv',stringsAsFactors = FALSE)

gender_sub <- read_csv("Documents/Documents/r-projects/titanic/gender_submission.csv")

head(train)
head(test)

head(gender_sub)

PassengerId <- test$PassengerId

#####
# Data Exploration

View(train)
# We can see a lot of missing values in the age column... let's confirm this

sum(is.na(train))
colSums(is.na(train))
# 177 missing values in Age column

# Detailed view of variables structure and composition
str(train)


# How many survived overall?
library(RColorBrewer)
barplot(prop.table(table(train$Survived)), col = brewer.pal(3, "Set1"))
# Just around 39% survived and 61% died

# How many survived - by sex?
barplot(prop.table(table(train$Survived, train$Sex)), col = brewer.pal(3, "Set1"))
# There were nearly 2x as many m en than women
# Around 10% of men survived, whilst only around 10% of women died

library(ggplot2)
qplot(factor(Pclass), data=train, geom="bar", fill=factor(Survived))
# 55% in 3rd class, 20% second class, 23% in 1st class
# There were around 490 in 3rd class and around 120 survived - 24%
# In second class there were around 180 and around half survived - 50%
# In 1st class there were around 210 and only around 70 died - 66% 

library(haven)
# train$Survived2 <- as_factor(train$Survived)
# # Converted the survived variable into a factor and added labels so I could understand the mosaic plot
# train$Survived2 <- factor(train$Survived2, levels = c(0,1), labels = c("Died", "Survived"))


table(train$Pclass, train$Sex, train$Survived)
mosaicplot(table(train$Pclass, train$Sex,train$Survived), sort = c(3,1,2), color = T)
# In class 2 men had the lowest number of survivals, whilst women had the number and the percentage going to first class
# The most men survived in class 3, but as a percentage the highest was in first class


# Survival by age - boxplot
install.packages("gridExtra")
install.packages("Hmisc")
library(gridExtra)
library(Hmisc)

cutSurviving <- cut2(train$Survived, g = 2)
cutSurviving
qplot(cutSurviving, Age, data = train, fill=cutSurviving,
      geom = c("boxplot"))
# Age of survivors is slightly younger, but this is not a lot to work with

qplot(Pclass,Age,colour=Survived,data=train, na.rm=TRUE)+geom_smooth(method='lm',formula=y~x)
qplot(factor(Pclass), Age, data = train, geom = "boxplot")

# Age  matters allot for surviving when is correlated with the Pclass variable. 
# Younger people had a better chance of surviving 
# But if we follow the smoothed line we can see that is inverse proportional with the number from Pclass.

qplot(cutSurviving,Fare, data=train,fill=cutSurviving,
      geom=c("boxplot"))

# This makes sense, because the most expensive tickets are in first class --> had the highest survival rate

qplot(Fare, Age, data=train, colour=factor(Pclass), na.rm = TRUE)



#####
# Data Mining - manipulate remaining variables to be able to use them for prediction model 

# Can use ticket variable to reflect afferent class
# The code below finds the right class of the person based on the first numbers or letters from the ticket using grep function.

install.packages("data.table")
library(data.table)
dt1<- as.data.table(train)

# grep() function is used to search for matches of a pattern within each element of the given string
dt1$Ticket[grep('^1|^57|^69|^P', dt1$Ticket)] <- 1
dt1$Ticket[grep('^2|^SW|^SO|^SC|^S.W|^S.P|^S.O|^S.C|^F', dt1$Ticket)] <- 2
dt1$Ticket[grep('^3|^C|^26|^4|^54|^65|^7|^8|^9|^A|LINE|SOTON|STON|^W|^160|^14', dt1$Ticket)] <- 3

# Handle missing variables in age category by grouping people based on the age category
# Method - take honorific term or prefix which appears in name description and replace with something more standard

head(dt1$Name)

dt1$Name[grep('Master.', dt1$Name, fixed=TRUE)] = 'Master'#I need to add fixed =True, else the Mrs it will be overwrite with Mr
dt1$Name[grep('Mr.', dt1$Name, fixed=TRUE)] = 'Mr'
dt1$Name[grep('Miss.|Mlle', dt1$Name)] = 'Miss'
dt1$Name[grep('Mrs.|Mme.|Ms.|Lady.|Countess.', dt1$Name)] = 'Mrs'
dt1$Name[grep("Dr.|Col.|Rev.|Capt.|Major.|Sir.|Don.|Jonkheer", dt1$Name)] = 'Sir'

head(dt1$Name)

#create a data table with the age average classified by names
dt_0<-dt1[, mean(Age, na.rm = TRUE), by = Name]

head(dt_0)

#create a new data table formed just from age missing values and the afferent names
dt0 <- dt1[is.na(Age),.(Age, Name)]
head(dt0)

#from previous constructed make a single data table
dt <- merge(dt0, dt_0, by = 'Name')
head(dt)

#make the necessary update on mising values and remove the V1 column after
dt <- dt[, Age := round(V1, 2)]
dt <- dt[, V1 := NULL]
head(dt)

unique(dt)

# Replace missing values in initial data - do so by setting key as Name, to avoid replacing in wrong order

setkey(dt1, Name)
dt1$Age[is.na(dt1$Age)] <- dt[,Age]
head(dt1$Age, 50)

# Density plot of age groups related to name
qplot(Age,colour=Name,data=dt1,geom="density")
# Names with Master and Miss were mostly younger than 25 yrs --> bigger chance of surviving

# How is Name variable represented in class?
table(dt1$Pclass, dt1$Name)
table(dt1$Pclass, dt1$Name,dt1$Survived)

ggplot(dt1, aes(Name, Pclass)) + geom_bar(stat = 'identity') + facet_grid(Survived~Pclass) +ylim(0, 300)


# Checking if any variables are correlated or have a high variability - avoid overfitting

install.packages("caret")
library(caret)

findCorrelation(cor(train[,-c(1, 4, 5, 9, 11, 12)]), cutoff = .7, names = T)
# No correlation

cor(train$Pclass, train$Fare)

table(train$Sex)
var(train$Pclass)


# Checking for variables with near zero variance
nzv <- nearZeroVar(dt1, saveMetrics=TRUE)
if (any(nzv$nzv)) nzv else message("No variables with near zero variance")

#####
# Feature Engineering and additional exploration

#this reflects the idea where the group names Master, Miss and Mrs had a higher/medium/acceptable chance
# of survival when correlated with the first class/second/third. 
# But because this category name groups implies a wide age variation, I restrict them as above.

dt1 <- dt1[, New_group := 0L]
for (i in 1:nrow(dt1)) {
  if(((dt1$Sex[i] == 'female' | dt1$Age[i] < 18)   &  dt1$Pclass[i] == 2) | (dt1$Pclass[i] == 1)){
    dt1$New_group[i] = 'High'
  } else if((dt1$Sex[i] == 'female' | dt1$Age[i] < 18) &  dt1$Pclass[i] == 3){
    dt1$New_group[i] = 'Acceptable'
  }     else{
    dt1$New_group[i] = 'Low'
  }
}

dt1  


# Perform similar transformations on the test data

test <- test[, -c(1, 9:11)] 
dt2<- as.data.table(test)
colSums(is.na(test))

dt2$Ticket[grep('^1|^57|^69|^68|^P', dt2$Ticket)] <- 1
dt2$Ticket[grep('^2|^SW|^SO|^SC|^S.W|^S.P|^S.O|^S.C|^F', dt2$Ticket)] <- 2
dt2$Ticket[grep('^3|^C|^26|^4|^54|^65|^7|^8|^9|^A|LINE|SOTON|STON|^W|^160|^14|^LP', dt2$Ticket)] <- 3

dt2

dt2$Name[grep('Master.', dt2$Name, fixed=TRUE)] = 'Master'
dt2$Name[grep('Mr.', dt2$Name, fixed=TRUE)] = 'Mr'
dt2$Name[grep('Miss.|Mlle', dt2$Name)] = 'Miss'
dt2$Name[grep('Mrs.|Mme.|Ms.|Lady.|Countess.', dt2$Name)] = 'Mrs'
dt2$Name[grep("Dr.|Col.|Rev.|Capt.|Major.|Sir.|Don.|Jonkheer", dt2$Name)] = 'Sir' 

dt_00<-dt2[, mean(Age, na.rm = TRUE), by = Name]
dt00 <- dt2[is.na(Age),.(Age, Name)]
dtt <- merge(dt00, dt_00, by = 'Name')
dtt <- dtt[, Age := round(V1, 2)]
dtt <- dtt[, V1 := NULL]
unique(dtt)

setkey(dt2, Name)
dt2$Age[is.na(dt2$Age)] <- dtt[,Age]              
dt2 <- dt2[, New_group := 0L]
for (i in 1:nrow(dt2)) {
  if(((dt2$Sex[i] == 'female' | dt2$Age[i] < 18)   &  dt2$Pclass[i] == 2) | (dt2$Pclass[i] == 1)){
    dt2$New_group[i] = 'High'
  } else if((dt2$Sex[i] == 'female' | dt2$Age[i] < 18) &  dt2$Pclass[i] == 3){
    dt2$New_group[i] = 'Acceptable'
  }     else{
    dt2$New_group[i] = 'Low'
  }
}


#####
# Machine Learning 

# LOGISTIC REGRESSION 

dt1[, c("PassengerId","Fare", 'Cabin', 'Embarked') := NULL]
dt1

glm<-glm(Survived ~ Pclass + I(Name == 'Master') + Sex + New_group  + Age +  Ticket+ SibSp+Parch + SibSp * Pclass,  data=dt1, family = binomial(link = 'logit'))
glm

# Used to predict the probability that the passenger survived or not
glm_p <- predict(glm, dt2, type = 'response')
glm_p

#preparing for submission
Survivor <- vector()
for(i in 1:length(glm_p)){
  if(glm_p[i] > .9){
    Survivor[i] = 1
  } else {
    Survivor[i] = 0
  }
}
Survivor

titanic_off <- cbind(PassengerId,Survivor)
titanic_off
colnames(titanic_off) <- c('PassengerId', "Survived")
colnames(titanic_off)

write.csv(titanic_off, file = "titanic.csv", row.names = FALSE)
