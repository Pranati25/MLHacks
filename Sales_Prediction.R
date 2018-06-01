setwd("C://Data Science Material/Analytics Vidhy Hackathns/Big Mart Sales - Analytics Vidhya")
getwd()

#read the test data and the train datas
test_data <- read.csv("Test.csv", na.strings=c(""))
train_data <- read.csv("Train.csv", na.strings = c(""))

merged_data <- merge.data.frame(test_data,train_data, all = TRUE)
write.csv(merged_data, file="Merged.csv")

#---------------------------------------------NA COUNT-------------------------------------------

#NA count of all 
table(is.na(merged_data))

#NA count Column Wise
colSums(is.na(merged_data))

summary(merged_data, maxsum=20)
str(merged_data)

#-----------------------------------------DATA CLEANING------------------------------------------------------
#Found two variables Item_Weight and Outlet_Size with missing values

#Imputing Missing Values
meanOfItem_Weight <- mean(merged_data$Item_Weight, na.rm=TRUE)
merged_data$Item_Weight[is.na(merged_data$Item_Weight)] <- meanOfItem_Weight

#Impute Outlet_Size with the mode of the Outlet_Size for the particular type of outlet.

#count outlet size for each outlet level
outlet_level <- as.data.frame( setNames(
  aggregate(
    merged_data$Outlet_Type, 
    by=list(Category=merged_data$Outlet_Type,
            Category=merged_data$Outlet_Size), 
    FUN= length),
  c("Outlet_Type", "Outlet_Size", "number")
))

merged_data[is.na(merged_data$Outlet_Size) & 
              merged_data$Outlet_Type=="Supermarket Type1", "Outlet_Size"] <- "Small"
merged_data[is.na(merged_data$Outlet_Size) & 
              merged_data$Outlet_Type=="Grocery Store", "Outlet_Size"] <- "Small"


#-----------------------------------------FEATURE ENGINEERING------------------------------------------------------

#STEP 1 :Combining Outlet_Type if there is no significant difference
levels(merged_data$Outlet_Type)
aggregate(merged_data$Item_Outlet_Sales, list(merged_data$Outlet_Type), mean, na.rm=TRUE)

#STEP2 :Modifying item visibility
length(which(merged_data$Item_Visibility == 0))
merged_data[which(merged_data$Item_Visibility == 0),"Item_Visibility"] <- NA
meanOfItem_Visibility <- mean(merged_data$Item_Visibility, na.rm = TRUE)
merged_data$Item_Visibility[is.na(merged_data$Item_Visibility)] <- meanOfItem_Visibility

aggregate(merged_data$Item_Visibility, list(merged_data$Outlet_Type, merged_data$Outlet_Size), mean)

#STEP3: Creating broad category of item
merged_data$Item_Type_Combined <- substr(merged_data$Item_Identifier, 1,2)
merged_data$Item_Type_Combined  <- factor(merged_data$Item_Type_Combined)
levels(merged_data$Item_Type_Combined) <- c('Drinks','Food','Non Consumable')
table(merged_data$Item_Type_Combined)

#STEP4: Determine the years of operation of a store
merged_data$Outlet_Years <- 2018 - merged_data$Outlet_Establishment_Year
merged_data$Outlet_Years <- as.factor(merged_data$Outlet_Years)

#STEP5: Modify categories of Item_Fat_Content

summary(merged_data$Item_Fat_Content)
merged_data[which(merged_data$Item_Fat_Content == "LF"), "Item_Fat_Content"] <- "Low Fat" 
merged_data[which(merged_data$Item_Fat_Content == "low fat"), "Item_Fat_Content"] <- "Low Fat" 
merged_data[which(merged_data$Item_Fat_Content == "reg"), "Item_Fat_Content"] <- "Regular"

merged_data$Item_Fat_Content <- droplevels(merged_data$Item_Fat_Content)
levels(merged_data$Item_Fat_Content) <- c("Low Fat","Regular","Non Edible")
merged_data[which(merged_data$Item_Type_Combined == "Non Consumable"), "Item_Fat_Content"] <- "Non Edible"
summary(merged_data$Item_Fat_Content)

#STEP 6 : Numerical and One Hot Coding of Categorical Variable
merged_data$Outlet <- merged_data$Outlet_Identifier

#STEP 7 : Taking Log of Response "Item_Outlet_Sales" to avoid homoscedasticity
merged_data$Log_Item_Sales <- log(merged_data$Item_Outlet_Sales)

summary(merged_data)
str(merged_data)

#-----------------------------------------MODEL CREATION------------------------------------------------------
Sales_train <- merged_data[which(merged_data$Item_Outlet_Sales != 0),]
Sales_test <- merged_data[is.na(merged_data$Item_Outlet_Sales),]

model_train <- lm(formula = Log_Item_Sales ~ Item_Fat_Content + Outlet  
                  + Item_Visibility  + Item_MRP +Item_Weight
                  , data = Sales_train)
RSS1 = sum(model_train$residuals^2)
RSS1

plot(model_train)
summary(model_train)


