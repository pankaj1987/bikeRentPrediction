library(shiny)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library("neuralnet")
library(lubridate)
library(RColorBrewer)
library(caret)
library(e1071)
library(shinyWidgets)
# Define UI for data upload app ----
ui <- fluidPage(

  # App title ----
  titlePanel(""),
 setBackgroundImage(src = 'topimage'),

  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Select a file ----
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),

     img(src='sideimage.jpg', align = "bottom",height = 140, width = 100),
  
      # Horizontal line ----
      tags$hr()

    ),

    # Main panel for displaying outputs ----
    mainPanel(
	 img(src="topimage.jpg", height = 100, width = 550),
	  tags$head(
        tags$style(HTML("body{ 
                background-image: url(backgroundImage.jpg);
    }"))),
      # Output: Data file ----
  tabsetPanel(type = "tabs",
                
                  tabPanel("Random Forest", verbatimTextOutput("sum1"),verbatimTextOutput("sum2"),verbatimTextOutput("sum3"),verbatimTextOutput("sum4"),verbatimTextOutput("sum5"),verbatimTextOutput("sum6"),plotOutput("plot"),plotOutput("plot1"),plotOutput("plot2")),
				  tabPanel("Neural Network", verbatimTextOutput("sag"),verbatimTextOutput("sag1"),verbatimTextOutput("sag2"),verbatimTextOutput("sag3"),verbatimTextOutput("sag4"),verbatimTextOutput("sag5"),plotOutput("neuralplot")),
				  tabPanel("Output Details", verbatimTextOutput("tab1"),verbatimTextOutput("tab2"),verbatimTextOutput("tab3"),verbatimTextOutput("tab4"),verbatimTextOutput("tab5"),verbatimTextOutput("tab6"),verbatimTextOutput("tab7"),verbatimTextOutput("tab8"),verbatimTextOutput("tab9"),verbatimTextOutput("tab10"))				
				 
                        ),
 	tableOutput('txtout'),
    tableOutput("contents")

    )

  )
)

# Define server logic to read selected file ----
server <- function(input, output) {

  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    req(input$file1)
train <-read.csv(input$file1$datapath)
setwd("F:/project report/Bike Rental") #give the location of test data set
test=read.csv("testnew.csv")
# reading the data files
#train=read.csv("train_bike.csv")
#test=read.csv("test_bike.csv")
str(train)

# introducing variables in test to combine train and test
# can also be done by removing the same variables from training data
test$registered=0
test$casual=0
test$count=0
data=rbind(train,test)

# getting some information about the combined data
str(data)
summary(data)

# factoring some variables from numeric
data$season=as.factor(data$season)
data$weather=as.factor(data$weather)
data$holiday=as.factor(data$holiday)
data$workingday=as.factor(data$workingday)

# extracting hour from the datetime variable
data$hour=substr(data$datetime,12,13)
data$hour=as.factor(data$hour)

# dividing again into train and test
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# creating some boxplots on the count of rentals
boxplot(train$count~train$hour,xlab="hour", ylab="count of users")
boxplot(train$casual~train$hour,xlab="hour", ylab="casual users")
boxplot(train$registered~train$hour,xlab="hour", ylab="registered users 1")

# extracting days of week from datetime
date=substr(data$datetime,1,10)
days<-weekdays(as.Date(date))
data$day=days

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# creating boxplots for rentals with different variables to see the variation
boxplot(train$registered~train$day,xlab="day", ylab="registered users 2")
boxplot(train$casual~train$day,xlab="day", ylab="casual users")

boxplot(train$registered~train$weather,xlab="weather", ylab="registered users 3")
boxplot(train$casual~train$weather,xlab="weather", ylab="casual users")

boxplot(train$registered~train$temp,xlab="temp", ylab="registered users 4")

 output$plot <- renderPlot({
 boxplot(train$casual~train$temp,xlab="temp", ylab="casual users")
  })
# extracting year from data
data$year=substr(data$datetime,1,4)
data$year=as.factor(data$year)

# ignore the division of data again and again, this could have been done together also
train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

# again some boxplots with different variables
# these boxplots give important information about the dependent variable with respect to the independent variables
boxplot(train$registered~train$year,xlab="year", ylab="registered users 5")
boxplot(train$casual~train$year,xlab="year", ylab="casual users")

boxplot(train$registered~train$windspeed,xlab="year", ylab="registered users 6")
boxplot(train$casual~train$windspeed,xlab="year", ylab="casual users")

boxplot(train$registered~train$humidity,xlab="humidity", ylab="registered users 7")
boxplot(train$casual~train$humidity,xlab="humidity", ylab="casual users 8")

data$hour=as.integer(data$hour)

# created this variable to divide a day into parts, but did not finally use it
data$day_part=0

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

data=rbind(train,test)

#using decision trees for binning some variables, this was a really important step in feature engineering
d=rpart(registered~hour,data=train)
fancyRpartPlot(d)

d=rpart(casual~hour,data=train)
fancyRpartPlot(d)

data=rbind(train,test)
data$dp_reg=0
data$dp_reg[data$hour<8]=1
data$dp_reg[data$hour>=22]=2
data$dp_reg[data$hour>9 & data$hour<18]=3
data$dp_reg[data$hour==8]=4
data$dp_reg[data$hour==9]=5
data$dp_reg[data$hour==20 | data$hour==21]=6
data$dp_reg[data$hour==19 | data$hour==18]=7

data$dp_cas=0
data$dp_cas[data$hour<=8]=1
data$dp_cas[data$hour==9]=2
data$dp_cas[data$hour>=10 & data$hour<=19]=3
data$dp_cas[data$hour>19]=4

f=rpart(registered~temp,data=train)
fancyRpartPlot(f)

f=rpart(casual~temp,data=train)
fancyRpartPlot(f)

data$temp_reg=0
data$temp_reg[data$temp<13]=1
data$temp_reg[data$temp>=13 & data$temp<23]=2
data$temp_reg[data$temp>=23 & data$temp<30]=3
data$temp_reg[data$temp>=30]=4

data$temp_cas=0
data$temp_cas[data$temp<15]=1
data$temp_cas[data$temp>=15 & data$temp<23]=2
data$temp_cas[data$temp>=23 & data$temp<30]=3
data$temp_cas[data$temp>=30]=4

data$year_part[data$year=='2011']=1
data$year_part[data$year=='2011' & data$month>3]=2
data$year_part[data$year=='2011' & data$month>6]=3
data$year_part[data$year=='2011' & data$month>9]=4
data$year_part[data$year=='2012']=5
data$year_part[data$year=='2012' & data$month>3]=6
data$year_part[data$year=='2012' & data$month>6]=7
data$year_part[data$year=='2012' & data$month>9]=8
table(data$year_part)

# creating another variable day_type which may affect our accuracy as weekends and weekdays are important in deciding rentals
data$day_type=0

data$day_type[data$holiday==0 & data$workingday==0]="weekend"
data$day_type[data$holiday==1]="holiday"
data$day_type[data$holiday==0 & data$workingday==1]="working day"

train=data[as.integer(substr(data$datetime,9,10))<20,]
test=data[as.integer(substr(data$datetime,9,10))>19,]

plot(train$temp,train$count)
data=rbind(train,test)
data$month=substr(data$datetime,6,7)
data$month=as.integer(data$month)

# dividing total data depending on windspeed to impute/predict the missing values
table(data$windspeed==0)
k=data$windspeed==0
wind_0=subset(data,k)
wind_1=subset(data,!k)

# predicting missing values in windspeed using a random forest model
# this is a different approach to impute missing values rather than just using the mean or median or some other statistic for imputation

 




  
  
   
  
  
#### Neural Network Start


biketrain <-read.csv(input$file1$datapath,stringsAsFactors = FALSE) 
str(biketrain)
dim(biketrain)
hist(biketrain$count)
apply(biketrain, 2,range)
bike1 <- biketrain[, sapply(biketrain, is.numeric)]
maxValue <- apply(bike1, 2,max)
minValue <- apply(bike1, 2,min)
bike1 <- as.data.frame(scale(bike1,center=minValue,scale = maxValue-minValue))
allVars<-colnames(bike1)
predictorVars<-allVars[!allVars%in%"count"]
predictorVars<-paste(predictorVars,collapse="+")
form=as.formula(paste("count~",predictorVars,collapse = "+"))
nuralModel <- neuralnet(formula = form,hidden = c(4,2),linear.output = T,data=bike1)
plot(nuralModel)
#test
biketest <-read.csv("testnew.csv",stringsAsFactors = FALSE)
str(biketest)
dim(biketest)

apply(biketest, 2,range)
bike2 <- biketest[, sapply(biketest, is.numeric)]
maxValue1 <- apply(bike2, 2,max)
minValue1 <- apply(bike2, 2,min)
bike2 <- as.data.frame(scale(bike2,center=minValue1,scale = maxValue1-minValue1))
biketrain$datetime <- ymd_hms(biketrain$datetime)
biketrain$season <- factor(biketrain$season
                           ,levels = c(1,2,3,4)
                           ,labels = c("spring", "summer", "fall", "winter")
)

biketrain$workingday <- factor(biketrain$workingday
                               ,levels = c(0,1)
                               ,labels = c("nonwkday", "wkday")
)
biketrain$weather <- factor(biketrain$weather
                            ,levels = c(4,3,2,1)
                            ,labels = c("very bad", "bad", "good", "very good")
                            ,ordered = TRUE)



biketrain$hour <- hour(biketrain$datetime)

biketrain$month_nbr <- month(biketrain$datetime)

biketrain$month <- factor(months(biketrain$datetime)
                          ,levels = c("January"
                                      ,"February"
                                      ,"March"
                                      ,"April"
                                      ,"May"
                                      ,"June"
                                      ,"July"
                                      ,"August"
                                      ,"September"
                                      ,"October"
                                      ,"November"
                                      ,"December")
                          ,ordered = TRUE)



yearmask <- year(biketrain$datetime) == 2012

biketrain <- biketrain[yearmask,] 

bike3 <- biketrain[c("temp"
                     ,"hour"
                     ,"workingday"
                     ,"month_nbr"
                     ,"count")]

nuralModel <- lm(count ~ .
             ,data = bike3
)


# Accuracy start

trainingRowIndex <- sample(1:nrow(bike3), 0.8*nrow(bike3))  # row indices for training data
trainingData <- bike3[trainingRowIndex, ]  # model training data
testData  <- bike3[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(count ~ ., data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict 

actuals_preds <- data.frame(cbind(actuals=testData$count, predicteds=distPred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  #
head(actuals_preds)

trControl1 <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


set.seed(1234)
# Run the model
nn_default <- train(count ~ .,
                    data = trainingData,
                   method = "lm",
                    trControl = trControl1)
# Print the results
print(nn_default)

#Accuracy End

biketest$datetime <- ymd_hms(biketest$datetime)

biketest$season <- factor(biketest$season
                          ,levels = c(1,2,3,4)
                          ,labels = c("spring", "summer", "fall", "winter")
)

biketest$workingday <- factor(biketest$workingday
                              ,levels = c(0,1)
                              ,labels = c("nonwkday", "wkday")
)

biketest$weather <- factor(biketest$weather
                           ,levels = c(4,3,2,1)
                           ,labels = c("very bad", "bad", "good", "very good")
                           ,ordered = TRUE)



biketest$hour <- hour(biketest$datetime)

biketest$month_nbr <- month(biketest$datetime)

biketest$month <- factor(months(biketest$datetime)
                         ,levels = c("January"
                                     ,"February"
                                     ,"March"
                                     ,"April"
                                     ,"May"
                                     ,"June"
                                     ,"July"
                                     ,"August"
                                     ,"September"
                                     ,"October"
                                     ,"November"
                                     ,"December")
                         ,ordered = TRUE)


biket4 <- biketest[c("temp"
                     ,"hour"
                     ,"workingday"
                     ,"month_nbr")]


brental <- predict(nuralModel
                   ,newdata = biket4
)


brental[brental < 0] <- min(brental[brental > 0]) 

brental <- round(brental, 0)


bikesub <- data.frame(datetime = as.character(biketest$datetime)
                      ,count = as.integer(brental)
)

  output$sag1 <- renderPrint({
  head(actuals_preds)

  })
  


  output$sag4 <- renderPrint({
   print(nn_default$results$Rsquared)


  })
 output$neuralplot <- renderPlot({
plot(nuralModel)

  })
  
  
write.csv(bikesub
          ,file = "neuralresult.csv"
          ,row.names = FALSE
          ,quote = FALSE
)


set.seed(415)
 
fit2 <- randomForest(count ~ ., data=bike3,importance=TRUE, ntree=250)
 
pred2=predict(fit2,biket4)

pred2[pred2 < 0] <- min(pred2[pred2 > 0]) 

pred2 <- round(pred2, 0)


bikesub1 <- data.frame(datetime = as.character(biketest$datetime)
                      ,count = as.integer(pred2)
)



trControl <- trainControl(method = "cv",
                          number = 10,
                          search = "grid")


set.seed(1234)
# Run the model
rf_default <- train(count ~ .,
                    data = trainingData,
                   method = "rf",
                    trControl = trControl)
# Print the results
print(rf_default)
rfmodel <- randomForest(count ~ ., data=trainingData,importance=TRUE, ntree=250)  # build the model

rfPred <- predict(rfmodel, testData)  # predict 

actuals_rf_preds <- data.frame(cbind(actuals=testData$count, predicteds=rfPred))  # make actuals_predicteds dataframe.
rf_correlation_accuracy <- cor(actuals_rf_preds)  #
head(actuals_rf_preds)


rf_min_max_accuracy <- mean(apply(actuals_rf_preds, 1, min) / apply(actuals_rf_preds, 1, max))  
#mean absolute percentage deviation
rf_mape <- mean(abs((actuals_rf_preds$predicteds - actuals_rf_preds$actuals))/actuals_rf_preds$actuals) 

   output$sum1 <- renderPrint({
  "Output file is created"

  })
   output$sum2 <- renderPrint({
  paste(getwd())

  }) 
    output$sum4 <- renderPrint({
  head(actuals_rf_preds)

  })
  
   output$plot <- renderPlot({
plot(train$registered,train$hour)
  })
   output$plot1 <- renderPlot({
plot(train$registered,train$weather)
  })
   output$plot2 <- renderPlot({
plot(train$registered,train$humidity)
  })
  
  output$tab1 <- renderPrint({
  "Random Forest Model"

  })
 
 output$tab2 <- renderPrint({
  print(rf_default)

  })
  
   output$tab3 <- renderPrint({
  "Random Forest RMSE"

  })
  
  output$tab4 <- renderPrint({
  paste(rf_min_max_accuracy*100)

  })
  
    output$tab7 <- renderPrint({
  "Neural Network RMSE"

  })
  
   output$tab8 <- renderPrint({
   print(nn_default$results$RMSE)

  })
 
 as.data.frame("Done")
 
write.csv(bikesub1
          ,file = "randomresult.csv"
          ,row.names = FALSE
          ,quote = FALSE
)

### Neural Network End
  
  
  })
}

# Create Shiny app ----
shinyApp(ui, server)
