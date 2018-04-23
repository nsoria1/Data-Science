# Load libraries
library(rpart) #Library for classification trees
library(rattle) #Better plotting for classification trees
library(randomForest) #Random Forest creation
library(Matrix) #To one hot variables
library(xgboost) #Load the XGBoost package

# Deinfe file path
file <- 'C:/Users/nsoria/Documents/Development/Data Science/Resolution Prediction/Until_January.csv'
#file <- '/home/nicolas/Documents/Development/Machine Learning/Resolution Prediction/Until_January.csv'

# Load dataset
tickets <- read.csv(file, header=TRUE, sep=';', dec=",", stringsAsFactors = FALSE)

# Removing all records that would add noise to the model
tickets <- tickets[!tickets$Status=="Cancelled",]
tickets <- tickets[!tickets$Status=="#�REF!",]
tickets <- tickets[!tickets$Status=="#N/A",]
tickets <- tickets[!tickets$GA.Team=="No",]
tickets <- tickets[!tickets$SLA.Result..4P.=="Not App",]
tickets <- tickets[!tickets$SLA.Result..4P.=="#�REF!",]
tickets <- tickets[!tickets$SLA.Result..4P.=="#N/A",]
tickets <- tickets[!tickets$Funct.Area=="CNC/L1",]
tickets <- tickets[!tickets$Funct.Area=="TECH LEAD",]
tickets <- tickets[!tickets$IRIS.Priority=="L",]

# Adding quarter information to the tickets
tickets$Quarter <- ifelse(tickets$Month.Submitted <= 4, "Quarter 1", 
                          ifelse(tickets$Month.Submitted <= 8, "Quarter 2", "Quarter 3"))
tickets$Date_Submitted <- as.Date(with(tickets, 
                          paste(tickets$Year.Submitted, tickets$Month.Submitted, tickets$Day.Submitted,sep="-")), "%Y-%m-%d")

# Drop features that are not required
tickets$Closed.in.the.month <- NULL
tickets$Day.Submitted <- NULL
tickets$Year.Submitted <- NULL
tickets$Month.Submitted <- NULL
tickets$Year.Month.Submitted <- NULL
tickets$Year.Closed <- NULL
tickets$Month.Closed <- NULL
tickets$Year.Month.Closed <- NULL
tickets$Due.Date.in.Month..4P. <- NULL
tickets$Due.Date.in.Years..4P. <- NULL
tickets$Year.Month.Due..4P. <- NULL
tickets$Closed..or.Last.Day.report.was.run.for.Open. <- NULL
tickets$Delta <- NULL
tickets$Day.of.week <- NULL
tickets$Networkdays.correction <- NULL
tickets$Period.YY.MM <- NULL
tickets$Closed.Within.SLA..4P. <- NULL
tickets$SOW.WO <- NULL
tickets$IRIS.Incident.Number <- NULL
tickets$TICKET.SUMMARY <- NULL
tickets$SLA.Defect.Category <- NULL
tickets$IRIS.Root.cause.category <- NULL
tickets$IRIS.Rootcause.sub.category <- NULL
tickets$SLA.Defect.Category <- NULL
tickets$PM..Yes. <- NULL
tickets$Problem.Type <- NULL
tickets$Group.per.work.note..only.Task. <- NULL
tickets$Big.Work.Order <- NULL
tickets$IRIS.Assignee <- NULL
tickets$IRIS.Requester.name <- NULL
tickets$Due.Date..4P. <- NULL
tickets$GA.Team <- NULL
tickets$Status <- NULL
tickets$Detail <- NULL
tickets$Belongs.to.this.month...4P. <- NULL
tickets$Backlog..4P. <- NULL
tickets$Aging.group..4P. <- NULL
tickets$X4.Priorities <- NULL
tickets$IRIS.Status <- NULL
tickets$IRIS.Submit.Date <- NULL
tickets$IRIS.Resolved.Date <- NULL
tickets$Work.Notes..CNC..DBA..SEC..SCH...TEC. <- NULL
tickets$IRIS.Submitted.By <- NULL
tickets$IRIS.Configuration.item <- NULL
tickets$IRIS.Country.Code <- NULL
tickets$IRIS.Resolution.code <- NULL
tickets$IRIS.CI.Application.ID <- NULL
tickets$IRIS.Resolution.Category <- NULL
tickets$Breached.Code <- NULL
tickets$Group.manual.input <- NULL
tickets$Detail.Group.by.CI <- NULL
tickets$Detail.Group.by.AG...CI <- NULL
tickets$IRIS.Region...Site <- NULL
tickets$IRIS.Assigned.Group <- NULL
tickets$Group.per.formula <- NULL
tickets$Service.Type <- NULL
tickets$Resolution.Time <- NULL
tickets$Source.File <- NULL
tickets$double <- NULL
tickets$Y.M.Submitted <- NULL
tickets$Y.M.Closed <- NULL
tickets$Days.per.Priority..4P. <- NULL
tickets$Closed..or.last.day.report.was.run.for.open. <- NULL
tickets$day.of.week <- NULL
tickets$IRIS.Resolution <- NULL
tickets$IRIS.Ticket.Summary <- NULL
tickets$Date_Submitted <- NULL

# Feature Engeneering over some fields
tickets$Funct.Area <- as.character(tickets$Funct.Area)
tickets$Environment <- as.character(tickets$Environment)
tickets$ServiceType <- as.character(tickets$ServiceType)
tickets$IRIS.Priority <- as.character(tickets$IRIS.Priority)

i1 <- with(tickets, grepl("^(EMEA|AME)", tickets$Funct.Area))
tickets$Funct.Area[i1] <- as.character(tickets$Environment[i1])

i1 <- with(tickets, grepl("^(SC)", tickets$Funct.Area))
tickets$Funct.Area[i1] <- as.character(tickets$ServiceType[i1])

i1 <- with(tickets, grepl("^(MON)", tickets$Funct.Area))
tickets$Funct.Area[i1] <- as.character("L1")

i1 <- with(tickets, grepl("^(Normal)", tickets$IRIS.Priority))
tickets$IRIS.Priority[i1] <- as.character("Priority 3")

i1 <- with(tickets, grepl("^(Low|LOW)", tickets$IRIS.Priority))
tickets$IRIS.Priority[i1] <- as.character("Priority 4")

i1 <- with(tickets, grepl("^(Medium)", tickets$IRIS.Priority))
tickets$IRIS.Priority[i1] <- as.character("Priority 3")

i1 <- with(tickets, grepl("^(High)", tickets$IRIS.Priority))
tickets$IRIS.Priority[i1] <- as.character("Priority 2")

i1 <- with(tickets, grepl("^(Critical)", tickets$IRIS.Priority))
tickets$IRIS.Priority[i1] <- as.character("Priority 1")

tickets$Funct.Area <- as.factor(tickets$Funct.Area)
tickets$Environment <- as.factor(tickets$Environment)
tickets$ServiceType <- as.factor(tickets$ServiceType)
tickets$IRIS.Priority <- as.factor(tickets$IRIS.Priority)
tickets <- tickets[!substr(tickets$Funct.Area, 0, 2) == "PM",]
tickets <- tickets[!substr(tickets$Funct.Area, 0, 7) == "PROJECT",]
tickets <- tickets[!substr(tickets$Funct.Area, 0, 2) == "NA",]
tickets <- tickets[!substr(tickets$Environment, 0, 4) == "BULK",]

# Separate Region into two colums
#tickets <- tickets %>% separate(Region, c("Field_Unformatted", "Country"))

tickets <- as.data.frame(lapply(tickets, function (x) if (is.factor(x)) factor(x) else x))

# Rename columns
names(tickets)[names(tickets) == 'Funct.Area'] <- 'Functional_Area'
names(tickets)[names(tickets) == 'Environment'] <- 'Operating_Company'
names(tickets)[names(tickets) == 'ServiceType'] <- 'Service_Type'
names(tickets)[names(tickets) == 'Ticket.Nature'] <- 'Ticket_Category'
names(tickets)[names(tickets) == 'SLA.Result..4P.'] <- 'Ticket_Resolution'
names(tickets)[names(tickets) == 'IRIS.Priority'] <- 'Priority'

# Test model created
spl <- sort(sample(nrow(tickets), nrow(tickets)*.7))
train <- tickets[spl,]
test <- tickets[-spl,]

# Set seed to reproduce it
set.seed(123)

################################################ Decision Tree Algorithm

# Create a decision tree model based on the train information.
model_rp <- rpart(train$Ticket_Resolution ~ ., data = train, method="class", 
               control = rpart.control(minsplit = 1, minbucket = 1, cp = 0.001))

# Plot it for visualization
fancyRpartPlot(model_rp)

# Make a prediction
prediction <- predict(model_rp, test, type = "class")

# Compare outcome with the test set of data
table(test$Ticket_Resolution, prediction)

################################################ Random Forest Algorithm

# Create a Random Forest model
model_rf <- randomForest(train$Ticket_Resolution ~., data = train, importance = TRUE, ntree = 10)

# Plot variable importance for visualization
varImpPlot(model_rf)

# Make a prediction
prediction <- predict(model_rf, test, type = "class")

# Compare outcome with the test set of data
table(test$Ticket_Resolution, prediction)

################################################ XGBoost Model Algorithm

# Create separate vectors of our outcome variable for both our train and test sets
# We'll use these to train and test our model later
train.label <- ifelse(train$Ticket_Resolution == "OK", 1, 0)
test.label <- ifelse(test$Ticket_Resolution == "OK", 1, 0)

# Create sparse matrixes and perform One-Hot Encoding to create dummy variables
dtrain <- sparse.model.matrix(Ticket_Resolution ~ .-1, data=train)
dtest <- sparse.model.matrix(Ticket_Resolution ~ .-1, data=test)

# Set our hyperparameters
param <- list(objective   = "binary:logistic",
              eval_metric = "error",
              max_depth   = 7,
              eta         = 0.1,
              gammma      = 1,
              colsample_bytree = 0.5,
              min_child_weight = 1)

# Pass in our hyperparameteres and train the model 
system.time(xgb <- xgboost(params  = param,
                           data    = dtrain,
                           label   = train.label, 
                           nrounds = 500,
                           print_every_n = 100,
                           verbose = 1))

# Create our prediction probabilities
pred <- predict(xgb, dtest)

# Set our cutoff to determine output values
pred.resp <- ifelse(pred >= 0.86, 1, 0)

# Get the trained model
model <- xgb.dump(xgb, with_stats=TRUE)

# Get the feature real names
names <- dimnames(dtrain)[[2]]

# Compute feature importance matrix
importance_matrix <- xgb.importance(names, model=xgb)[0:20] # View top 20 most important features

# Plot
xgb.plot.importance(importance_matrix)

# Save outcome
submit <- data.frame(Resultado_Original = test$Tickets_Resolution, Prediccion = prediction)

write.csv(submit, file = "C:/Users/nsoria/Downloads/decsiiontree.csv", row.names = FALSE)
#write.csv(submit, file = "/home/nicolas/Downloads/decisiontree.csv", row.names = FALSE)