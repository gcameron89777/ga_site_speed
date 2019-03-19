library(RGA)
library(tidyverse)
library(caret)


# authorize
# options("httr_oob_default" = TRUE) # stops RGA redirecting to localhost after authorizing
authorize(cache = F) # Access token will be stored in the '.ga-token.rds' file.
profile_id <- readline(prompt = "Enter Google Analytics Profile ID: ") # ga account with ecom. Get this number to the right of the "p" in URL while logged into GA, right at the end of the url

# last 120 days
## GA Site speed metrics are sampled, need a longer time frame to get a decent amount of data
start_date <- as.Date(Sys.Date() - 60)
end_date <- as.Date(Sys.Date() - 1)

# data
ga_site_speed <- get_ga(profileId = profile_id,
                  start.date = start_date,
                  end.date = end_date,
                  metrics = "ga:pageLoadTime, ga:speedMetricsSample, ga:transactions, ga:transactionRevenue",
                  dimensions = "ga:dimension1, ga:channelGrouping, ga:userType, ga:deviceCategory", # dimension1 is sessionID
                  filters = "ga:speedMetricsSample > 0", # only keep sessions with speed metric data
                  samplingLevel = "HIGHER_PRECISION",
                  fetch.by = "day", # minimise sampling
                  max.results = NULL
)

# wrangling
ga_data <- ga_site_speed %>% 
  mutate(Traffic_Type = ifelse(grepl("Referral|Organic|Direct", channelGrouping), "Unpaid Traffic", "Paid Traffic")) %>% 
  select(-channelGrouping) %>% 
  mutate(Avg_Load_Time = round((pageLoadTime / 1000) / speedMetricsSample, 2)) %>%   # convert load time from milliseconds to seconds
  filter(Avg_Load_Time <= 20) %>% # remove outliers
  mutate(Avg_Load_Bin = cut(Avg_Load_Time, c(0, 4, 6, 8, 10, 15, 20))) %>% 
  select(-c(speedMetricsSample, transactionRevenue, pageLoadTime)) %>% 
  select(transactions, everything()) %>% 
  mutate(Log_Avg_Load_Time = log(Avg_Load_Time))


# Exploratory Analysis
visual_df <- ga_data %>% 
  select(-c(dimension1, Avg_Load_Bin)) %>% 
  mutate(transactions = ifelse(transactions > 0, 1, 0)) %>% 
  mutate(transactions = transactions %>% make.names() %>% factor(levels = c("X1", "X0"))) %>% 
  mutate(desktop = factor(ifelse(deviceCategory == "desktop", 1, 0) %>% make.names())) %>% 
  select(-deviceCategory)

## pairs and corellations
GGally::ggpairs(visual_df, aes(alpha = 0.4),
                upper = list(continuous = "density", combo = "box", discrete = "facetbar", mapping = aes(color = transactions)),
                diag = list(continuous = "densityDiag", discrete = "barDiag", mapping = aes(color = transactions)),
                lower = list(continuous = "density", combo = "facethist", discrete = "blank", mapping = aes(color = transactions)))

## plot conversion rate by site speed bin
cr_overall <- ga_data %>% group_by(Avg_Load_Bin) %>% 
  summarise(Sessions = n(),
            Transactions = sum(transactions)) %>% 
  mutate(Transaction_Conversion_Rate = round(Transactions / Sessions, 3))


# transactions
ggplot(cr_overall, aes(x = Avg_Load_Bin, y = Transaction_Conversion_Rate, group = 1)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# segments
cr_segments <- ga_data %>% group_by(Avg_Load_Bin, userType, deviceCategory, Traffic_Type) %>% 
  summarise(Sessions = n(), 
            Transactions = sum(transactions)) %>% 
  mutate(Transaction_Conversion_Rate = round(Transactions / Sessions, 3))

## transactions analysis
ggplot(cr_segments, aes(x = Avg_Load_Bin, y = Transaction_Conversion_Rate, group = userType, color = userType)) +
  geom_line() +
  facet_grid(rows = vars(deviceCategory),
             cols = vars(Traffic_Type)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Statistical Modeling

## data prep
### remove uneeded vars
training_data <- ga_data %>% select(-c(dimension1, Avg_Load_Bin))

### dummys
dummy <- caret::dummyVars(~ ., data = training_data, fullRank = F, sep = ".")
training_data <- predict(dummy, ga_data) %>% as.data.frame()
clean_names <- names(training_data) %>% str_replace_all(" |`", "")
names(training_data) <- clean_names

# training data preparation
target <- training_data$transactions %>% make.names() %>% factor(levels = c("X0", "X1")) # glm fits towards the second factor level
training_data <- training_data %>% select(userTypeNewVisitor, deviceCategorydesktop, Traffic_TypePaidTraffic, Log_Avg_Load_Time, Avg_Load_Time)

## custom evaluation metric function
my_summary  <- function(data, lev = NULL, model = NULL){
  a1 <- defaultSummary(data, lev, model)
  b1 <- twoClassSummary(data, lev, model)
  c1 <- prSummary(data, lev, model)
  out <- c(a1, b1, c1)
  out}

## tuning & parameters
set.seed(123)
train_control <- trainControl(
  method = "cv",
  number = 5,
  sampling = "up", # very imbalanced data, otherwise see oddities like specificity value of 1
  savePredictions = TRUE,
  verboseIter = TRUE,
  classProbs = TRUE,
  summaryFunction = my_summary
)

linear_model = train(
  x = training_data,
  y = target,
  trControl = train_control,
  method = "glm", # logistic regression
  family = "binomial",
  metric = "AUC"
)

quad_model = train(
  x = training_data %>% mutate(Load_Sq = Avg_Load_Time^2),
  y = target,
  trControl = train_control,
  method = "glm", # logistic regression
  family = "binomial",
  metric = "AUC"
)

interaction_model = train(
  x = training_data %>% mutate(Log_LoadTime_Desktop = Log_Avg_Load_Time * deviceCategorydesktop), 
  y = target,
  trControl = train_control,
  method = "glm", # logistic regression
  family = "binomial",
  metric = "AUC"
)

tree_model = train(
  x = training_data %>% select(-Avg_Load_Time),
  y = target,
  trControl = train_control,
  method = "rpart", # decision tree
  metric = "AUC",
  tuneLength = 20
)

## model evaluation
results <- resamples(list(logit = linear_model,
                          quad = quad_model,
                          interaction = interaction_model))

summary(results)
dotplot(results)



# Statistical Analysis
## get probabilities
preds <- predict(quad_model, 
                 newdata = training_data %>% mutate(Load_Sq = Avg_Load_Time^2),
                 type = "prob")

## join onto original data
modeling_data <- ga_data %>% mutate(Predicted_Probability_Transaction = preds$X1) %>% 
  filter(Avg_Load_Time < 50) # get rid of outliers

## odds ratio
broom::tidy(linear_model$finalModel) %>%
  mutate(odds_ratio = exp(estimate),
         significant_5_pct = ifelse(p.value <= 0.05, TRUE, FALSE)) %>% 
  DT::datatable(options = list(paging = F, searching = F), rownames = F)

broom::tidy(quad_model$finalModel) %>%
  mutate(odds_ratio = exp(estimate),
         significant_5_pct = ifelse(p.value <= 0.05, TRUE, FALSE)) %>% 
  DT::datatable(options = list(paging = F, searching = F), rownames = F)


broom::tidy(interaction_model$finalModel) %>%
  mutate(odds_ratio = exp(estimate),
         significant_5_pct = ifelse(p.value <= 0.05, TRUE, FALSE)) %>% 
  DT::datatable(options = list(paging = F, searching = F), rownames = F)

## plot
ggplot(modeling_data, 
       aes(x = Avg_Load_Time, 
           y = Predicted_Probability_Transaction, 
           group = userType, 
           color = userType)) +
  geom_line() +
  facet_grid(rows = vars(deviceCategory),
             cols = vars(Traffic_Type)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
  