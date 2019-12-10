library(ISLR)
library(glmnet)
library(dplyr)
library(tidyr)
library(lattice)
library(lme4)
library(plotmo)
library(labels)
library(lmerTest)

### Final project Stackoverflow data
#### Lets do it


### Read the input file
public_responses = read.csv("/Users/ishangupta/Downloads/developer_survey_2019 (1)/survey_results_public.csv")
schema = read.csv("/Users/ishangupta/Downloads/developer_survey_2019 (1)/survey_results_schema.csv")

#### Lets start analyzing the difference by different countries
salary_data_complete = public_responses[!is.na(public_responses$ConvertedComp),]

### Filter out the data with outliers
### Only consider salaries greater than 10000 and less than 1000000
salary_data_complete <- salary_data_complete[salary_data_complete$ConvertedComp > 10000,]
salary_data_complete <- salary_data_complete[salary_data_complete$ConvertedComp < quantile(salary_data_complete$ConvertedComp,prob=1-3/100),]

country_level_data <- data.frame(count=sort(table(salary_data_complete$Country), decreasing=TRUE))
top_countries = c('United States', 'United Kingdom', 'Germany', 'Canada',"Netherlands", "Australia", "Sweden", "Switzerland")

country_required <- salary_data_complete[salary_data_complete$Country %in% top_countries, ]


### Histogram of salary data
### Log looks better
hist(salary_data_complete$ConvertedComp, col = c("royalblue2"))
hist(log(salary_data_complete$ConvertedComp), col = c("royalblue2"))
# grouped boxplot
ggplot(country_required, aes(x=Country, y=ConvertedComp)) +
  geom_boxplot(fill = "royalblue2") +
  scale_y_continuous(limits = quantile(country_required$ConvertedComp, c(0.1, 0.9)))+
  theme_bw()

### Filter out only required degrees
### Consider only required degrees
education_level_data <- data.frame(count=sort(table(salary_data_complete$EdLevel), decreasing=TRUE))
required_education_levels <- c("Bachelor’s degree (BA, BS, B.Eng., etc.)", "Master’s degree (MA, MS, M.Eng., MBA, etc.)", "Some college/university study without earning a degree", "Associate degree", "Other doctoral degree (Ph.D, Ed.D., etc.)")
education_required <- salary_data_complete[salary_data_complete$EdLevel %in% required_education_levels, ]
education_required_us <- salary_data_complete[salary_data_complete$EdLevel %in% required_education_levels & (salary_data_complete$Country == "United States"), ]
ggplot(education_required_us, aes(x=EdLevel, y=ConvertedComp)) + 
  geom_boxplot(fill = "royalblue2") +
  scale_y_continuous(limits = quantile(country_required$ConvertedComp, c(0.1, 0.9)))+
  theme_bw()


#### DevType
######
#####

            ######### Sanitize developer names
#####
####

#### Lets look at technologies now, consider only first technology
#### Extracting technology
a <- str_split_fixed(salary_data_complete$LanguageWorkedWith, ";", 10)
salary_data_complete_with_tech <- cbind(salary_data_complete, a)
names(salary_data_complete_with_tech) <- make.names(names(salary_data_complete_with_tech))
salary_data_complete_with_tech$LangaugesKnown <- sapply(strsplit(as.character(salary_data_complete_with_tech$LanguageWorkedWith), ";"), length)

tech_level_data <- data.frame(count=sort(table(salary_data_complete_with_tech$X1), decreasing=TRUE))
top_techs <- c("Bash/Shell/PowerShell", "HTML/CSS","C#", "Java", "C", "C++", "Assembly", "JavaScript", "Go", "Python", "Objective-C",
"Elixir", "Clojure", "SQL", "R")
tech_required <- salary_data_complete_with_tech[salary_data_complete_with_tech$X1 %in% top_techs, ]
tech_required$YearsCodePro[tech_required$YearsCodePro == "Less than 1 year"] <- 1
tech_required$YearsCodePro[tech_required$YearsCodePro == "More than 50 years"] <- 50
tech_required$YearsCodePro <- as.numeric(tech_required$YearsCodePro)
# 
# tech_required$YearsCodePro[tech_required$YearsCodePro >= 15] <- 15
# tech_required$YearsCodePro[tech_required$YearsCodePro >= 10 & tech_required$YearsCodePro < 15] <- 10
# tech_required$YearsCodePro[tech_required$YearsCodePro >= 5 & tech_required$YearsCodePro < 10] <- 5
# tech_required$YearsCodePro[tech_required$YearsCodePro >= 3 & tech_required$YearsCodePro < 5] <- 3
# tech_required$YearsCodePro[tech_required$YearsCodePro >= 1 & tech_required$YearsCodePro < 3] <- 1

final_data <- tech_required[(tech_required$EdLevel %in% required_education_levels) & (tech_required$Country %in% top_countries),]
final_data$new_dev = sub("^Developer.*", "Developer", final_data$DevType)

final_data$new_dev[startsWith(as.character(final_data$DevType), "Developer")] <- "Developer"
b <- str_split_fixed(final_data$new_dev, ";", 2)
final_data <- cbind(final_data, b)
names(final_data) <- make.names(names(final_data), unique = TRUE)
dev_level_data <- data.frame(count=sort(table(final_data$new_dev), decreasing=TRUE))
dev_positions <- c("Developer", "Data scientist or machine learning specialist", "Data or business analyst", "Engineering manager", "DevOps specialist", "Designer",
"Engineer, data")

final_data_with_position = final_data[final_data$X1.1 %in% dev_positions, ]


unique(final_data$new_dev)
ggplot(tech_required, aes(x=X1, y=ConvertedComp)) + 
  geom_boxplot(fill = "royalblue2") +
  scale_y_continuous(limits = quantile(tech_required$ConvertedComp, c(0.1, 0.9)))



test_df <- tech_required %>%
  group_by(Country) %>%
  dplyr::summarize(Mean = mean(ConvertedComp, na.rm=TRUE))

### create better data divisions
ggplot(data=test_df, aes(x=as.numeric(YearsCodePro), y=Mean, group=X1)) +
  geom_line() +
  geom_point()

model <- lm(log(ConvertedComp) ~ as.factor(X1) + as.factor(Country) + as.factor(EdLevel) + as.numeric(YearsCodePro), data = tech_required)
summary(model)

####
final_data_sanitized = final_data_with_position[c("ConvertedComp", "Country", "EdLevel", "YearsCodePro", "X1.1", "X1", "LastHireDate", "OpenSourcer", "OpSys", "OrgSize", "YearsCodePro")]
final_data_sanitized <- droplevels(final_data_sanitized)
final_data_sanitized$ConvertedComp <- log(final_data_sanitized$ConvertedComp)
final_data_sanitized$Country <- as.factor(final_data_sanitized$Country)
final_data_sanitized$EdLevel <- as.factor(final_data_sanitized$EdLevel)
final_data_sanitized$X1 <- as.factor(final_data_sanitized$X1)
final_data_sanitized$X1.1 <- as.factor(final_data_sanitized$X1.1)
final_data_sanitized$LastHireDate <- as.factor(final_data_sanitized$LastHireDate)
final_data_sanitized$OpSys <- as.factor(final_data_sanitized$OpSys)
final_data_sanitized$OpenSourcer <- as.factor(final_data_sanitized$OpenSourcer)
final_data_sanitized$OrgSize <- as.factor(final_data_sanitized$OrgSize)

### Stepwise BIC

final_data_sanitized_clean = na.omit(final_data_sanitized)
# x = model.matrix(ConvertedComp~., final_data_sanitized_clean)[,-1]
# y = final_data_sanitized_clean %>%
#   select(ConvertedComp) %>%
#   unlist() %>%
#   as.numeric()

set.seed(1)

train = final_data_sanitized_clean %>%
  sample_frac(0.7)

test = final_data_sanitized_clean %>%
  setdiff(train)

x_train = model.matrix(ConvertedComp~., train)[,-1]
x_test = model.matrix(ConvertedComp~., test)[,-1]

y_train = train$ConvertedComp %>% unlist()

y_test = test$ConvertedComp %>% 
set.seed(1)
cv.out = cv.glmnet(data.matrix(train),train$ConvertedComp, y_train, alpha = 1) # Fit lasso model on training data
plot(cv.out)
# bestlam = 0.01394185 # Select lamda that minimizes training MSE

 # Draw plot of training MSE as a function of lambda
# lasso_pred = predict(cv.out, s = bestlam, newx = x_test) # Use best lambda to predict test data
# sse <- sum((lasso_pred - y_test)^2) # Calculate test MSE
# sst <- sum((y_test - mean(y_test))^2)
# rsq <- 1 -sse/sst
# plot(cv.out)
# lasso_coef = predict(cv.out, type = "coefficients", s = bestlam)[1:20,] # Display coefficients using lambda chosen by CV
# lasso_coef[lasso_coef != 0]
# plotres(cv.out)

##### Find the variance
#### Model validation looks good



model_full <- lm(ConvertedComp ~ (X1 +X1.1 + EdLevel + Country + LastHireDate + OpSys + OpenSourcer),
                 data=final_data_sanitized_clean)



model_base <- lm(ConvertedComp ~ 1,
                 data=final_data_sanitized_clean)


model_stepwise_bic = step(model_base,
                          scope = list(upper = model_full,
                                       lower = model_base),
                          direction = 'both',
                          trace=0,
                          k = 2)






















##########################
##############################
##########3
################ Hierarchial model
### My baseline is Associate, Business Analyst,  Open Source  - Less than once a month but more than once per year

mod1 <- lmer(ConvertedComp ~  (1|Country) + EdLevel + (1|X1) + X1.1 + OpenSourcer + YearsCodePro, data = train)



final_model <- lmer(log(ConvertedComp) ~  (1|Country) + EdLevel + (1|Tech) + Designation + OpenSourcer + OrgSize, data = train)




coef(RandomIntercepts5)


#these equal the fixed effects plus the random effect
fixef(mod1)
ranef(mod1)
dotplot(ranef(mod1,
              condVar=TRUE))

plot(y = residuals(mod1), x = fitted(mod1) , xlab= "Weight", ylab = "Residuals", col = "royalblue2")
qqnorm(residuals(mod1), col = "royalblue2")


predicted <- predict(mod1, newdata = test)

sse <- sum((predicted - test$ConvertedComp)^2) # Calculate test MSE
sst <- sum((test$ConvertedComp - mean(test$ConvertedComp))^2)
rsq <- 1 -sse/sst

summary(predicted)

dotplot(ranef(mod1, condVar=TRUE))
#####################33