subject_name <- c("John Doe", "Jane Doe", "Steve Graves")
temperature <- c(98.1, 98.6, 101.4)
flu_status <- c(FALSE, FALSE, TRUE)
temperature[2]
blood <- factor(c("O", "AB", "A"), levels = c("A", "B", "AB", "O"))
blood
subject1 <- list(full_name = subject_name[1],
                 temperature = temperature[1],
                 flu_status = flu_status[1],
                 blood = blood[1])
subject1
pt_data <- data.frame(subject_name, temperature, flu_status,
                      blood, stringsAsFactors = FALSE)


#save(x, y, z, file = "data.RData") where x, y, z are any objects
#load("data.RData")
#pt_data <- read.csv("pt_data.csv", stringsAsFactors = FALSE)
#write.csv(pt_data, file = "pt_data.csv")


#vv making stuff with odbcconnections, didn't get far
#install.packages("RODBC")
#library(RODBC)
#mydb <- odbcConnect("LocalDSN")
#mydb <- odbcConnect("my_dsn", uid = "my_username", pwd = "my_password")

#linear discetion

usedcars <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter02/usedcars.csv", stringsAsFactors = FALSE)
usedcars$year
summary(usedcars)
summary(usedcars[c("price", "mileage")])
quantile(usedcars$price, seq(from = 0, to = 1, by = .2)) 
boxplot(usedcars$price, main = "Boxplot of Used Car Prices", ylab = "Price in $")
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab = "Price in $")
var(usedcars$price); sd(usedcars$price)

#model <- lm(usedcars$price ~ usedcars$mileage * usedcars$year)
#summary(model)

model_table <- table(usedcars$year)
prop.pct <- prop.table(model_table)*100; round(prop.pct, digits = 1)

plot(x = usedcars$mileage, y = usedcars$price, main = "Scatterplot of Price v Mileage", xlab ="Used Car Odometer (mi.)", ylab = "Used Car Price ($)")
model.1 <- lm(usedcars$price ~ usedcars$mileage)
abline(model.1)

install.packages("gmodels")
library(gmodels)
usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
CrossTable(x = usedcars$model, y = usedcars$conservative)

#babies <- babies[-1] -- drops column 1

normalize <- function(x) {
  return ((x-mean(x))/sd(x))
}
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))
babiesbwt_n <- as.data.frame(lapply(babies[1:2], normalize))

#kNN

wbcd <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter03/wisc_bc_data.csv", stringsAsFactors = FALSE)
wbcd <- wbcd[-1] #no more column 1
table(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c('B', 'M'), labels = c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis))*100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n$area_mean)
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]
wbcd_train_levels <- wbcd[1:469, 1]
wbcd_test_levels <- wbcd[470:569, 1]
library(class)
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_levels, k = 21) #uses knn(train, test, class, k)
#usually use something close to # in training data (ie 21 approx sqrt(469))

library(gmodels)
CrossTable(x = wbcd_test_levels, y = wbcd_pred, prop.chisq = FALSE) #in quadrants: Q1: FP; Q2: TN; Q3: FN; Q4: TP
#wbcd_n was supposed to be min-maxed to (x-min)/(max-min) where FN = .02

#not great, less try improving
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)
wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ] #wbcd_n was supposed to be min-maxed to (x-min)/(max-min)
wbcd_train_levels <- wbcd[1:469, 1]
wbcd_test_levels <- wbcd[470:569, 1]
wbcd_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_levels, k = 21)
CrossTable(x = wbcd_test_levels, y = wbcd_pred, prop.chisq = FALSE) #k=1 WOULD be better in this specific case, though don't use as it changes between patients

#naïve Bayes

sms_raw <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter04/sms_spam.csv", stringsAsFactors = FALSE)
head(sms_raw)
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:3])
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords()) #stopwords() removes common words "and", "to", "by", ...
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(corpus_clean)

sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test <- corpus_clean[4170:5559]

prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))
spam <- subset(sms_raw_train, type == "spam")
ham <- subset(sms_raw_test, type == "ham")
library(wordcloud)
wordcloud(spam$text, max.words = 40, scale = c(3, .5))
wordcloud(ham$text, max.words = 40, scale = c(3, .5))

findFreqTerms(sms_dtm_train, 5)
sms_dict <- c(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

convert_counts <- function(x) {
  x <- ifelse(x>0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return(x)
}

sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)
library(e1071)

sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_test_pred <- predict(sms_classifier, sms_test)
library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type, prop.chisq = FALSE, dnn = c('predicted', 'actual'))

#decision trees

#infogain(F) = Entropy_pre_split(S_1) - Entropy_post_split(S_2)
credit <- read.csv("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter05/credit.csv", stringsAsFactors = FALSE)
str(credit)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)

set.seed(12345)
credit_rand <- credit[order(runif(1000)), ]
credit_train <- credit_rand[1:900, ]
credit_test <- credit_rand[901:1000, ]
prop.table(table(credit_train$default))

library(C50)
credit_train$default<-as.factor(credit_train$default)
credit_model <- C5.0(x = credit_train[-17], y = credit_train$default)
credit_model
summary(credit_model)

credit_pred <- predict(credit_model, credit_test)
library(gmodels)
CrossTable(credit_test$default, credit_pred, prop.r = FALSE, prop.c = FALSE, dnn = c('actual default', 'predicted default'))

#using adapive boosting
credit_boost10 <- C5.0(credit_train[-17], credit_train$default, trials = 10)
credit_boost10
summary(credit_boost10)
credit_boost_pred10 <- predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10, prop.c = FALSE, prop.r = FALSE, dnn = c('actual default', 'predicted default'))

#making a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
#error_cost
credit_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)
CrossTable(credit_test$default, credit_cost_pred, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))

#making my own
credit_boost_cost <- C5.0(credit_train[-17], credit_train$default, costs = error_cost, trials = 10)
credit_boost_cost_pred <- predict(credit_boost_cost, credit_test)
CrossTable(credit_test$default, credit_boost_cost_pred, prop.chisq = FALSE, dnn = c('actual default', 'predicted default'))
