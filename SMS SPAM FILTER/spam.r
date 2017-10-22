library(tm) #text cleaning
library(SnowballC) #stemming
library(e1071) #Naive Bayes
library(gmodels) #tablular data for predictions

sms_raw <- read.csv("sms_spam", stringAsFactors = FALSE) #Loading data
str(sms_raw)
sms_raw$type <- factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
sms_corpus <- VCorpus(VectorSource(sms_raw$type) #Creating Corpus for tm package
print(sms_corpus)
inspect(sms_corpus[1:2]) #details for first two messages
as.character(sms_corpus[[1]]) #complete first message text
sms_corpus_clean <- tm_map(sms_corpus,
content_transformer(tolower)) #lower case
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers) #remove numbers
sms_corpus_clean <- tm_map(sms_corpus_clean,
removeWords, stopwords()) #remove stopwords
ms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #remove punctuation
replacePunctuation <- function(x) {
gsub("[[:punct:]]+", " ", x) #creating custom function to remove punctuations
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument) #stemming document
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) #removing extra white spaces
sms_dtm <- DocumentTermMatrix(sms_corpus_clean) #creating a document term matrix

#Creating Traing and Test data sets

sms_dtm_train <- sms_dtm[1:4169, ] 
sms_dtm_test <- sms_dtm[4170:5559, ]
sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

#Visualizing the data using wordcloud

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE) #visualizing words in the clean set with frequency greater than 50
spam <- subset(sms_raw, type == "spam") #creating data subsets for spam and ham
ham <- subset(sms_raw, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5)) #visualizing spam and ham (40 words)
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))
sms_freq_words <- findFreqTerms(sms_dtm_train, 5)
str(sms_freq_words)
sms_dtm_freq_train<- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#Preparing data for Naive Bayes 

convert_counts <- function(x) {
x <- ifelse(x > 0, "Yes", "No")
}

#Naive Bayes only works on catageorical data
sms_train <- apply(sms_dtm_freq_train, MARGIN = 2,
convert_counts)
> sms_test <- apply(sms_dtm_freq_test, MARGIN = 2,
convert_counts)

#Training model
sms_classifier <- naiveBayes(sms_train, sms_train_labels)

#Making predictions

sms_test_pred <- predict(sms_classifier, sms_test)

#Displaying predictions in tabular form 

CrossTable(sms_test_pred, sms_test_labels,
prop.chisq = FALSE, prop.t = FALSE,
dnn = c('predicted', 'actual'))

#Improving model using laplace
sms_classifier2 <- naiveBayes(sms_train, sms_train_labels,
laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test) #Making predictions

#Displaying predictions in tabular form 
CrossTable(sms_test_pred2, sms_test_labels,
prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
dnn = c('predicted', 'actual'))



