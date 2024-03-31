#https://www.kaggle.com/nicapotato/womens-ecommerce-clothing-reviews/home
data <- read.csv("Womens Clothing E-Commerce Reviews.csv")
data=data[which(data$Recommended.IND==1),] #note:資料太大 清理時會跑一陣子



library(tm)
## Make a vector source and a corpus
x=Corpus(VectorSource(data$Review.Text))

##Clean text
x=tm_map(x, tolower) #convert to lower case
x<-tm_map(x, content_transformer(tolower))

x=tm_map(x, removePunctuation) #remove punctuation 标点符号

#Remove stopwords
x=tm_map(x, removeWords, stopwords("english"))


x=tm_map(x, stemDocument)
x[[8]][1]

x_tdm <- TermDocumentMatrix(x) #term is row；document is column
inspect(x_tdm)
# Convert TDM to matrix
review_m <- as.matrix(x_tdm)
# Sum rows and frequency data frame
freq_df <- rowSums(review_m)
# Sort term_frequency in descending order 排序
freq_df <- sort(freq_df, decreasing = T)
# View the top 10 most common words
freq_df[1:10]
barplot(freq_df[1:20], col = "royalblue", las = 2)

freq_df <- data.frame(word = names(freq_df),
                               num = freq_df)

library(wordcloud2)
wordcloud2(freq_df,size=0.5)


x_tdm2 <- removeSparseTerms(x_tdm, sparse = 0.9)
mydata <- as.data.frame(as.matrix(x_tdm2))
library("proxy")
hc <- hclust(d = dist(mydata, method = "cosine"), method = "complete")
plot(hc)

#sentiments 
library(tidytext)
get_sentiments("bing")  #bing 内建字典
bing_word_counts <- freq_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) 

bing_word_counts

table(bing_word_counts$sentiment)
bing_word_counts %>% 
  filter(sentiment == "positive") %>% 
  select(word,n)%>% 
  wordcloud2()

