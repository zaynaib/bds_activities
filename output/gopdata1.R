

#do assignment 1 in sql,r,and python
#write about it in blog
#nba data set of jimmy butler
#http://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/

setwd("D:/git/bds_activities/output")
library(ggplot2)
library(wordcloud)

sentiment<-read.csv('Sentiment.csv', stringsAsFactors=FALSE,na.strings=c("", "NA","No candidate mentioned"))
View(sentiment)


#descriptive summary statistics (min, max, mean, median, variance, number of distinct values, etc.) on each key column in your dataset
summary(sentiment)

#find all the names of each canidate

candidate_names<-unique(sentiment$candidate)


#split by candidate

#for each candidate count the positive sentiments
#split by candidate
#count the positive tweets
#then graph

total_sentiment_counts<-aggregate(candidate~sentiment,sentiment,length)

#sentiment counts by each candidate
sentiment_counts<-aggregate(sentiment[,"sentiment"],list(sentiment$candidate,sentiment$sentiment),FUN=length)
colnames(sentiment_counts)<- c("candidates","feelings","counts")

#posititve sentiment counts for each candidate
positive<-sentiment_counts[grep("Positive", sentiment_counts$feelings), ]

#negative sentiment counts for each candidate
negative<-sentiment_counts[grep("Negative", sentiment_counts$feelings), ]


#which candidates had the most positive tweets
positive[which.max(positive$counts),]

#which candidate had the most negative tweets
negative[which.max(negative$counts),]

#which candidate had the least negative tweets
negative[which.min(negative$counts),]

#which candidates had the least positive tweets
positive[which.min(positive$counts),]

#simple plot of sentiment counts
g<-ggplot(sentiment_counts, aes(x=sentiment_counts[,1],y=sentiment_counts[,3], fill=sentiment_counts[,2])) +geom_bar(stat="identity")+
  xlab("Candidates") + ylab("Number of Tweets") +ggtitle("Which Candidate has the most tweets")
print(g)

#which candidate had the highest positve tweets


#split by candidate

candidateData<-split(sentiment, sentiment$candidate)

#wordcloud Ben Carson
wordcloud(candidateData[[1]]$text,max.words = 100,colors=brewer.pal(8, "Dark2"))





#split the candidates with their tweets
candidateTweets<-data.frame(sentiment$candidate,sentiment$text)
candidateTweets <-candidateTweets[order(candidateTweets$sentiment.candidate),]


#sentitment using splity, apply,combine

candidate_data <- with(sentiment, split(sentiment,candidate))
countCandidate<-lapply(candidate_data, length)
counts<-unlist(countCandidate)

#alternatively you can use the aggeregate function and save lines of code
count_names<-aggregate(sentiment$candidate,list(sentiment$candidate),FUN=length)

colnames(count_names) <-c("candidate","count")

g<-ggplot(count_names, aes(x=count_names[,1],y=count_names[,2])) +geom_bar(stat="identity")+
  xlab("Candidates") + ylab("Number of Tweets") +ggtitle("Which Candidate has the most tweets")
print(g)

#Or we can create a bubble plot
#http://www.matthewmaenner.com/blog/2010/11/23/how-to-make-bubble-charts-in-ggplot2/

b <- ggplot(count_names, aes(count_names[,1],count_names[,2],size=count_names[,2], label=count_names[,1],fill=count_names[,1]))
b <- b+geom_point(colour="red") +geom_text(size=3)+scale_size_area(max_size = 15)



#who had the most negative sentiment tweets

#visualize subject matter
#wordcloud subject matter

#which user had the most retweets
max_retweet<-sentiment[which.max(sentiment$retweet_count),]
print(paste(max_retweet$name,max_retweet$text))
#which.max retweets
#use the subset fucntion

#produce a word cloud about black lives matter
sentiment[grep("Donald",sentiment$candidate),]
blm<-sentiment[grep("Black Lives",sentiment$text,ignore.case=TRUE),]
wordcloud(blm)
wordcloud()



#how many rows of data are in your dataset
nrow(sentiment)
#how to preview a sample of rows in your dataset
head(sentiment)
tail(sentiment)
#how to refer to a particular row and column to find a value
#grepl??
sentiment[grep("Donald",sentiment$candidate),]
#for each column, how many rows have data values, and what datatype are they
str(sentiment)



