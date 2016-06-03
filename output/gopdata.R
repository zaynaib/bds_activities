#http://www.gregreda.com/2015/02/15/web-scraping-finding-the-api/
#http://www.datasciguide.com/data-sources-apis-for-data-science-projects/

#do assignment 1 in sql,r,and python
#write about it in blog
#nba data set of jimmy butler
#http://www.analyticsvidhya.com/blog/2016/01/guide-data-exploration/

setwd("D:/git/ds_challenge/output")
library(ggplot2)
sentiment<-read.csv('Sentiment.csv', stringsAsFactors=FALSE,na.strings=c("", "NA","No candidate mentioned"))
View(sentiment)


#first find all the names of each canidate
#no-candiate,donald trump,etc

candidate_names<-unique(sentiment$candidate)


#http://www.cookbook-r.com/Graphs/Bar_and_line_graphs_(ggplot2)/#bar-graphs-of-values
#http://www.statmethods.net/advgraphs/ggplot2.html


#who had the most positive sentiment tweets
#split,apply,combine
#http://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/
#dpply sean anderson

#split by candidate

#for each candidate count the positive sentiments
#split by candidate
#count the positive tweets
#then graph

total_sentiment_counts<-aggregate(candidate~sentiment,sentiment,length)
sentiment_counts<-aggregate(sentiment[,"sentiment"],list(sentiment$candidate,sentiment$sentiment),FUN=length)
colnames(sentiment_counts)<- c("candidates","feelings","counts")

positive<-sentiment_counts[grep("Positive", sentiment_counts$feelings), ]

negative<-sentiment_counts[grep("Negative", sentiment_counts$feelings), ]


#which candidates had the most positive tweets
positive[which.max(positive$counts),]

#which candidate had the most negative tweets
negative[which.max(negative$counts),]

#which candidate had the least negative votes
negative[which.min(negative$counts),]

#which candidates had the least positive votes
positive[which.min(positive$counts),]

#instead of writing all of this code we can do a simple plot
g<-ggplot(sentiment_counts, aes(x=sentiment_counts[,1],y=sentiment_counts[,3], fill=sentiment_counts[,2])) +geom_bar(stat="identity")+
  xlab("Candidates") + ylab("Number of Tweets") +ggtitle("Which Candidate has the most tweets")
print(g)

#which candidate had the highest positve tweets


#for each candiate count the negative sentiments
#same for negative tweets
#apply
#grep "Positive"
#length

#visualize which canidate had the most tweets


#split by candidate
candidate_data <- with(sentiment, split(sentiment,candidate))
#loop through the data and count the frame
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

#what constitutes a missing value (null, blank space, NaN, placeholder value, etc.)
#http://www.theanalysisfactor.com/r-tutorial-count/
length(data[which(is.NA(sentiment$tweet_location)),])
#descriptive summary statistics (min, max, mean, median, variance, number of distinct values, etc.) on each key column in your dataset
summary(sentiment)

