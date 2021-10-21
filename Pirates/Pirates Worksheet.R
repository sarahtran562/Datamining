#Pirate Worksheet
#Start Date: 6/11/21

#Working Directory
setwd("~/FRI/R Folder/Pirates")

#Packages Libraries and Databases
library(yarrr)
library(dplyr)
library(ggplot2)
head(pirates)
names(pirates)
str(pirates)
View(pirates)

#Descriptive Statistics
#calculating mean age (1)
mean(pirates$age)
#calculating mean age (2)
pirates%>%
  summarize(mean_age = mean(age))
#height of tallest pirate (1)
max(pirates$height)
#height of tallest pirate (2)
pirates%>%
  summarize(max_height = max(height))
#how many pirates of each gender
pirates%>%
  group_by(sex)%>%
  summarize(count=n())
#average age of pirates of each gender
pirates%>%
  group_by(sex)%>%
  summarize(mean_age = mean(age))
#summary of dataset
summary(pirates)

#Data Plotting: Scatter Plot
#pirate height vs weight
ggplot(data=pirates, aes(x=height, y=weight))+ 
  geom_point(alpha=.5)+ 
  labs(title="Pirate Height V.S. Weight", x="Height (cm)", y="Weight (kg)")+ 
  geom_smooth(method='lm', seom=FALSE)
#pirate age vs parrots owned
ggplot(data=pirates, aes(x=age, y=parrots))+ 
  geom_point(pch=16, col=gray(level=0.5, alpha=0.6))+ 
  labs(title="Pirate Age V.S. Parrots Owned", x="Age (years)", 
       y="Number of Parrots Owned") + geom_smooth(method='lm', seom=FALSE)+ 
  theme_classic()

#Data Plotting: Bar Graphs
#method 1 following protocol
pirates2 <- pirates%>%
  group_by(sex)%>%
  summarize(avgbeard1 = mean(beard.length), sdbeard = sd(beard.length))%>% 
  ungroup()
ggplot(data=pirates2, mapping=aes(x=sex, y=avgbeard1))+
  geom_bar(stat='identity',fill="#bf5700")+
  geom_errorbar(aes(ymin=avgbeard1-sdbeard, ymax=avgbeard1+sdbeard), width=.2,
                position=position_dodge(.9))+
  labs(title="How Long Are the Pirate Beards?")+
  xlab("Pirate Sex")+
  ylab('Average Beard Length')

#method 2 from mohanad
pirates %>% 
  ggplot(aes(x=sex, y=beard.length))+
  geom_bar(stat = 'summary', fun=mean, fill="#bf5700")+
  geom_errorbar(stat='summary',fun.data=mean_se)+
  labs(title="How Long Are the Pirate Beards?")+
  xlab("Pirate Sex")+
  ylab('Average Beard Length')

#Data Plotting: Box Plot
ggplot(data=pirates, aes(x=sex, y=age),fill=headband)+
  geom_boxplot()+
  labs(title="Age Distribution of Pirates")+
  facet_wrap(~headband)

#Data Plotting: Violin Plot
pirateplot(formula=age~sword.type, data=pirates, 
           main="Pirateplot of ages by favorite sword")
pirateplot(formula=height~sex, data=pirates, 
           main="Height Range of Different Pirate Sexes",theme=3, pal = "pony")
piratepal(palette = "all", plot.result=TRUE)

#Hypothesis Testing: Histograms
#average age of pirates who do and do not wear headbands
pirates%>%
  group_by(headband)%>%
  summarize(mean_age = mean(age))
#making no_headband
no_headband <- data.frame(head(filter(pirates, headband=='no')), n=10)
no_headband
#making the dataframes
no_headband_shorter <- data.frame(head(select(filter(pirates, headband=='no'), 
                                              age,headband), n=10))
no_headband_shorter

yes_headband_shorter <- data.frame(head(select(filter(pirates, headband=='yes'), 
                                               age,headband), n=10))
yes_headband_shorter
#histogram for yes
ggplot(data=yes_headband_shorter, aes(x=age))+
  geom_histogram(color="black",fill = "#bf5700",binwidth=1)+
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1)
  labs(title="Age Distribution of Pirates")+
  theme_classic()
#histogram for no
ggplot(data=no_headband_shorter, aes(x=age))+
  geom_histogram(color="black",fill = "#bf5700",binwidth=1)+
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1)
  labs(title="Age Distribution of Pirates")+
  theme_classic()
#no need to make new dataframes
ggplot(data=pirates, aes(x=age))+
  geom_histogram(color="black",fill="white",bins = 35)+
  geom_vline(aes(xintercept=mean(age)), color="blue", linetype="dashed", size=1)+
  labs(title="Age Distribution of Pirates")+
  theme_classic()+
  facet_grid(. ~ headband)
#box plot of the same data
ggplot(data=pirates, aes(x=headband,y=age),fill=headband)+
  geom_boxplot()+
  labs(title="Age Distribution of Pirates")

#Hypothesis Testing: Statistical Tests
#t-test on 2 dataframes
age_headband.htest<-t.test(no_headband_shorter$age, yes_headband_shorter$age, 
                           paired=FALSE,alternative='two.sided')
age_headband.htest$p.value
#t-test on original pirates data
age_headband.htest<-t.test(formula = age ~ headband, data = pirates)
age_headband.htest$p.value
#h-test
age_headband.htest
#correlation test on height and weight
cor.test(formula = ~ height + weight, data = pirates)
#box plot showing # of tattoos based on sword type
ggplot(data=pirates, aes(x=sword.type,y=tattoos))+
  geom_boxplot()+
  labs(title="Number of Tattoos Based On Sword Type")
#anova test
res.aov <-aov(tattoos ~ sword.type, pirates)
summary(res.aov)

#Some Programming Practice
no_headband[1,1]
no_headband[1:3,4:8]
no_headband[3:1, c(12,11,16)]
#mean of sword.time vector
pirates %>% 
  summarize(mean_time = mean(pirates[1-1000,14]))
#length of sword.time vector
pirates %>% 
  summarize(length_time = length(pirates[1-1000,14]))
#sum of the elements of sword.time vector
pirates %>% 
  summarize(sum_time = sum(pirates[1-1000,14]))
#new vector: swordresults
swordresults <- c(mean(pirates[1-1000,14]), length(pirates[1-1000,14]), 
                                                  sum(pirates[1-1000,14]))
swordresults
#choose one item at random
sample(swordresults,1,replace=TRUE)
#vector representing outcomes of a coin toss
coin_toss <- c('heads','tails')
coin_toss
#toss the coin
tossed <- sample(coin_toss,1,replace=TRUE)
print(tossed)
if (tossed=='tails'){
  print("TAILS Woohoo!")
}
#putting it together in a for loop: fxn tossing a coin a random number of times
cointossfxn<-function(x){
  tail.count <- 0
  for(i in 1:x) {
    coin_toss <- c('heads','tails')
    coin_toss
    tossed <- sample(coin_toss,1,replace=TRUE)
    if (tossed=='tails'){
      print("TAILS Woohoo!")
      tail.count <- tail.count +1
    }
  }
  print(tail.count)
  return(tail.count)
}
cointossfxn(x=1000)
#another for loop: 5000 experiments, each one tossing a coin 40 times
vectorname<-rep(NA, 5000)
for (i in 1:5000){
  tailcountx<-cointossfxn(x=40)
  vectorname[i] <- tailcountx
}
print(vectorname)
#coin toss histogram
datavector<-as.data.frame(vectorname)
ggplot(data=datavector, aes(x=vectorname))+
  geom_histogram(binwidth=1,color="black",fill="white")+
  labs(title="Coin Flipping Experiment",x="Number of Tails After 40 Flips",
       y="Frequency")+
  theme_classic()



