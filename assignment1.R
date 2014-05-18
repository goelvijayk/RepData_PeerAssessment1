
#download file from github
#unzip file to get activity.zip in working directory
setwd("~/Training/Reproducible research/assignment1/RepData_PeerAssessment1")

#part1: read data
activity <- read.csv("activity.csv")
activity$date2<- as.Date(activity$date, format = "%Y-%m-%d") #formatted date





#part2: daily summary
d<- data.frame(unique(activity$date2))
names(d)<- "date2"

for (i in 1:nrow(d))
{
  #print(i)
  #print(d[i,1])
  t<- activity[activity$date2==d[i,1],1]
  #print(head(t,1))
  d[i,2]<- sum(t, na.rm = TRUE)
  #print(head(d,1))
  d[i,3]<- mean(t, na.rm = TRUE)
  d[i,4]<- median(t, na.rm = TRUE)
}

print(c("Mean=", mean(d[,2])), sep = "")
print(median(d[,2]))

#part3: interval summary
d<- data.frame(unique(activity$interval))
names(d)<- "interval"

for (i in 1:nrow(d))
{
  #print(i)
  #print(d[i,1])
  t<- activity[activity$interval==d[i,1],1]
  #print(head(t,1))
  #d[i,2]<- sum(t, na.rm = TRUE)
  #print(head(d,1))
  d[i,2]<- mean(t, na.rm = TRUE)
  #d[i,4]<- median(t, na.rm = TRUE)
}

d[d[,2]==max(d[,2]),1]
#######


sum(!complete.cases(activity))







# Part5: Imputing missing values
sum(!complete.cases(activity))

a<- activity
#d<- data.frame(unique(a$date2))
#names(d)<- "date2"

for (i in 1:nrow(a))
{
  if(is.na(a[i,1])) 
    {
    t<- a[i,3]
    a[i,1]<- d[d[,1]==t,2]
    }
}



d<- data.frame(unique(a$date2)) #just the dates for aggregation
names(d)<- "date2" 

for (i in 1:nrow(d)) #loop through each date
{
  t<- a[a$date2==d[i,1],1] #store current date in t
  d[i,2]<- sum(t, na.rm = TRUE)   #aggregate and keep adding to data frame d (one value for each date)
  d[i,3]<- mean(t, na.rm = TRUE)
  d[i,4]<- median(t, na.rm = TRUE)
}
names(d)<- c("Date", "Total Steps", "Mean Steps", "Median Steps")

plot(d[,1], d[,2], type = "h", xlab = "Date", ylab="Total steps", main = "Histogram of total steps each day")


print(d[,c(1,3,4)])

###############

a$weekday<- weekdays(a$date2)
a$w_tf<- a$weekday == "Saturday" | a$weekday == "Sunday"
a$wday_flag <- factor(a$w_tf, labels = c("weekend","weekday"))

#names(a)

a_part5<- sqldf("select wday_flag, interval, avg(steps) as avg_steps
                from a
                group by wday_flag, interval")


plot_part5<- function() {
ggplot(data = a_part5, aes(x=interval, y=avg_steps)) +
  geom_line(color = "steelblue", size = 0.5)+
  facet_wrap(~wday_flag, nrow=2, ncol=1) +
  theme_bw() +
  xlab("Interval") +
  ylab("Number of steps") 
}
plot_part5()


ggplot(data = a_part5,aes(x=interval, y=avg_steps)) +  geom_line(color = "black", size = 1) 

