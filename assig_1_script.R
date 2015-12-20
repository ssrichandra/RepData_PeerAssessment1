#1. Load the data (i.e. read.csv())

temp<-"activity.zip"
data<-read.csv(unz(temp,"activity.csv"))

#2. Process/transform the data (if necessary) into a format suitable for your analysis

#Question 1: What is mean total number of steps taken per day?
library("plyr")

q1_data <- na.omit(ddply(data,"date",summarize,Steps_per_day=sum(steps)))


#Create the Histogram plot,  - Steps per Day
hist(q1_data$Steps_per_day,main="Histogram of Steps Per Day")

#Calculate the mean and media of the data
q1_mean<-mean(q1_data$Steps_per_day)

q1_median<-median(q1_data$Steps_per_day)


#Question 2: What is the average daily activity pattern
        #Set data summary as Q2_data
                q2_data<-na.omit(data)

                q2_data_avgsteps<-ddply(q2_data,"interval",summarize,Avg_steps=mean(steps))
        
        #Plot line chart
                plot(q2_data_avgsteps$interval,q2_data_avgsteps$Avg_steps,type="l")

        #Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
        q2_data_avgsteps[which.max(q2_data_avgsteps$Avg_steps),1]

#Question 3: Input missing values
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

        sum(is.na(data$steps))


#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated.
#using the mean for that 5-minute interval

#Create a new dataset that is equal to the original dataset but with the missing data filled in.


        clean_data<-data
        
        navalues<-is.na(clean_data$steps)
        
        mean_interval <- tapply(clean_data$steps, clean_data$interval, mean, na.rm=TRUE, simplify=TRUE)
        
        clean_data$steps[navalues] <- mean_interval[as.character(clean_data$interval[navalues])]


#Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total 
#number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? 
#What is the impact of imputing missing data on the estimates of the total daily number of steps?

q3_data <- ddply(clean_data,"date",summarize,Steps_per_day=sum(steps))


#Create the Histogram plot,  - Steps per Day
hist(q3_data$Steps_per_day,main="Histogram of Steps Per Day")

#Calculate the mean and media of the data
q3_mean<-mean(q3_data$Steps_per_day)

q3_median<-median(q3_data$Steps_per_day)

#What is the impact of imputing missing data on the estimates of the total daily number of steps?
print("cleaned data, has same median and mean value")


#Question 4: Are there differences in activity patterns between weekdays and weekends?
        #Create a new factor variable in the dataset with two levels 
        #- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

        clean_data$Weekday<-weekdays(as.Date(data2$date))

        clean_data$Type<-ifelse(clean_data$Weekday=="Saturday","Weekend",ifelse(clean_data$Weekday=="Sunday","Weekend","Weekday"))


        #Create Panel - 2 rows and 1 column
        par(mfrow = c(2, 1))

        #Set Margins
        par(mar = c(2, 2, 1, 1), oma = c(0.75, 0.75, 0.5, 0.5))

        #Compile Plots
        plot(q4_weekday$interval,q4_weekday$Avg_steps,type="l",main="Weekday")
        plot(q4_weekend$interval,q4_weekend$Avg_steps,type="l",main="Weekend")
        
