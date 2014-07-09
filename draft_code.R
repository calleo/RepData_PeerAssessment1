library(ggplot2)
CSV_FILE <- "activity.csv"
ZIP_FILE <- "activity.zip"

if(!file.exists(CSV_FILE)) {
  unzip(ZIP_FILE)
}

act_data <- read.csv(CSV_FILE)
act_data$date <- as.Date(act_data$date, "%Y-%m-%d")

tot_steps_day <- aggregate(x = act_data$steps, by = list(date = act_data$date), FUN = "sum")
colnames(tot_steps_day) <- c("Date", "Steps")

qplot(x=tot_steps_day$Steps, geom = "histogram", xlab="Total no. of steps", ylab="Frequency")
#mean(x=tot_steps_day$Steps, na.rm=TRUE)
#median(x=tot_steps_day$Steps, na.rm=TRUE)

# Remove date column
#act_pattern <- act_data[,c(1,3)]
#act_pattern <- aggregate(x = act_data$steps, by = list(interval = act_data$interval), FUN = "mean", na.rm=TRUE)

for(i in 1:dim(act_data)[2]) {
  act_data[i]
}
