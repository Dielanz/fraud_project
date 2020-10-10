##
View(merged_df)
#subset the entire dataframe for AA stock
sub_df <- subset(subdf, subdf$stockname == 'ADI')
sub_df <- arrange(sub_df, Date)
# #define the starting and ending day of the company. Generate the dates between
#  
start_date <- max(as.Date(sub_df$Date[1]), as.Date('2004-01-02'))
end_date <- min(as.Date(sub_df$Date[nrow(sub_df)]), as.Date('2012-12-31'))
num_days <- seq(start_date, end_date, by="days")
sub_df <- sub_df[(as.Date(sub_df$Date) >= start_date & as.Date(sub_df$Date) <= end_date),]
sub_df$Date <- as.Date(sub_df$Date)
#  
work_days <- as_data_frame(as.Date(intersect(num_days,as.Date(MSDS$Date)), origin="1970-01-01"))
#  
colnames(work_days) <- "Date"
#  
missing_days <- as.data.frame(as.Date(setdiff(as.Date(work_days$Date), as.Date(sub_df$Date)), origin="1970-01-01"))
colnames(missing_days) <- "Date"
#  
merged_df <- bind_rows(sub_df, missing_days)
merged_df <- arrange(merged_df, Date)
# 

Impute_column <- function(df, column)
{
  for(i in 1:length(df[,1]))
  {
    if(is.na(df[,column][i]))
    {
      t1 <- as.numeric(df[,1][i-1])
      t2 <- as.numeric(df[,1][i])
      if(is.na(df[,column][i+1])){
        t3 <- as.numeric(df[,1][i+2])
        df[,column][i] <-  ((t3-t2)*df[,column][i-1] + (t2 - t1)*df[,column][i+2])/(t3 - t1)
      }else{
        t3 <- as.numeric(df[,1][i+1])
        df[,column][i] <- 
          ((t3-t2)*df[,column][i-1] + (t2 - t1)*df[,column][i+1])/(t3 - t1)
      }
    }
  }
  return(df[,column])[i]
}
merged_df$Open <- Impute_column(merged_df, 'Open')
merged_df$High <- Impute_column(merged_df, 'High')
merged_df$Low <- Impute_column(merged_df, 'Low')
merged_df$Close <- Impute_column(merged_df, 'Close')
merged_df$Volume <- Impute_column(merged_df, 'Volume')
merged_df$Adj.Close <- Impute_column(merged_df, 'Adj.Close')



# 
# 
# 
# merged_df[,'Open'][2] <- 
#   ((t3-t2)*merged_df[,'Open'][1] + (t2 - t1)*merged_df[,'Open'][3])/(t3 - t1)
# 
# 
# for(i in 1:length(merged_df[,1]))
#   {
#     if(is.na(merged_df[,'Open'][i]))
#     {
#       t1 <- as.numeric(merged_df[,1][i-1])
#       t3ind <- which.min(as.numeric(merged_df[,1]) > t1)
#       t3 <- as.numeric(merged_df[,1][i+t3ind])
#       print(t3ind)
#       print(t3)
#     }
#     
# }
# 
