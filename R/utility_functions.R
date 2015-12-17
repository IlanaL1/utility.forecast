# To create a git repository: see http://www.r-bloggers.com/create-r-package-rstudio-github-devtools/
  

#' @title Select subset of data.
#' @name data_frame_select
#' @description Selects a subset of input data from a data-frame
#' @author Ilana Lichtenstein
#' @param input_df A dataframe with a column labelled 'date', and 1 or more other columns
#' @export
data_frame_select<-function(input_df,start_date,end_date,num_var,rem_dups=FALSE){
  subset_df<-subset(input_df,input_df$date >= start_date & input_df$date <= end_date)[,1:(num_var+1)]
  if(rem_dups==TRUE){
    subset_df<-subset_df[!duplicated(lapply(subset_df, summary))]
  }
  subset_df  #return 
}

# test 
# A=matrix(c(1:12),nrow=3)
# A_df<-data.frame(A)
# dates<-seq.Date(from=as.Date("2011-01-01"),length.out=3,by=1)
# B_df<-cbind(dates,A_df)
# B_subset<-data_frame_select(B_df,as.Date("2011-01-01"),as.Date("2011-01-03"),num_var=2)

#xreg_select<-data.frame(
#  DATE_ID=B_subset$dates,
#  VAR1=if(num_var>=1) {B_subset$X1} else{c(rep(NA,nrow(B_subset)))},
#  VAR2=if(num_var>=2) {B_subset$X2} else{c(rep(NA,nrow(B_subset)))},
#  VAR3=if(num_var>=3) {B_subset$X3} else{c(rep(NA,nrow(B_subset)))}
#)

#' @name get_train_start_end
#' @title gets start and end dates for input dates
#' @description Get start and end dates for input period (typically a training period)
#' @param DATE_ID A list of dates 
# @example dt<-get_train_start_end(DATE_ID)
#' @export
get_train_start_end<-function(DATE_ID){  
train_start_date=as.Date(DATE_ID[1],"%d/%m/%Y")
train_end_date<-as.Date(DATE_ID[length(DATE_ID)] ,"%d/%m/%Y")  # length first_training_dates gives the index of the last day of training data
list(train_start_date=train_start_date,train_end_date=train_end_date)
}


#' @name get_horizon_start_end
#' @title gets start and end dates for horizon
#' @description Get start and end dates for horizon
#' @param train_end_date A date
#' @param horizon A number
#' @param freq_type A value of 1 or 2 (weekdays). See type_to_descrip
#' @param freq_descrip: e.g. days, weeks
# @example dt2<-get_horizon_start_end(dt$train_end_date,horizon=30,2,"days")
#' @export
get_horizon_start_end<-function(train_end_date, horizon, freq_type, freq_descrip){
library(timeDate)
horizon_start_date=train_end_date+1
#horizon_sequence_dates<-seq(from=horizon_start_date, length.out=hor, by=1) #length.out not working
end_date<-horizon_start_date+15*365 # 15 years worth of values (e.g. 15*365, or 15*52 etc
horizon_sequence_dates <- seq(from=horizon_start_date, to=end_date, by=freq_descrip)  ## set "by" to be days, weeks, months etc
if (freq_type==2){
  horizon_sequence_dates <-horizon_sequence_dates [isWeekday(horizon_sequence_dates )]
}
#horiz_dates<-horizon_sequence_dates[1:hor]
horizon_end_date=horizon_sequence_dates[horizon] 
list(horizon_start_date=horizon_start_date,horizon_end_date=horizon_end_date)
}

#' Get horizon dates
#' @name get_horizon_dates
#' @title Returns dates for a specified horizon
#' @description seq.Date with length.out not currently working due to eclipse
#' @param train_end_date
#' @param horizon 
#' @param freq_type: 1 or 2 (weekdays). See type_to_descrip
#' @param freq_descrip: e.g. days, weeks
# @example dt2<-get_horizon_start_end(dt$train_end_date,horizon=30,2,"days")
# @example get_horizon_dates(dt2$horizon_start_date,dt2$horizon_end_date,2,"days")
#' @export
get_horizon_dates<-function(horizon_start_date, horizon_end_date, freq_type, freq_descrip){
  #horizon_sequence_dates<-seq(from=horizon_start_date, length.out=hor, by=1) #length.out not working
  #end_date<-horizon_start_date+15*365 # 15 years worth of values (e.g. 15*365, or 15*52 etc
  horizon_sequence_dates <- seq(from=horizon_start_date, to=horizon_end_date, by=freq_descrip)  ## set "by" to be days, weeks, months etc
  if (freq_type==2){
    horizon_sequence_dates <-horizon_sequence_dates [isWeekday(horizon_sequence_dates )]
  }
  horizon_sequence_dates
}

#' @name Mean_APE_function
#' @title Mean Average Percentage Error
#' @export
Mean_APE_function<-function(actual_values,forecast_values){
  df<-data.frame(A=actual_values,F=forecast_values)
  df_filtered<-df[df$A!=0 & df$F!=0,]
  actual_values_filtered<-df_filtered$A
  forecast_values_filtered<-df_filtered$F
  forecast_error<-abs(forecast_values_filtered - actual_values_filtered)
  APE<-forecast_error/actual_values_filtered * 100
  sum(APE) /length(actual_values_filtered)  ## MAPE
  #mean(forecast_error)
}

# sort input dataframe by Date column
# Precondition: timeseries is a dataframe with a column DATE_ID and a column TOTAL (numeric)
sort_data_frame_by_date<-function(DATE_ID_original,actuals_original){
  partial<-data.frame(DATE_ID_original,actuals_original,row.names=NULL)
  colnames(partial)<-c("date","value")
  sorted_partial<-partial[order(as.Date(partial$date, format="%d/%m/%Y")),]  
  list(date=sorted_partial$date,value=sorted_partial$value)
}

# Smooth data
# Function: calc_mean_indices - smoothing function
#calc_mean_indices<-function(index){
#  mean(actuals[(index-3):index])
#}

# guess likely time step difference in observations 
# http://stackoverflow.com/questions/19217729/check-the-frequency-of-time-series-data?rq=1
guess_period <- function(x) { 
  #average_period <- as.double( mean(diff(x$date)), units="days" )
  #average_period <- as.double( mean(diff(index(x))), units="days" )
  average_period <- as.double( mean(diff(x)), units="days" )
  
  difference <- abs(log( average_period / c(
    daily = 1,
    business_days = 7/5,
    weekly = 7,
    monthly = 30,
    quarterly = 365/4,
    annual = 365
  ) ) )
  #names( which.min( difference ) )
  which.min( difference ) 
}

is.wholenumber <-
  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

type_to_descrip<-function(spacing_type)
{
  switch(spacing_type,
         "1" = "days",
         "2" = "days",
         "3" = "weeks",
         "4" = "months",
         "5" = "quarters",
         "6" = "years")
}

type_to_annual_freq<-function(spacing_type)
{
  switch(spacing_type,
         "1" = 365.25, # accounding for leap year
         "2" = 260.89,  ## update.. this depends on the year!!! will need an average 365*5/7?
         "3" = 52,
         "4" = 12,
         "5" = 4,
         "6" = 1)
}
type_to_short_freq<-function(spacing_type)
{
  switch(spacing_type,
         "1" = 7, # accounding for leap year
         "2" = 5)  ## update.. this depends on the year!!! will need an average 365*5/7?
}

#' @name checkXreg
#' @title Check validity of input variables
#' @description Returns error flags indicating if there is an error in xreg and new_xreg

checkXreg<-function(xreg_select_no_dups,new_xreg_select_no_dups,DATE_ID,horiz_dates){
    
  # set default values for diagnostic flags
  variables_error_value=0  # value informs type of error
  variables_error_flag<-0 # assume no errors
  
  if(ncol(xreg_select_no_dups)!=ncol(new_xreg_select_no_dups)){
    variables_error_flag<-1  # set error
    variables_error_value<-4  # there is a redundant column in either xreg, or new_xreg 
  }else if (!identical(xreg_select_no_dups$date,DATE_ID) | !identical(new_xreg_select_no_dups$date,horiz_dates)){
    variables_error_flag<-1
    variables_error_value<-3 # dates do not match
  }
  
  list(variables_error_flag=variables_error_flag,variables_error_value=variables_error_value)
}

#' @name fill_missing_dates_timeseries
#' @title Interpolate missing dates in timeseries and insert associated value as 0
#' @param freq_type A value: 1 or 2(weekdays only)
#' @return returns a dataframe with columns: "Date","value","holiday", Date is a complete sequence of dates
#' @description Creates a dataframe of dates and values, and fills in (interpolates) any missing dates adding a 0 as the associated value.
#' @export
fill_missing_dates_timeseries<-function(DATE_ID, value, freq_type=1) {
  partial<-data.frame(DATE_ID, value,row.names=NULL)
  colnames(partial)<-c("date","value")
  # sort partial by date
  sorted_partial<-partial[order(as.Date(partial$date, format="%d/%m/%Y")),]
  first_value<-sorted_partial$date[1] # First time-series point
  start_date=as.Date(first_value,"%d/%m/%Y")
  end_val<-sorted_partial$date[nrow(sorted_partial)]  
  end_date<-as.Date(end_val,"%d/%m/%Y")
  # create full set of dates in time series
  full <- seq(from=start_date, to=end_date, by=1)  ## set "by" to be frequency
  if(freq_type==2){   # business days only
    full<-full[isWeekday(full)] ## only weekdays
  }
  #create full data frame, wtih missing dates as NAs
  time_series_df<-data.frame(Date=full, value=with(sorted_partial, value[match(full, date)]))
  holiday<-as.integer(!complete.cases(time_series_df$value) & !(time_series_df$Date %in% sorted_partial$date )) # create holiday as vector 1 if skipped value, 0 if value present
  #time_series_df$value[!complete.cases(time_series_df$value)==TRUE]<-0 #set to 0
  time_series_df$value[!complete.cases(time_series_df$value) & !(time_series_df$Date %in% sorted_partial$date )]<-0 # if date has just been added in this function
  time_series_df<-cbind(time_series_df,holiday)
  colnames(time_series_df)<-c("date","value","holiday")
  time_series_df
}

#' @name: fill_missing_dates_xreg
#' @title Interpolate missing dates and fill in values
#' @param: xreg_select has column "date"; freq_type must be set to 2 if weekdays only
#' @return: returns a dataframe with original columns (and names); Date is a complete sequence of dates
#' @description Fills in missing dates (interpolates) in xreg_data_frame, adding a 0 as the associated value.
#' @export
fill_missing_dates_xreg<-function(xreg_select, freq_type=1) {
  if(xreg_select==NULL){
    return(NULL)
  }
  partial<-xreg_select
  # sort partial by date
  sorted_partial<-partial[order(as.Date(partial$date, format="%d/%m/%Y")),]
  first_value<-sorted_partial$date[1] # First time-series point
  start_date=as.Date(first_value,"%d/%m/%Y")
  end_val<-sorted_partial$date[nrow(sorted_partial)]  # length first_training_dates gives the index of the last day of training data
  end_date<-as.Date(end_val,"%d/%m/%Y")
  # create full set of dates in time series
  full <- seq(from=start_date, to=end_date, by=1)  ## set "by" to be frequency
  if(freq_type==2){   # business days only
    full<-full[isWeekday(full)] ## only weekdays
  }
  
  # create data frame with full set of dates, and NAs where dates originally missing
  xreg_complete_NA<-data.frame(Date=full,xreg_select[match(full, xreg_select$date),-c(1)])   # added Date back in as gets lost
  
  #holiday_vector<-as.integer(!complete.cases(xreg_complete_NA[,2]) & !(xreg_complete_NA$Date %in% sorted_partial$date)) # create holiday as vector 1 if skipped value, 0 if value present
  
  # Function: col_dates: for an input column, sets the value to 0, if value is NA and date was missing
  col_dates<-function(col,dates) {col[!complete.cases(col) & !(dates %in% sorted_partial$date)]<-0 ; col}
  
  if (ncol(xreg_complete_NA)>2){
    xreg_complete_zero<-apply(xreg_complete_NA[,-c(1)],2, col_dates, dates=xreg_complete_NA$Date)
  }else{ # 1 column only
    xreg_complete_zero<-col_dates(xreg_complete_NA[,-c(1)],xreg_complete_NA$Date)
  }
  
  xreg_df<-cbind(xreg_complete_NA$Date,xreg_complete_zero)
  colnames(xreg_df)<-colnames(xreg_select)
  xreg_df
} # end fill_missing_dates_xreg
