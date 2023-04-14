#' Generate residences
#'
#' @param data a data frame of detection data.
#' @param cutoff the maximum allowable time difference between detections to
#' be considered a single residence event.
#' @param units the units of the cutoff. Options are "secs", "mins", "hrs", or "days".
#' @param ID a string of the name of the column in `data` that holds the tag or sample IDs
#' @param date a string of the name of the column in `data` that holds the date and time
#'
#' @return A data frame
#' @export
#'
#' @examples
#' residences(data=dat,cutoff=1,units="days")
residences<-function(data,ID,date,cutoff,units){
  # # See if there is already a function to convert units
  # if (units=="secs"){
  #   cutoff<-cutoff/60
  # }
  # else if (units=="hrs"){
  #   cutoff<-cutoff*60
  # }
  # else if (units=="days"){
  #   cutoff<-cutoff*60*24
  # }
  tag<-unique(na.omit(data[[ID]]))

  # Is there a way to automatically detect the date format? Or do something like
  # if (class(date)=), convert to POSIXct and UTC
  # Are there timezones in date/time fields?

  # res<-data[,]
  #
  # res
}
