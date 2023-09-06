#' Generate residence events
#' @description Generate residence events from passive acoustic telemetry data.
#' @details Note that a progress bar appears, based on how many of the unique tag
#' IDs have been processed. There will be a delay both before the progress bar
#' appears and after the progress bar has reached 100%, which may be substantial
#' depending on the size of the telemetry dataset.
#'
#' @param data a data frame of detection data.
#' @param cutoff the maximum allowable time difference between detections to
#' be considered a single residence event.
#' @param units the units of the cutoff. These will also be the units used to calculate the duration of the residence events.
#' Options are "secs", "mins", "hours", "days", and "weeks".
#' @param ID a string of the name of the column in `data` that holds the tag or sample IDs.
#' @param datetime a string of the name of the column in `data` that holds the date and time.
#' @param station a string of the name of the column in `data` that holds the station name or receiver location.
#' @param verbose option to display progress bar as residences are generated.
#' Default is TRUE.
#'
#' @return A data frame with one row for each residence event, including date/time of
#' residence start, date/time of residence end, and duration of residence event. All
#' input data fields (e.g., any name, location, or species information that was
#' included with detection data) will be retained.
#' @export
#' @importFrom stats na.omit
#' @importFrom utils setTxtProgressBar txtProgressBar
#'
#' @examples
#' head(detections)
#' res.events<-residences(data=detections[1:500,],ID="ID",station="Station.Name",
#' datetime="DateTimeUTC",cutoff=1,units="days",verbose=FALSE)
#' head(res.events)

residences<-function(data,ID,station,datetime,cutoff,units,verbose=TRUE){
  # Create list of unique IDs
  tag<-unique(na.omit(data[[ID]]))

  # Format date/times, if datetime column is not already formatted as POSIXt
  if (!is(data[[datetime]],"POSIXt")){
    # If data not already in POSIXt, assume that time zone is UTC
    try(data[[datetime]]<-as.POSIXct(data[[datetime]],tz="UTC"),silent=TRUE)
    if (!is(data[[datetime]],"POSIXt")){
      stop(paste(datetime,"is not of class POSIXt and is not in the format YYYY-mm-dd HH:MM:SS"))
    }
  }
  else {
    # If datetime already in POSIXt, but no time zone specified, assume UTC
    if (is.null(attributes(data[[datetime]])$tzone)){
      attributes(data[[datetime]])$tzone<-"UTC"
    }
  }

  # Get number of columns of data - to help fill in res later
  ncdata<-ncol(data)

  # Set up res
  res<-cbind(data[0,],ResidenceEnd=as.POSIXct(as.character()))

  if (verbose==TRUE){
    pb<-txtProgressBar(1,length(tag),style=3)
  }
  for (i in 1:length(tag)){
    # Subset by ID
    res.sub<-data[data[[ID]]==tag[i],]
    # Order by time
    res.sub<-res.sub[order(res.sub[[datetime]]),]

    j<-1
    repeat{
      # Begin new residence and fill in preliminary info
      k<-nrow(res)+1
      res[k,]<-NA
      res[k,1:ncdata]<-res.sub[j,]
      if (nrow(res.sub)>j){
        repeat{
          # If End time has already been assigned, then break repeat
          # and begin new residence
          # There will be no End time if this is the first time
          # going through the repeat
          if (!(is.na(res$ResidenceEnd[k]))){
            break
          }
          # If the same station name and within res.cutoff, move to next row
          else if ((as.numeric(difftime(res.sub[[datetime]][j+1],res.sub[[datetime]][j],units=units))<=cutoff)&
                   (res.sub[[station]][j+1]==res.sub[[station]][j])){
            j<-j+1
            # If the next row is the last row, then break
            if (nrow(res.sub)==j){
              res$ResidenceEnd[k]<-res.sub[[datetime]][j]
              j<-j+1
              break
            }
          }
          # If difftime before the next row is greater than res.cutoff,
          # then break
          else if (as.numeric(difftime(res.sub[[datetime]][j+1],res.sub[[datetime]][j],units=units))>cutoff){
            res$ResidenceEnd[k]<-res.sub[[datetime]][j]
            j<-j+1
            break
          }
          # If different station name and less than res.cutoff
          else {
            res$ResidenceEnd[k]<-res.sub[[datetime]][j]
            j<-j+1
            break
          }
        }
      }
      # End residence if end of detection record
      else if (nrow(res.sub)==j) {
        res$ResidenceEnd[k]<-res.sub[[datetime]][j]
        break
      }
      # Failsafe in case j<-j+1 when at end of detection record
      if (nrow(res.sub)<j){
        break
      }
    }
    if (verbose==TRUE){
      setTxtProgressBar(pb,i)
    }
  }
  colnames(res)[colnames(res)==datetime]<-"ResidenceStart"
  attributes(res$ResidenceStart)$tzone<-attributes(data[[datetime]])$tzone
  attributes(res$ResidenceEnd)$tzone<-attributes(data[[datetime]])$tzone

  # Calculate time difference
  res$ResidenceLength<-difftime(res$ResidenceEnd,res$ResidenceStart,units=units)
  res$ResidenceLength<-as.numeric(res$ResidenceLength)
  colnames(res)[colnames(res)=="ResidenceLength"]<-paste0("ResidenceLength.",units)

  res
}
