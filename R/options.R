#### Working backwards ####
#' Shift start time of potential mortalities earlier
#' @description Shift the start time of potential mortalities earlier, if
#' station/location has not changed.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration. Residence events should be
#' continuous (i.e., not subset by season).
#' @param morts a dataframe containing potential mortalities. Must use
#' #' the same column names and in the same order as `data`.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param season a dataframe with the start and end times of the season(s) or
#' time period(s) of interest.
#' @param stnchange a dataframe with the start time and location of the most
#' recent station or location change. Must use the same column names and in
#' the same order as `data`.
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potential mortality or expelled tag was identified.
#' Returns the input `morts` dataframe if no potential mortalities are shifted
#' earlier.
#' All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#'
#' @export
#'
#' @examples
#' \dontrun{backwards(data=data,morts=morts,ID="TagID",station="Receiver",
#' res.start="ResidencesStart",stnchange=stn.change)}
backwards<-function(data,morts,ID,station,res.start,season=NULL,
                    stnchange=NULL){
  # If morts was not based on season, then shift records earlier to station change
  if (is.null(season)&
      !is.null(stnchange)){
    for (i in 1:nrow(morts)){
      j<-which(stnchange[[ID]]==morts[[ID]][i])
      if (morts[[res.start]][i]>stnchange[[res.start]][j]){
        morts[i,]<-stnchange[j,]
      }
    }
  }
  else {
    if (nrow(morts)>0){
      for (i in 1:nrow(morts)){
        res.temp<-data[data[[ID]]==morts[[ID]][i]&
                         data[[res.start]]<=morts[[res.start]][i],]
        # Start at end and work backwards, looking for station change
        j<-nrow(res.temp)
        repeat{
          if (j>=2){
            if (res.temp[[station]][j]==res.temp[[station]][j-1]){
              j<-j-1
            }
            else {break}
          }
          else {break}
        }
        # If the identified station change is earlier than the
        # previously identified mortality
        if (res.temp[[res.start]][j]<morts[[res.start]][i]){
          morts[i,]<-res.temp[j,]
        }
      }
    }
  }
  morts
}

#### ddd (dead drift direction) ####
#' Dead drift
#' @description Identifies sequential residence events where detected movement
#' between stations may be due to drifting of an expelled tag or dead animal.
#'
#' @param data a data frame of residence events. Residence events must include
#' tag ID, location name, start time, and end time.
#' @param format the format used to generate the residence events in `data`.
#' Options are "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then
#' user must specify `ID`, `station`, `res.start`, and `res.end`.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `format="manual"`.
#' @param ddd a dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param from.station a string of the name of the column in `data` that contains
#' the station/location names where drifting detections may start from.
#' @param to.station a string of the name of the column in `data` that contains
#' the station/location names where drifting detections may move to.
#' @param cutoff the maximum allowable time difference between detections to be
#' considered a single residence event. Default is `NULL`.
#' @param units the units of the cutoff. Options are "secs", "mins", "hours",
#' "days", and "weeks".
#'
#' @return A data frame with one row for each residence event. Format is the
#' same as the input residence events, but events that may be due to dead drift
#' are combined into single residence events.
#' @export
#'
#' @examples
#' \dontrun{drift(data=data,format="manual",ID="TagID",station="Receiver",
#' res.start="ResidenceStart",res.end="ResidenceEnd",ddd=driftstations,
#' from.station="FrStation",to.station="ToStation")}
drift<-function(data,format,ID,station,res.start,res.end,
                residences,ddd,from.station,to.station,
                cutoff=NULL,units=NULL){

  # Could make this check be a function
  if (all(class(data[[res.start]])!="POSIXt")){
    try(data[[res.start]]<-as.POSIXct(data[[res.start]],tz="UTC",silent=TRUE))
    if (all(class(data[[res.start]])!="POSIXt")){
      stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (all(class(data[[res.end]])!="POSIXt")){
    try(data[[res.end]]<-as.POSIXct(data[[res.end]],tz="UTC",silent=TRUE))
    if (all(class(data[[res.end]])!="POSIXt")){
      stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }

  # Convert station to list (to hold all stations, in order)
  data[[station]]<-as.list(data[[station]])

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  res.drift<-data[0,]

  for (i in 1:length(tag)){
    res.temp<-data[data[[ID]]==tag[i],]
    if (nrow(res.temp)>1&
        any(res.temp[[station]][1:(nrow(res.temp)-1)] %in% ddd[[from.station]])){
      j<-which(res.temp[[station]] %in% ddd[[from.station]])
      # Exclude any where j is the last record
      if (nrow(res.temp) %in% j){
        j<-j[-which(j==nrow(res.temp))]
      }
      del<-as.numeric()
      if (is.null(cutoff)){
        for (k in 1:length(j)){
          if (res.temp[[station]][j[k]+1] %in%
              ddd[[to.station]][ddd[[from.station]]==res.temp[[station]][[j[k]]][length(res.temp[[station]][[j[k]]])]]){
            res.temp[[res.start]][j[k+1]]<-res.temp[[res.start]][j[k]]
            res.temp[[station]][[j[k+1]]]<-append(res.temp[[station]][[j[k]]],res.temp[[station]][[j[k+1]]])
            # res.temp[[station]][j[k+1]]<-res.temp[[station]][j[k]]
            del<-c(del,j[k])
          }
        }
      }
      else {
        for (k in 1:length(j)){
          if ((res.temp[[station]][j[k]+1] %in%
               ddd[[to.station]][ddd[[from.station]]==res.temp[[station]][j[k]]])&
              difftime(res.temp[[res.start]][j[k]+1],
                       res.temp[[res.end]][j[k]],
                       units=units)<cutoff){
            res.temp[[res.start]][j[k+1]]<-res.temp[[res.start]][j[k]]
            res.temp[[station]][j[k+1]]<-res.temp[[station]][j[k]]
            del<-c(del,j[k])
          }
        }
      }
      if (length(del)>0){
        res.temp<-res.temp[-del,]
      }
    }
    res.drift[(nrow(res.drift)+1):(nrow(res.drift)+nrow(res.temp)),]<-res.temp[,]
  }

  res.drift[[residences]]<-difftime(res.drift[[res.end]],
                                    res.drift[[res.start]],
                                    units=units)

  res.drift
}




#### Seasonal ####
