#### Actel notes ####
# Output of explore function
# Residence events are called movement events
# And are saved in a list, under valid.movements and then the transmitter (including code space)
# Name of the ID/transmitter is not in the file (but is the name of the list)
# Units are always in minutes
# Times are in local time

#### VTrack notes ####
# Residences events are called residences
# And are saved in a list, under residences
# Units are always in seconds

#### glatos notes ####
# Residence events are called detection events
# And are saved in a dataframe
# Units are always in seconds



#' Identify potential mortalities or expelled tags
#' @description Identifies potential mortalities or expelled tags from passive
#' acoustic telemetry data. Mortalities are identified based on thresholds
#' derived from the dataset itself.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration. Residence events must also
#' include end time if `season` is provided.
#' @param format the format used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify `ID`, `station`, `res.start`, `res.end`, `residences`, and `units`.
#' @param units units of the duration of the residence events in `data`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param method the method to be used in flagging mortalities. Options are
#' "last", "any", "cumulative", or "all"
#' @param season a dataframe with start and end dates of the season(s) of interest
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
#' @param singles specifies if single detections (length of residence event = 0)
#' should be removed. Removing single detections is the most conservative method,
#' so chance or potentially invalid detections do not affect mortality estimates.
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potential mortality or expelled tag was identified.
#' All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
#' \dontrun{mort(data=res.events,format="manual",units="days",residences="ResidenceLength")}
morts<-function(data,format="mort",ID,station,res.start="auto",res.end="auto",
               method="all",units="auto",residences="auto",season=NULL,
               singles=FALSE){
  # if (format=="mort"){
  #   units=sub("ResidenceLength.","",colnames(data)[grep("ResidenceLength",colnames(data))])
  # }
  # else if (format=="actel"){
  #   units="mins"
  # }
  # else if (format=="glatos"){
  #   units="secs"
  # }
  # else if (format=="vtrack"){
  #   units="secs"
  # }
  # else if (format=="manual"){
  #   # Check that units were provided
  # }
  # # if (is.null(season)){
  # #
  # # }
  # # else {
  # #
  # # }
  #

  ### Add in a check that if any parameters are missing for manual method,
  # then give an error

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

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  ### This is where ddd should be incorporated
  stn.change=stationchange(data=data,ID=ID,station=station,res.start=res.start,
                           residences=residences,singles=singles)

  morts<-data[0,]

  # Most recent residence longer than max residence before station change
  if (method %in% c("last","any","all")){
    # Identify the longest residence followed by a station change
    max.res<-max(resmax(data=data,ID=ID,station=station,res.start=res.start,
                        residences=residences,stnchange=stn.change)[[residences]])
    if (method=="last"){
      # Note that not run for "all" or "any", because "last" is also captured with "any"
      for (i in 1:length(tag)){
        if (data[[residences]][data[[ID]]==tag[i]&
                               data[[res.start]]==max(data[[res.start]][data[[ID]]==tag[i]])]>max.res){
          morts[nrow(morts)+1,]<-data[data[[ID]]==tag[i]&
                                        data[[res.start]]==max(data[[res.start]][data[[ID]]==tag[i]]),]
        }
      }
    }
    if (method %in% c("any","all")){
      # Identify all the residences longer than max.res
      # If they are after the last station change, then add them to morts
      for (i in 1:length(tag)){
        res.temp<-data[data[[ID]]==tag[i],]
        j<-which(res.temp[[residences]]>max.res&
                   res.temp[[res.start]]>1)
        # If there are any long residences after the station change
        if (length(j)>0){
          # Find which one is the earliest
          k<-which(res.temp[[res.start]][j]==min(res.temp[[res.start]][j]))
          # Add to morts
          morts[nrow(morts)+1,]<-res.temp[j[k],]
        }
      }
    }
  }

  if (method %in% c("cumulative","all")){
    # Identify the longest cumulative residence followed by a station change
    max.rescml<-max(resmaxcml(data=data,ID=ID,station=station,res.start=res.start,
                              res.end=res.end,residences=residences,units=units,
                              stnchange=stn.change)[[residences]])
    for (i in 1:nrow(stn.change)){
      res.temp<-data[data[[ID]]==stn.change[[ID]][i],]
      # If the cumulative residence at the most recent station is longer than the
      # threshold of max.rescml
      if (difftime(res.temp[[res.end]][res.temp[[res.end]]==max(res.temp[[res.end]])],
                   stn.change[[res.start]][i],units=units)>max.rescml){
        # If the ID is already in morts
        if (stn.change[[ID]][i] %in% morts[[ID]]){
          # Identify which row
          j<-which(morts[[ID]]==stn.change[[ID]][i])
          # If the residence currently in res.morts ocurred later
          # than the cumulative residence period
          if (morts[[res.start]][j]>stn.change[[res.start]][i]){
            # Adjust the start time to the earlier date
            morts[j,]<-stn.change[i,]
          }
        }
        # If the ID is not yet in morts
        else {
          morts[nrow(morts)+1,]<-stn.change[i,]
        }
      }
    }
  }

  morts

}




#' Identify potential mortalities from infrequent detections
#' @description Identifies potential mortalities or expelled tags from infrequent
#' detections in passive acoustic telemetry data. Mortalities are identfied based
#' on a user-defined threshold and timeframe.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration. Residence events must also
#' include end time if `season` is provided.
#' @param format the format used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify `ID`, `station`, `res.start`, `res.end`, `residences`, and `units`.
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
#' @param residences residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param units Units of the duration of the residence events in `data`. Options are "secs",
#' "mins", "hours", "days", and "weeks".
#' @param method a character string of the threshold method. Options are "recent"
#' and "defined". If "recent", must specify `recent.period` and `recent.units`.
#' If "defined", must specify `start` and `end`.
#' @param threshold the minimum summed duration of residence events for an animal
#' to be considered alive. Units must be the same as the units of `residences`.
#' @param threshold.units the units of `threshold`. Options are "secs",
#' "mins", "hours", "days", and "weeks". If `NULL`, assumed to be the same as
#' `units`.
#' @param recent.period the length of the period of time in which an animal must
#' be detected longer than the `threshold` to be considered alive. The period
#' ends with the most recent detection of a given animal.
#' @param recent.units the units of recent.period. Options are "secs",
#' "mins", "hours", "days", and "weeks".
#' @param start character string with the start of the user-defined time period to search for infrequent
#' detections if `method="defined"`. Must be in the format
#' YYYY-mm-dd HH:MM:SS. The time zone is the same as `res.start` or
#' assumed to be UTC if no time zone is defined for `res.start`.
#' @param end character string with the end of the user-defined time period to search for infrequent
#' detections if `method="defined"`. Must be in the format
#' YYYY-mm-dd HH:MM:SS. The time zone is the same as `res.start` or
#' assumed to be UTC if no time zone is defined for `res.start`.
#' @param morts a dataframe containing potential mortalities. The dataframe must
#' have the same columns and in the same order as `data`.
#' @param replace if `morts` specified and an animal with infrequent detections
#' is already in `morts`, the record in `morts` will be replaced if `TRUE`.
#' Default is `FALSE`.
#'
#' @details Example of `method="recent"`: if `threshold=10` and `recent.period=1` and
#' `recent.units="years"`, an animal will be flagged as a potential mortality if
#' it was detected for less than 10 minutes within a year, ending with the most
#' recent detection.
#'
#' Example of `method="defined"`: if `threshold=10` and `start="2019-10-01"` and `end="2020-06-01"`, an animal
#' will be flagged as a potential mortality if it was detected for less than 10
#' minutes between 01 October 2019 and 01 June 2020.
#'
#' @return if `morts=NULL`, a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potential mortality or expelled tag was identified.
#' If `morts` is specified, any potential mortalities will be added to existing
#' `morts` dataframe.
#' All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
#' ## Recent example
#' \dontrun{infrequent(data=data,format="manual",ID="TagID",station="Receiver",
#' res.start="ResStart",res.end="ResEnd",residences="ResidenceLength.days",
#' units="days",method="recent",threshold=0.5,recent.period=1,
#' recent.units="years")}
#' ## User-defined example
#' \dontrun{infrequent(data=data,format="manual",ID="TagID",station="Receiver",
#' res.start="ResStart",res.end="ResEnd",residences="ResidenceLength.days",
#' units="days",method="defined",threshold=0.5,start="2019-10-01",
#' end="2020-06-01")}


infrequent<-function(data,format,ID,station,res.start,res.end,residences,units,
                     method,threshold,threshold.units=NULL,recent.period=NULL,recent.units=NULL,
                     start=NULL,end=NULL,morts=NULL,replace=FALSE){
  #### Need to add in if want to move backwards
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

  #### If want to be able to specify units, will need to write unit functions
  # if (!is.null(threshold.units)&
  #     units!=threshold.units){
  #   # Convert threshold.units to units
  #
  # }

  tag<-unique(data[[ID]])

  # If there is already a morts dataframe, remove those IDs from tag
  if (!is.null(morts)){
    tag<-tag[!(tag %in% morts[[ID]])]
    inf.morts<-morts
  }
  else {inf.morts<-data[0,]}

  if (method=="recent"){
    # Give warning if recent.period and recent.units are undefined
    if (is.null(recent.period)){
      stop("recent.period is not specified")
    }
    if (is.null(recent.units)){
      stop("recent.units is not specified")
    }
    for (i in 1:length(tag)){
      if (is.null(morts)|
          (!is.null(morts)&
           (replace==TRUE|
            !(tag[i] %in% morts)))){
        res.temp<-data[data[[ID]]==tag[i]&
                         difftime(data[[res.end]][data[[res.end]]==max(data[[res.end]])],
                                  data[[res.end]],units=recent.units)<recent.period,]
        if (nrow(res.temp)>=1&
            length(unique(res.temp[[station]]))==1&
            sum(res.temp[[residences]])<threshold){
          inf.morts[nrow(inf.morts)+1,]<-res.temp[1,]
        }
      }
    }
  }

  if (method=="defined"){
    # Give warning if start and end are undefined
    if (is.null(start)){
      stop("start is not specified")
    }
    if (is.null(end)){
      stop("end is not specified")
    }
    # Convert start and end to POSIXt
    try(start<-as.POSIXct(start,tz=attributes(data[[res.start]])$tzone),silent=TRUE)
    if (all(class(start)!="POSIXt")){
      stop("start is not in the format YYYY-mm-dd HH:MM:SS")
    }
    try(end<-as.POSIXct(end,tz=attributes(data[[res.start]])$tzone),silent=TRUE)
    if (all(class(end)!="POSIXt")){
      stop("end is not in the format YYYY-mm-dd HH:MM:SS")
    }
    for (i in 1:length(tag)){
      if (is.null(morts)|
          (!is.null(morts)&
           (replace==TRUE|
            !(tag[i] %in% morts)))){
        res.temp<-data[data[[ID]]==tag[i]&
                         data[[res.end]]>=start&
                         data[[res.start]]<=end,]
        if (nrow(res.temp)>=1&
            length(unique(res.temp[[station]]))==1&
            sum(res.temp[[residences]])<threshold){
          inf.morts[nrow(inf.morts)+1,]<-res.temp[1,]
        }
      }
    }
  }

  inf.morts
}





