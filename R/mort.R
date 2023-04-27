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
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param singles specifies if single detections (length of residence event = 0)
#' should be removed. Removing single detections is the most conservative method,
#' so chance or potentially invalid detections do not affect mortality estimates.
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potentia mortality or expelled tag was identified.
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





