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
#' should be retained. Default is `TRUE`. Note that if single detections are
#' removed (`singles=FALSE`), `backwards` will also not include single detections.
#' @param morts.prev a dataframe containing potential mortalities. The dataframe must
#' have the same columns and in the same order as `data`.
#' @param backwards option to examine residence events prior to the one that was
#' flagged as a potential mortality. If prior residence events are at the same
#' station/location as the flagged event, the time of the potential mortality is shifted
#' earlier. Note that if `backwards=TRUE`, then the output of `method="last"` is
#' the same as `method="any"`.
#' @param drift option to account for potential drifting in identifying
#' thresholds and/or mortalities. Options are "none", "threshold",
#' "morts", "both". Default is "none".
#' @param ddd a dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param from.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may start from. Must
#' be identical to the station/location names in `data`.
#' @param to.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may move to. Must
#' be identical to the station/location names in `data`.
#' @param drift.cutoff the maximum allowable time difference between detections to be
#' considered a single residence event. Recommended to be the same as used
#' to generate residence events in `data`.
#' @param drift.units the units of the cutoff. Options are "secs", "mins", "hours",
#' "days", and "weeks". Recommended to be the same as used to generate
#' residence events in `data`.
#' @param season.start the start date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param season.end the end date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param season.overlap option to include residence events that overlap either the
#' beginning or the end of the period of interest. If `TRUE`, the full overlapping
#' residence events will be retained. If `FALSE`, only the portion of the
#' residence events that is within the period of interest will be retained,
#' and `residences` will be recalculated, using specified `units`.
#' Default is `TRUE`
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potential mortality or expelled tag was identified.
#' All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @import methods
#' @importFrom methods is
#' @export
#'
#' @examples
#' \dontrun{mort(data=res.events,format="manual",units="days",residences="ResidenceLength")}
morts<-function(data,format="mort",ID,station,res.start="auto",res.end="auto",
               method="all",units="auto",residences="auto",
               singles=TRUE,backwards=FALSE,drift="none",ddd=NULL,
               from.station=NULL, to.station=NULL,drift.cutoff=NULL,
               drift.units=NULL,season.start=NULL,season.end=NULL,
               season.overlap=TRUE,morts.prev=NULL){
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

  if (!is(data[[res.start]],"POSIXt")){
    try(data[[res.start]]<-as.POSIXct(data[[res.start]],tz="UTC",silent=TRUE))
    if (!is(data[[res.start]],"POSIXt")){
      stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is(data[[res.end]],"POSIXt")){
    try(data[[res.end]]<-as.POSIXct(data[[res.end]],tz="UTC",silent=TRUE))
    if (!is(data[[res.end]],"POSIXt")){
      stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }

  if (!is.null(season.start)|
      !is.null(season.end)){
    data.full<-data
    data<-season(data=data,res.start=res.start,res.end=res.end,
                 residences=residences,units=units,season.start=season.start,
                 season.end=season.end,overlap=season.overlap)
  }

  if (any(is.na(data[[station]]))){
    stop("one or more station names is NA")
  }

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  if (!is.null(morts.prev)){
    morts<-morts.prev
    if (backwards==TRUE){
      nm<-nrow(morts)
      new.morts<-as.numeric()
    }
  }
  else {morts<-data[0,]}

  if (drift=="none"){
    data.morts<-data
    if (!is.null(season.start)){
      data.full.morts<-data.full
    }
    stn.change<-stationchange(data=data,ID=ID,station=station,res.start=res.start,
                             residences=residences,singles=singles)
    stn.change.morts<-stn.change
  }
  else if (drift=="threshold"){
    data.morts<-data
    if (!is.null(season.start)){
      data.full.morts<-data.full
    }
    if (!is.null(drift.cutoff)){
      data.morts<-drift(data=data,ID=ID,station=station,
                        res.start=res.start,res.end=res.end,residences=residences,
                        units=units,
                        ddd=ddd,from.station=from.station,to.station=to.station,
                        cutoff.units=drift.units,cutoff=drift.cutoff)
    }
    else {
      data.morts<-drift(data=data,ID=ID,station=station,
                        res.start=res.start,res.end=res.end,residences=residences,
                        units=units,
                        ddd=ddd,from.station=from.station,to.station=to.station)
    }
    stn.change<-stationchange(data=data,ID=ID,station=station,res.start=res.start,
                             residences=residences,singles=singles)
    stn.change.morts<-stationchange(data=data.morts,ID=ID,station=station,res.start=res.start,
                                   residences=residences,singles=singles)
  }
  else if (drift=="morts"){
    data.morts<-drift(data=data,ID=ID,station=station,
                      res.start=res.start,res.end=res.end,residences=residences,
                      units=units,
                      ddd=ddd,from.station=from.station,to.station=to.station)
    if (!is.null(drift.cutoff)){
      data.resmax<-drift(data=data,ID=ID,station=station,
                         res.start=res.start,res.end=res.end,residences=residences,
                         units=units,
                         ddd=ddd,from.station=from.station,to.station=to.station,
                         cutoff.units=drift.units,cutoff=drift.cutoff)
    }
    else {data.resmax<-data.morts}
    drift.resmax<-FALSE
    if (!is.null(season.start)){
      data.full<-drift(data=data.full,ID=ID,station=station,
                       res.start=res.start,res.end=res.end,residences=residences,
                       units=units,
                       ddd=ddd,from.station=from.station,to.station=to.station)
    }
    sc1<-stationchange(data=data.morts,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
    sc2<-stationchange(data=data.full,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
  }
  else if (drift=="both"){
    if (is.null(drift.cutoff)){
      data<-drift(data=data.full,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station)
    }
    else {
      data<-drift(data=data.full,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station,
                  cutoff.units=drift.units,cutoff=drift.cutoff)
    }
    data.morts<-data
    if (!is.null(season.start)){
      if (is.null(drift.cutoff)){
        data.full.morts<-drift(data=data.full,ID=ID,station=station,
                               res.start=res.start,res.end=res.end,residences=residences,
                               units=units,
                               ddd=ddd,from.station=from.station,to.station=to.station)
      }
      else {
        data.full.morts<-drift(data=data.full,ID=ID,station=station,
                               res.start=res.start,res.end=res.end,residences=residences,
                               units=units,
                               ddd=ddd,from.station=from.station,to.station=to.station,
                               cutoff.units=drift.units,cutoff=drift.cutoff)
      }
    }
    stn.change<-stationchange(data=data,ID=ID,station=station,res.start=res.start,
                             residences=residences,singles=singles)
    stn.change.morts<-stn.change
    if (is(morts[[station]],"list")){
      morts[[station]]<-as.list(morts[[station]])
    }
  }
  else {stop("drift is not specified correctly")}

  # Most recent residence longer than max residence before station change
  if (method %in% c("last","any","all")){
    # Identify the longest residence followed by a station change
    max.res<-max(resmax(data=data.resmax,ID=ID,station=station,res.start=res.start,
                        residences=residences,stnchange=sc1,
                        drift=drift.resmax)[[residences]])
    if (method=="last"){
      # Note that not run for "all" or "any", because "last" is also captured with "any"
      for (i in 1:length(tag)){
        # Only run if ID not already in morts
        # If it is already in morts, it would be from infrequent function, which
        # would identify a time earlier or equal to the most recent detection
        if (!(tag[i] %in% morts[[ID]])){
          if (data.morts[[residences]][data.morts[[ID]]==tag[i]&
                                       data.morts[[res.start]]==max(data.morts[[res.start]][data.morts[[ID]]==tag[i]&
                                                                                            data.morts[[station]]!="Break"])]>max.res){
            morts[nrow(morts)+1,]<-data.morts[data.morts[[ID]]==tag[i]&
                                                data.morts[[res.start]]==max(data.morts[[res.start]][data.morts[[ID]]==tag[i]&
                                                                                                       data.morts[[station]]!="Break"]),]
          }
        }
      }
    }
    if (method %in% c("any","all")){
      # Identify all the residences longer than max.res
      # If they are during or after the last station change, then add them to morts
      for (i in 1:length(tag)){
        res.temp<-data.morts[data.morts[[ID]]==tag[i],]
        j<-which(res.temp[[residences]]>max.res&
                   res.temp[[res.start]]>=sc2[[res.start]][sc2[[ID]]==tag[i]])
        # If there are any long residences after the station change
        if (length(j)>0){
          # Find which one is the earliest
          k<-which(res.temp[[res.start]][j]==min(res.temp[[res.start]][j]))
          if (length(k)>1){
            k<-min(k)
          }
          if (tag[i] %in% morts[[ID]]){
            m<-which(morts[[ID]]==tag[i])
            if (res.temp[[res.start]][j[k]]<morts[[res.start]][m]){
              morts[m,]<-res.temp[j[k],]
            }
            if (backwards==TRUE&
                !is.null(morts.prev)){
              new.morts<-c(new.morts,m)
            }
          }
          else {
            # Add to morts
            morts[nrow(morts)+1,]<-res.temp[j[k],]
          }
        }
      }
    }
  }

  if (method %in% c("cumulative","all")){
    # Identify the longest cumulative residence followed by a station change
    max.rescml<-max(resmaxcml(data=data.morts,ID=ID,station=station,res.start=res.start,
                              res.end=res.end,residences=residences,units=units,
                              stnchange=sc1)[[residences]],na.rm=TRUE)
    for (i in 1:nrow(sc2)){
      res.temp<-data.morts[data.morts[[ID]]==sc2[[ID]][i],]
      res.temp<-res.temp[order(res.temp[[res.start]]),]
      repeat {
        if (res.temp[[station]][nrow(res.temp)]=="Break"){
          res.temp<-res.temp[-nrow(res.temp),]
        }
        else {break}
      }
      if (any(res.temp[[station]]=="Break")){
        # Need two values, and determine which is lower
        # difftime between end of most recent residence and station change (sc2)
        dt1<-difftime(res.temp[[res.end]][nrow(res.temp)],
                      sc2[[res.start]][i],
                      units=units)
        # difftime between end of most recent residence and most recent break
        k<-(which(res.temp[[res.start]]==max(res.temp[[res.start]][res.temp[[station]]=="Break"])))+1
        dt2<-difftime(res.temp[[res.end]][nrow(res.temp)],
                      res.temp[[res.start]][k],
                      units=units)
        comp<-min(dt1,dt2)
      }
      else {
        comp<-difftime(res.temp[[res.end]][nrow(res.temp)],
                           sc2[[res.start]][i],
                           units=units)
      }
      # If the cumulative residence at the most recent station is longer than the
      # threshold of max.rescml
      if (comp>max.rescml){
        # If the ID is already in morts
        if (sc2[[ID]][i] %in% morts[[ID]]){
          # Identify which row
          j<-which(morts[[ID]]==sc2[[ID]][i])
          # If the residence currently in res.morts ocurred later
          # than the cumulative residence period
          if (morts[[res.start]][j]>sc2[[res.start]][i]){
            # Adjust the start time to the earlier date
            morts[j,]<-sc2[i,]
          }
        }
        # If the ID is not yet in morts
        else {
          morts[nrow(morts)+1,]<-sc2[i,]
        }
      }
    }
  }

  if (backwards==TRUE){
    if (!is.null(morts.prev)){
      if (nrow(morts)>nm){
        new.morts<-unique(c(new.morts,seq((nm+1),nrow(morts),1)))
        for (i in 1:length(new.morts)){
          morts[i,]<-backwards(data=data.morts,morts=morts[i,],ID=ID,station=station,
                               res.start=res.start,stnchange=sc2)
        }
      }
    }
    else {
      morts<-backwards(data=data.full.morts,morts=morts,ID=ID,station=station,
                         res.start=res.start,stnchange=sc2)
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
#' @param morts.prev a dataframe containing potential mortalities. The dataframe must
#' have the same columns and in the same order as `data`.
#' @param replace if `morts` specified and an animal with infrequent detections
#' is already in `morts`, the record in `morts` will be replaced if `TRUE`.
#' Default is `FALSE`.
#' @param backwards option to examine residence events prior to the one that was
#' flagged as a potential mortality. If prior residence events are at the same
#' station/location as the flagged event, the time of the potential mortality is shifted
#' earlier.
#' @param ddd an optional dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param from.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may start from. Must
#' be identical to the station/location names in `data`.
#' @param to.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may move to. Must
#' be identical to the station/location names in `data`.
#' @param drift.cutoff the maximum allowable time difference between detections to be
#' considered a single residence event. Recommended to be the same as used
#' to generate residence events in `data`.
#' @param drift.units the units of the cutoff. Options are "secs", "mins", "hours",
#' "days", and "weeks". Recommended to be the same as used to generate
#' residence events in `data`.
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
                     start=NULL,end=NULL,morts.prev=NULL,replace=FALSE,backwards=FALSE,
                     ddd=NULL,from.station=NULL,to.station=NULL,
                     drift.cutoff=NULL,drift.units=NULL){

  if (!is(data[[res.start]],"POSIXt")){
    try(data[[res.start]]<-as.POSIXct(data[[res.start]],tz="UTC",silent=TRUE))
    if (!is(data[[res.start]],"POSIXt")){
      stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is(data[[res.end]],"POSIXt")){
    try(data[[res.end]]<-as.POSIXct(data[[res.end]],tz="UTC",silent=TRUE))
    if (!is(data[[res.end]],"POSIXt")){
      stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }

  if (any(data[[station]]=="Break")){
    warning("Either a station name was 'Break' or data included seasonal
            breaks. Breaks were removed.")
  }

  #### If want to be able to specify units, will need to write unit functions
  # if (!is.null(threshold.units)&
  #     units!=threshold.units){
  #   # Convert threshold.units to units
  #
  # }
  k<-which(data[[station]]=="Break")
  if (length(k)>0){
    data<-data[-k,]
  }
  if (!is.null(ddd)){
    if (is.null(drift.cutoff)){
      data.og<-data
      data<-drift(data=data,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station)
    }
    else {
      data.og<-data
      data<-drift(data=data,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station,
                  cutoff=drift.cutoff,cutoff.units=drift.units)
    }
  }

  tag<-unique(data[[ID]])

  # If there is already a morts dataframe and replace=FALSE, remove those IDs from tag
  if (!is.null(morts.prev)){
    if (replace==FALSE){
      tag<-tag[!(tag %in% morts.prev[[ID]])]
    }
    inf.morts<-morts.prev
    if (backwards==TRUE){
      new.morts<-as.numeric()
    }
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
      if (is.null(morts.prev)|
          (!is.null(morts.prev)&
           (replace==TRUE|
            !(tag[i] %in% morts.prev[[ID]])))){
        res.temp<-data[data[[ID]]==tag[i],]
        res.temp<-res.temp[order(res.temp[[res.start]]),]
        j<-min(which(difftime(res.temp[[res.end]][nrow(res.temp)],
                          res.temp[[res.end]],
                          units=recent.units)<=recent.period))
        # if (length(j)>0){
        #   res.temp<-res.temp[-j,]
        # }
        if (is.null(ddd)){
          if (nrow(res.temp)>=1&
              length(unique(res.temp[[station]][j:nrow(res.temp)]))==1&
              sum(res.temp[[residences]])<threshold){
            if (tag[i] %in% inf.morts[[ID]]){
              j<-which(inf.morts[[ID]]==tag[i])
              if (res.temp[[res.start]][1]<inf.morts[[res.start]][j]){
                inf.morts[j,]<-res.temp[1,]
                if (!is.null(morts.prev)&backwards==TRUE){
                  new.morts<-c(new.morts,j)
                }
              }
            }
            else {
              inf.morts[nrow(inf.morts)+1,]<-res.temp[1,]
            }
          }
        }
        else {
          res.temp.drift<-drift(data=res.temp[j:nrow(res.temp),],ID=ID,station=station,
                          res.start=res.start,res.end=res.end,
                          residences=residences,units=units,ddd=ddd,
                          from.station=from.station,to.station=to.station)
          if (nrow(res.temp.drift)>1){
            # Then run backwards
            rb<-backwards(data=res.temp.drift,morts=res.temp.drift[nrow(res.temp.drift),],
                          ID=ID,station=station,res.start=res.start)
            k<-which(res.temp.drift[[res.start]]==rb[[res.start]])
            if (k==1){
              if (j>1){
                # Run drift on whole dataset
                res.temp<-drift(data=res.temp,ID=ID,station=station,
                                res.start=res.start,res.end=res.end,
                                residences=residences,units=units,ddd=ddd,
                                from.station=from.station,to.station=to.station)
                # Run backwards on dataset, starting with j, and add to inf.morts
                inf.morts[nrow(inf.morts)+1,]<-backwards(data=res.temp,
                                                         morts=res.temp[j,],
                                                         ID=ID,station=station,res.start=res.start)
              }
              else {
                inf.morts[nrow(inf.morts)+1,]<-res.temp.drift[1,]
              }
            }
          }
          else if (nrow(res.temp.drift)==1){
            if (j>1){
              # Run drift on whole dataset
              res.temp<-drift(data=res.temp,ID=ID,station=station,
                              res.start=res.start,res.end=res.end,
                              residences=residences,units=units,ddd=ddd,
                              from.station=from.station,to.station=to.station)
              # Then add the result from backwards to inf.morts
              inf.morts[nrow(inf.morts)+1,]<-backwards(data=res.temp,morts=res.temp[nrow(res.temp),],
                                                       ID=ID,station=station,res.start=res.start)
            }
            else {
              inf.morts[nrow(inf.morts)+1,]<-res.temp.drift[1,]
            }
          }
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
    if (!is(start,"POSIXt")){
      stop("start is not in the format YYYY-mm-dd HH:MM:SS")
    }
    try(end<-as.POSIXct(end,tz=attributes(data[[res.start]])$tzone),silent=TRUE)
    if (!is(end,"POSIXt")){
      stop("end is not in the format YYYY-mm-dd HH:MM:SS")
    }
    for (i in 1:length(tag)){
      if (is.null(morts.prev)|
          (!is.null(morts.prev)&
           (replace==TRUE|
            !(tag[i] %in% morts.prev)))){
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

  if (backwards==TRUE){
    if (is.null(morts.prev)){
    inf.morts<-backwards(data=data.og,morts=inf.morts,ID=ID,station=station,
                         res.start=res.start)
    }
    else {
      if (nrow(inf.morts)>nrow(morts.prev)){
        new.morts<-c(new.morts,seq(nrow(morts.prev)+1,nrow(inf.morts),1))
      }
      if (length(new.morts)>0){
        for (i in 1:length(new.morts)){
          inf.morts[new.morts[i],]<-backwards(data=data.og,morts=inf.morts[new.morts[i],],ID=ID,station=station,
                                           res.start=res.start)
        }
      }
    }
  }

  inf.morts
}





