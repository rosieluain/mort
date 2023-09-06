#' Identify potential mortalities or expelled tags
#' @description Identifies potential mortalities or expelled tags from passive
#' acoustic telemetry data. Mortalities are identified based on thresholds
#' derived from the dataset itself.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, end time, and duration.
#' @param type the method used to generate the residence events. Options are
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
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
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
#' @param verbose option to display updates and progress bars as
#' sub-functions are called and run. Default is TRUE.
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
#' morts_ex<-morts(data=events,type="mort",ID="ID",
#' station="Station.Name",method="any",verbose=FALSE)
#' head(morts_ex)
#'
#'morts_ex_bw<-morts(data=events,type="mort",ID="ID",
#'station="Station.Name",method="any",backwards=TRUE,verbose=FALSE)
#'head(morts_ex_bw)
morts<-function(data,type="mort",ID,station,res.start="auto",res.end="auto",
               method="all",units="auto",residences="auto",
               singles=TRUE,backwards=FALSE,drift="none",ddd=NULL,
               from.station=NULL, to.station=NULL,drift.cutoff=NULL,
               drift.units=NULL,season.start=NULL,season.end=NULL,
               season.overlap=TRUE,morts.prev=NULL,verbose=TRUE){

  if (!(type %in% c("actel","glatos","manual","mort","vtrack"))){
    stop("invalid type. Please enter valid type: 'actel', 'glatos',
    'manual', 'mort', or 'vtrack' (note lowercase 'v')")
  }

  if (type %in% c("actel","vtrack")){
    data<-extractres(data=data,type=type)
  }

  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, res.start, res.end, residences, and units")
  }

  if (!(type %in% c("manual","actel"))&units!="auto"){
    unitcheck(type=type,units=units,data=data)
  }

  # Check that ID and station are specified (not "auto") for type="mort"
  if (type=="mort"&(ID=="auto"|station=="auto")){
    stop("ID and station must be specified (i.e., cannot be 'auto') for type='mort'")
  }

  # Fill in auto fields
  if (ID=="auto"){
    ID<-autofield(type=type,field="ID")
  }
  if (station=="auto"){
    station<-autofield(type=type,field="station")
  }
  if (res.start=="auto"){
    res.start<-autofield(type=type,field="res.start")
  }
  if (res.end=="auto"){
    res.end<-autofield(type=type,field="res.end")
  }
  if (residences=="auto"){
    residences<-autofield(type=type,field="residences",data=data)
  }
  if (units=="auto"){
    units<-autofield(type=type,field="units",data=data)
  }

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

  data.full<-data
  if (!is.null(season.start)|
      !is.null(season.end)){
    if (verbose==TRUE){
      print("Extracting data from the period/season(s) of interest")
    }
    data<-season(data=data,type=type,ID=ID,station=station,res.start=res.start,res.end=res.end,
                 residences=residences,units=units,season.start=season.start,
                 season.end=season.end,overlap=season.overlap,
                 verbose=verbose)
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
    data.resmax<-data
    drift.resmax<-FALSE
    if (verbose==TRUE){
      print("Identifying last station change")
    }
    sc1<-stationchange(data=data.morts,type=type,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
    if(!is.null(season.start)){
      if (verbose==TRUE){
        print("Identifying last station change in full dataset")
      }
      sc2<-stationchange(data=data.full,type=type,ID=ID,station=station,res.start=res.start,
                         residences=residences,singles=singles)
    }
    else {sc2<-sc1}
  }
  else if (drift=="threshold"){
    data.morts<-data
    if (verbose==TRUE){
      print("Applying drift")
    }
    if (!is.null(drift.cutoff)){
      data.resmax<-drift(data=data,type=type,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station,
                  cutoff.units=drift.units,cutoff=drift.cutoff,
                  verbose=verbose)
    }
    else {
      data.resmax<-drift(data=data,type=type,ID=ID,station=station,
                         res.start=res.start,res.end=res.end,residences=residences,
                         units=units,
                         ddd=ddd,from.station=from.station,to.station=to.station,
                         verbose=verbose)
    }
    drift.resmax<-TRUE
    if (method %in% c("all","cumulative")){
      if(!is.null(drift.cutoff)){
        # This just gets used for resmaxcml, so there should not be a cutoff
        # when identifying the drift residence events
        if (verbose==TRUE){
          print("Applying drift to identify cumulative threshold")
        }
        data<-drift(data=data,type=type,ID=ID,station=station,
                    res.start=res.start,res.end=res.end,residences=residences,
                    units=units,
                    ddd=ddd,from.station=from.station,to.station=to.station,
                    verbose=verbose)
      }
      else {
        data<-data.resmax
      }
    }
    if (verbose==TRUE){
      print("Identifying last station change")
    }
    sc1<-stationchange(data=data.morts,type=type,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
    if(!is.null(season.start)){
      if (verbose==TRUE){
        print("Identifying last station change in full dataset")
      }
      sc2<-stationchange(data=data.full,type=type,ID=ID,station=station,res.start=res.start,
                         residences=residences,singles=singles)
    }
    else {sc2<-sc1}
  }
  else if (drift=="morts"){
    if (verbose==TRUE){
      print("Applying drift")
    }
    data.morts<-drift(data=data,type=type,ID=ID,station=station,
                      res.start=res.start,res.end=res.end,residences=residences,
                      units=units,
                      ddd=ddd,from.station=from.station,to.station=to.station,
                      verbose=verbose)
    if (!is.null(drift.cutoff)){
      if (verbose==TRUE){
        print("Identifying drift residence events to be removed from identifying resmax threshold")
      }
      data.resmax<-drift(data=data,type=type,ID=ID,station=station,
                         res.start=res.start,res.end=res.end,residences=residences,
                         units=units,
                         ddd=ddd,from.station=from.station,to.station=to.station,
                         cutoff.units=drift.units,cutoff=drift.cutoff,
                         verbose=verbose)
    }
    else {data.resmax<-data.morts}
    drift.resmax<-FALSE
    if (!is.null(season.start)){
      if (verbose==TRUE){
        print("Applying drift to full dataset")
      }
      data.full<-drift(data=data.full,type=type,ID=ID,station=station,
                       res.start=res.start,res.end=res.end,residences=residences,
                       units=units,
                       ddd=ddd,from.station=from.station,to.station=to.station,
                       verbose=verbose)
    }
    if (verbose==TRUE){
      print("Identifying last station change")
    }
    sc1<-stationchange(data=data.morts,type=type,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
    if(!is.null(season.start)){
      if (verbose==TRUE){
        print("Identifying last station change in full dataset")
      }
      sc2<-stationchange(data=data.full,type=type,ID=ID,station=station,res.start=res.start,
                         residences=residences,singles=singles)
    }
    else {sc2<-sc1}
  }
  else if (drift=="both"){
    if (verbose==TRUE){
      print("Applying drift")
    }
    data.morts<-drift(data=data,type=type,ID=ID,station=station,
                      res.start=res.start,res.end=res.end,residences=residences,
                      units=units,
                      ddd=ddd,from.station=from.station,to.station=to.station,
                      verbose=verbose)
    if (!is.null(drift.cutoff)){
      if (verbose==TRUE){
        print("Applying drift to identify resmax threshold")
      }
      data.resmax<-drift(data=data,type=type,ID=ID,station=station,
                         res.start=res.start,res.end=res.end,residences=residences,
                         units=units,
                         ddd=ddd,from.station=from.station,to.station=to.station,
                         cutoff.units=drift.units,cutoff=drift.cutoff,
                         verbose=verbose)
    }
    else {
      data.resmax<-data.morts
    }
    drift.resmax<-TRUE
    if (!is.null(season.start)){
      if (verbose==TRUE){
        print("Applying drift to full dataset")
      }
      data.full<-drift(data=data.full,type=type,ID=ID,station=station,
                       res.start=res.start,res.end=res.end,residences=residences,
                       units=units,
                       ddd=ddd,from.station=from.station,to.station=to.station,
                       verbose=verbose)
    }
    if (verbose==TRUE){
      print("Identifying last station change")
    }
    sc1<-stationchange(data=data.morts,type=type,ID=ID,station=station,res.start=res.start,
                       residences=residences,singles=singles)
    if(!is.null(season.start)){
      if (verbose==TRUE){
        print("Identifying last station change in full dataset")
      }
      sc2<-stationchange(data=data.full,type=type,ID=ID,station=station,res.start=res.start,
                         residences=residences,singles=singles)
    }
    else {sc2<-sc1}
  }
  else {stop("drift is not specified correctly")}

  # Most recent residence longer than max residence before station change
  if (method %in% c("last","any","all")){
    # Identify the longest residence followed by a station change
    if (verbose==TRUE){
      print("Identifying the resmax threshold")
    }
    max.res<-max(resmax(data=data.resmax,ID=ID,station=station,res.start=res.start,
                        residences=residences,stnchange=sc1,
                        drift=drift.resmax)[[residences]])
    if (method=="last"){
      # Note that not run for "all" or "any", because "last" is also captured with "any"
      if (verbose==TRUE){
        print("Identifying mortalities for method='last'")
        pb<-txtProgressBar(1,length(tag),style=3)
      }
      for (i in 1:length(tag)){
        # Only run if ID not already in morts
        # If it is already in morts, it would be from infrequent function, which
        # would identify a time earlier or equal to the most recent detection
        if (!(tag[i] %in% morts[[ID]])){
          j<-which(data.morts[[ID]]==tag[i]&
                     data.morts[[res.start]]==max(data.morts[[res.start]][data.morts[[ID]]==tag[i]&
                                                                            data.morts[[station]]!="Break"]))
          if (length(j)>1){
            j<-j[1]
          }
          if (data.morts[[residences]][j]>max.res){
            morts[nrow(morts)+1,]<-data.morts[j,]
          }
        }
        if (verbose==TRUE){
          setTxtProgressBar(pb,i)
        }
      }
    }
    if (method %in% c("any","all")){
      # Identify all the residences longer than max.res
      # If they are during or after the last station change, then add them to morts
      if (verbose==TRUE){
        print("Identifying mortalities for method='any'")
        pb<-txtProgressBar(1,length(tag),style=3)
      }
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
        if (verbose==TRUE){
          setTxtProgressBar(pb,i)
        }
      }
    }
  }

  if (method %in% c("cumulative","all")){
    # Identify the longest cumulative residence followed by a station change
    if (verbose==TRUE){
      print("Identifying the resmaxcml threshold")
    }
    max.rescml<-max(resmaxcml(data=data,ID=ID,station=station,res.start=res.start,
                              res.end=res.end,residences=residences,units=units,
                              stnchange=sc1)[[residences]],na.rm=TRUE)
    if (verbose==TRUE){
      print("Identifying mortalities for method='cumulative'")
      pb<-txtProgressBar(1,nrow(sc2),style=3)
    }
    for (i in 1:nrow(sc2)){
      res.temp<-data.morts[data.morts[[ID]]==sc2[[ID]][i],]
      res.temp<-res.temp[order(res.temp[[res.start]]),]
      if (nrow(res.temp)>0){
        repeat {
          if (res.temp[[station]][nrow(res.temp)]=="Break"){
            res.temp<-res.temp[-nrow(res.temp),]
          }
          else {break}
        }
        # To compensate for seasonality (which introduces "Breaks")
        if (any(res.temp[[station]]=="Break")){
          # Need two values, then determine which is lower
          # Value 1: difftime between end of most recent residence and station change (sc2)
          dt1<-difftime(res.temp[[res.end]][nrow(res.temp)],
                        sc2[[res.start]][i],
                        units=units)
          # Value 2: difftime between end of most recent residence and most recent break
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
      if (verbose==TRUE){
        setTxtProgressBar(pb,i)
      }
    }
  }

  if (backwards==TRUE&nrow(morts)>0){
    if (verbose==TRUE){
      print("Applying backwards=TRUE")
    }
    if (!is.null(morts.prev)){
      if (nrow(morts)>nm){
        new.morts<-unique(c(new.morts,seq((nm+1),nrow(morts),1)))
        if (verbose==TRUE){
          pb<-txtProgressBar(1,length(new.morts),style=3)
        }
        for (i in 1:length(new.morts)){
          morts[i,]<-backwards(data=NULL,morts=morts[i,],ID=ID,station=station,
                               res.start=res.start,stnchange=sc2)
          if (verbose==TRUE){
            setTxtProgressBar(pb,i)
          }
        }
      }
    }
    else {
      morts<-backwards(data=NULL,morts=morts,ID=ID,station=station,
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
#' tag ID, location name, start time, end time, and duration.
#' @param type the method used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify `ID`, `station`, `res.start`, `res.end`, `residences`, and `units`.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
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
#' @param verbose option to display updates and progress bars as
#' sub-functions are called and run. Default is TRUE.
#'
#' @details Example of `method="recent"`: if `threshold=10`, `threshold.units="mins"`,
#' `recent.period=52` and `recent.units="weeks"` (1 year), an animal will be
#' flagged as a potential mortality if it was detected for less than 10 minutes
#' within a year, ending with the most recent detection.
#'
#' Example of `method="defined"`: if `threshold=10`, `threshold.units="mins"`,
#' `start="2019-10-01"`, and `end="2020-06-01"`, an animal
#' will be flagged as a potential mortality if it was detected for less than 10
#' minutes between 01 October 2019 and 01 June 2020.
#'
#' @return if `morts=NULL`, a dataframe with one row for each tag ID, including the date/time of
#' the residence start when the potential mortality or expelled tag was identified.
#' If `morts` is specified, any potential mortalities will be added to existing
#' `morts` dataframe. If `morts` is specified and `replace=TRUE`, then any
#' mortalities that are flagged by `infrequent()` and occurred at an earlier
#' time than those in `morts` will be replaced to the earlier date.
#' All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
#' ## Recent example
#' inf_recent<-infrequent(data=events,type="mort",ID="ID",
#' station="Station.Name",method="recent",
#' threshold=72,threshold.units="hours",
#' recent.period=52,recent.units="weeks",
#' verbose=FALSE)
#' head(inf_recent)
#'
#' ## User-defined example
#' inf_defined<-infrequent(data=events,type="mort",ID="ID",
#' station="Station.Name",method="defined",
#' threshold=12,threshold.units="hours",
#' start="2006-06-15",end="2006-10-15",
#' verbose=FALSE)
#' head(inf_defined)
infrequent<-function(data,type="mort",ID,station,res.start="auto",
                     res.end="auto",residences="auto",units="auto",
                     method,threshold,threshold.units=NULL,recent.period=NULL,recent.units=NULL,
                     start=NULL,end=NULL,morts.prev=NULL,replace=FALSE,backwards=FALSE,
                     ddd=NULL,from.station=NULL,to.station=NULL,
                     drift.cutoff=NULL,drift.units=NULL,verbose=TRUE){

  if (type %in% c("actel","vtrack")){
    data<-extractres(data=data,type=type)
  }

  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, res.start, res.end, residences, and units")
  }

  if (!(type %in% c("manual","actel"))&units!="auto"){
    unitcheck(type=type,units=units,data=data)
  }

  # Check that ID and station are specified (not "auto") for type="mort"
  if (type=="mort"&(ID=="auto"|station=="auto")){
    stop("ID and station must be specified (i.e., cannot be 'auto') for type='mort'")
  }

  # Fill in auto fields
  if (ID=="auto"){
    ID<-autofield(type=type,field="ID")
  }
  if (station=="auto"){
    station<-autofield(type=type,field="station")
  }
  if (res.start=="auto"){
    res.start<-autofield(type=type,field="res.start")
  }
  if (res.end=="auto"){
    res.end<-autofield(type=type,field="res.end")
  }
  if (residences=="auto"){
    residences<-autofield(type=type,field="residences",data=data)
  }
  if (units=="auto"){
    units<-autofield(type=type,field="units",data=data)
  }

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
    k<-which(data[[station]]=="Break")
    if (length(k)>0){
      data<-data[-k,]
    }
  }

  if (!is.null(ddd)){
    if (is.null(drift.cutoff)){
      if (verbose==TRUE){
        print("Applying drift")
      }
      data<-drift(data=data,type=type,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station,
                  verbose=verbose)
      data.morts<-data
    }
    else {
      if (verbose==TRUE){
        print("Applying drift with cutoff")
      }
      data<-drift(data=data,type=type,ID=ID,station=station,
                  res.start=res.start,res.end=res.end,residences=residences,
                  units=units,
                  ddd=ddd,from.station=from.station,to.station=to.station,
                  cutoff=drift.cutoff,cutoff.units=drift.units,
                  verbose=verbose)
      if (verbose==TRUE){
        print("Applying drift (no cutoff)")
      }
      data.morts<-drift(data=data,type=type,ID=ID,station=station,
                     res.start=res.start,res.end=res.end,residences=residences,
                     units=units,
                     ddd=ddd,from.station=from.station,to.station=to.station,
                     verbose=verbose)
    }
  }
  else {data.morts<-data}

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
    # Convert threshold units if threshold units and residence units do not match
    if (units!=threshold.units){
      threshold<-unitconvert(unit1=threshold.units,unit2=units,val1=threshold)
    }
    if (verbose==TRUE){
      print("Identifying mortalities from infrequent detections")
      pb<-txtProgressBar(1,length(tag),style=3)
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
        res.temp<-res.temp[j:nrow(res.temp),]
        if (is.null(ddd)){
          if (nrow(res.temp)>=1&
              length(unique(res.temp[[station]]))==1&
              sum(res.temp[[residences]])<threshold){
            if (tag[i] %in% inf.morts[[ID]]){
              k<-which(inf.morts[[ID]]==tag[i])
              if (res.temp[[res.start]][1]<inf.morts[[res.start]][k]){
                inf.morts[k,]<-res.temp[1,]
                if (!is.null(morts.prev)&backwards==TRUE){
                  new.morts<-c(new.morts,k)
                }
              }
            }
            else {
              inf.morts[nrow(inf.morts)+1,]<-res.temp[1,]
            }
          }
        }
        else {
          if (nrow(res.temp)>0&
              sum(res.temp[[residences]])<threshold){
            res.temp.drift<-drift(data=res.temp,
                                  type=type,ID=ID,station=station,
                                  res.start=res.start,res.end=res.end,
                                  residences=residences,units=units,ddd=ddd,
                                  from.station=from.station,to.station=to.station,
                                  verbose=FALSE)
            if (nrow(res.temp.drift)>1){
              # Then run backwards
              rb<-backwards(data=res.temp.drift,morts=res.temp.drift[nrow(res.temp.drift),],
                            ID=ID,station=station,res.start=res.start)
              k<-which(res.temp.drift[[res.start]]==rb[[res.start]])
              if (length(k)>1){
                k<-which(res.temp.drift[[res.end]][k]==max(res.temp.drift[[res.end]][k]))
              }
              if (k==1){
                if (backwards==TRUE&j>1){
                  # Run drift on whole dataset
                  res.temp.drift.full<-drift(data=res.temp,type=type,ID=ID,station=station,
                                             res.start=res.start,res.end=res.end,
                                             residences=residences,units=units,ddd=ddd,
                                             from.station=from.station,to.station=to.station,
                                             verbose=FALSE)
                  # Run backwards on dataset, starting with j
                  morts.temp<-backwards(data=res.temp.drift.full,
                                        morts=res.temp[j,],
                                        ID=ID,station=station,res.start=res.start)
                  if (tag[i] %in% inf.morts[[ID]]){
                    m<-which(inf.morts[[ID]]==tag[i])
                    if (morts.temp[[res.start]]<inf.morts[[res.start]][m]){
                      inf.morts[m,]<-morts.temp[1,]
                      if (!is.null(morts.prev)&backwards==TRUE){
                        new.morts<-c(new.morts,m)
                      }
                    }
                  }
                  else {
                    inf.morts[nrow(inf.morts)+1,]<-morts.temp
                  }
                }
                else {
                  if (tag[i] %in% inf.morts[[ID]]){
                    m<-which(inf.morts[[ID]]==tag[i])
                    if (res.temp.drift[[res.start]][1]<inf.morts[[res.start]][m]){
                      inf.morts[m,]<-res.temp.drift[1,]
                      if (!is.null(morts.prev)&backwards==TRUE){
                        new.morts<-c(new.morts,m)
                      }
                    }
                  }
                  else {
                    inf.morts[nrow(inf.morts)+1,]<-res.temp.drift[1,]
                  }
                }
              }
            }
            else if (nrow(res.temp.drift)==1){
              if (backwards==TRUE&j>1){
                # Run drift on whole dataset
                res.temp<-drift(data=res.temp,type=type,ID=ID,station=station,
                                res.start=res.start,res.end=res.end,
                                residences=residences,units=units,ddd=ddd,
                                from.station=from.station,to.station=to.station,
                                verbose=FALSE)
                # Run backwards
                morts.temp<-backwards(data=res.temp,
                                      morts=res.temp[nrow(res.temp),],
                                      ID=ID,station=station,res.start=res.start)
                if (tag[i] %in% inf.morts[[ID]]){
                  m<-which(inf.morts[[ID]]==tag[i])
                  if (morts.temp[[res.start]]<inf.morts[[res.start]][m]){
                    inf.morts[m,]<-morts.temp[1,]
                    if (!is.null(morts.prev)&backwards==TRUE){
                      new.morts<-c(new.morts,m)
                    }
                  }
                }
                else {
                  inf.morts[nrow(inf.morts)+1,]<-morts.temp
                }
              }
              else {
                if (tag[i] %in% inf.morts[[ID]]){
                  m<-which(inf.morts[[ID]]==tag[i])
                  if (length(m)>0){
                    if (res.temp.drift[[res.start]][1]<inf.morts[[res.start]][m]){
                      inf.morts[m,]<-res.temp.drift[1,]
                      if (!is.null(morts.prev)&backwards==TRUE){
                        new.morts<-c(new.morts,m)
                      }
                    }
                  }
                }
                else {
                  inf.morts[nrow(inf.morts)+1,]<-res.temp.drift[1,]
                }
              }
            }
          }
        }
      }
      if (verbose==TRUE){
        setTxtProgressBar(pb,i)
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
    try(start<-as.POSIXct(start,tz="UTC",silent=TRUE))
    if (!is(start,"POSIXt")){
      stop("start is not in the format YYYY-mm-dd HH:MM:SS")
    }
    try(end<-as.POSIXct(end,tz="UTC",silent=TRUE))
    if (!is(end,"POSIXt")){
      stop("end is not in the format YYYY-mm-dd HH:MM:SS")
    }
    # Convert threshold units if threshold units and residence units do not match
    if (units!=threshold.units){
      threshold<-unitconvert(unit1=threshold.units,unit2=units,val1=threshold)
    }
    if (verbose==TRUE){
      pb<-txtProgressBar(1,length(tag),style=3)
    }
    for (i in 1:length(tag)){
      if (is.null(morts.prev)|
          (!is.null(morts.prev)&
           (replace==TRUE|
            !(tag[i] %in% morts.prev[[ID]])))){
        res.temp<-data[data[[ID]]==tag[i]&
                         data[[res.end]]>=start&
                         data[[res.start]]<=end,]
        if (is.null(ddd)){
          if (nrow(res.temp)>=1&
              length(unique(res.temp[[station]]))==1&
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
          if (nrow(res.temp)>0&
              sum(res.temp[[residences]])<threshold){
            res.temp.drift<-drift(data=res.temp,type=type,ID=ID,station=station,
                                  res.start=res.start,res.end=res.end,
                                  residences=residences,units=units,ddd=ddd,
                                  from.station=from.station,to.station=to.station,
                                  verbose=FALSE)
            if (nrow(res.temp.drift)>1){
              # Then run backwards
              rb<-backwards(data=res.temp.drift,morts=res.temp.drift[nrow(res.temp.drift),],
                            ID=ID,station=station,res.start=res.start)
              k<-which(res.temp.drift[[res.start]]==rb[[res.start]])
              if (k==1){
                if (tag[i] %in% inf.morts[[ID]]){
                  j<-which(inf.morts[[ID]]==tag[i])
                  if (rb[[res.start]]<inf.morts[[res.start]][j]){
                    inf.morts[j,]<-rb
                    if (!is.null(morts.prev)&backwards==TRUE){
                      new.morts<-c(new.morts,j)
                    }
                  }
                }
                else {
                  inf.morts[nrow(inf.morts)+1,]<-rb
                }
              }
            }
            else if (nrow(res.temp.drift)==1){
              if (tag[i] %in% inf.morts[[ID]]){
                j<-which(inf.morts[[ID]]==tag[i])
                if (res.temp.drift[[res.start]]<inf.morts[[res.start]][j]){
                  inf.morts[j,]<-res.temp.drift
                  if (!is.null(morts.prev)&backwards==TRUE){
                    new.morts<-c(new.morts,j)
                  }
                }
              }
              else {
                inf.morts[nrow(inf.morts)+1,]<-res.temp.drift
              }
            }
          }
        }
      }
      if (verbose==TRUE){
        setTxtProgressBar(pb,i)
      }
    }
  }

  if (backwards==TRUE){
    if (verbose==TRUE){
      print("Applying backwards=TRUE")
    }
    if (is.null(morts.prev)){
      inf.morts<-backwards(data=data.morts,morts=inf.morts,ID=ID,station=station,
                         res.start=res.start)
    }
    else {
      if (nrow(inf.morts)>nrow(morts.prev)){
        new.morts<-c(new.morts,seq(nrow(morts.prev)+1,nrow(inf.morts),1))
      }
      if (length(new.morts)>0){
        for (i in 1:length(new.morts)){
          inf.morts[new.morts[i],]<-backwards(data=data.morts,morts=inf.morts[new.morts[i],],ID=ID,station=station,
                                           res.start=res.start)
        }
      }
    }
  }

  inf.morts
}





