#' Identify most recent station change
#' @description Identify the most recent station or location change from passive
#' acoustic telemetry data.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration.
#' @param type the method used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify `ID`, `station`, `res.start`, and `residences`.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param singles specifies if single detections (length of residence event = 0)
#' should be retained. Default is `TRUE`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `type="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
#' @param drift option to account for potential drifting in identifying
#' station changes.
#' @param ddd a dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param units optional units of the duration of the residence events in `data`.
#' Required if `drift=TRUE`. Options are "auto", or "secs", "mins", "hours", "days",
#' or "weeks".
#' @param from.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may start from. Must
#' be identical to the station/location names in `data`.
#' @param to.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may move to. Must
#' be identical to the station/location names in `data`.
#' @param verbose option to display progress bar as function is run. Default
#' is TRUE.
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start at the most recent station or location, the date/time of
#' the residence end, and duration of the residence event. All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
#' stn.change<-stationchange(data=events,type="mort",ID="ID",station="Station.Name",verbose=FALSE)
#' head(stn.change)
stationchange<-function(data,type="mort",ID,station,res.start="auto",
                        res.end="auto",residences="auto",
                        singles=TRUE,drift=FALSE,ddd=NULL,units=NULL,from.station=NULL,
                        to.station=NULL,verbose=TRUE){

  if (type %in% c("actel","vtrack")&is(data,"list")){
    data<-extractres(data=data,type=type)
  }

  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, res.start, res.end, and residences")
  }

  if (drift==TRUE&is.null(units)){
    stop("units must be specified if applying drift")
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

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  stn.change<-data[0,]

  if (drift==FALSE&
      !is(data[[station]],"list")){
    if (verbose==TRUE){
      pb<-txtProgressBar(1,length(tag),style=3)
    }
    for (i in 1:length(tag)){
      res.temp<-data[data[[ID]]==tag[i],]
      # Order by time
      res.temp<-res.temp[order(res.temp[[res.start]]),]
      if (singles==FALSE){
        # Remove single detections
        res.temp<-res.temp[res.temp[[residences]]!=0,]
      }
        # If there are at least two records
      if (nrow(res.temp)>=2){
        # Set j to second last record
        j<-nrow(res.temp)-1
        repeat {
          if (j>0){
            # If residences at j and j+1 are at the same station
            if (res.temp[[station]][j+1]=="Break"|
               res.temp[[station]][j]=="Break"){
             j<-j-1
            }
            else if (res.temp[[station]][j]==res.temp[[station]][j+1]){
             j<-j-1
            }
            # This means that there is a station change between row j and j+1
            else {break}
          }
          else {break}
        }
        if (j>0){
          stn.change[nrow(stn.change)+1,]<-res.temp[(j+1),]
        }
        # If the fish has not changed locations for the whole detection history
        else {
         stn.change[nrow(stn.change)+1,]<-res.temp[1,]
        }
       }
      else if (nrow(res.temp)==1){
        stn.change[nrow(stn.change)+1,]<-res.temp[1,]
      }
      if (verbose==TRUE){
        setTxtProgressBar(pb,i)
      }
    }
  }

  else {
    if (drift==TRUE){
      data.drift<-drift(data=data,type=type,ID=ID,station=station,
                         res.start=res.start,res.end=res.end,residences=residences,
                         units=units,ddd=ddd,from.station=from.station,to.station=to.station,
                        verbose=verbose)
    }
    else {data.drift<-data}
    pb<-txtProgressBar(1,length(tag),style=3)
    for (i in 1:length(tag)){
      res.temp<-data.drift[data.drift[[ID]]==tag[i],]
      # Order by time
      res.temp<-res.temp[order(res.temp[[res.start]]),]
      if (singles==FALSE){
        # Remove single detections
        res.temp<-res.temp[res.temp[[residences]]!=0,]
      }

      # If there are at least two records
      if (nrow(res.temp)>=2){
        # Set j to second last record
        j<-nrow(res.temp)-1
        repeat {
          if (j>0){
            # If residences at j and j+1 are at the same station
            if (any(res.temp[[station]][[j+1]]=="Break")|
                any(res.temp[[station]][[j]]=="Break")){
              j<-j-1
            }
            else if (res.temp[[station]][[j]][length(res.temp[[station]][[j]])]==
                     res.temp[[station]][[j+1]][1]){
              j<-j-1
            }
            # This means that there is a station change between row j and j+1
            else {break}
          }
          else {break}
        }
        if (j>0){
          stn.change[nrow(stn.change)+1,]<-res.temp[(j+1),]
        }
        # If the fish has not changed locations for the whole detection history
        else {
          stn.change[nrow(stn.change)+1,]<-res.temp[1,]
        }
      }
      else if (nrow(res.temp)==1){
        stn.change[nrow(stn.change)+1,]<-res.temp[1,]
      }
      setTxtProgressBar(pb,i)
    }
  }

  stn.change
}

#' Maximum residence duration
#' @description Find the maximum duration of a single residence in the dataset that occurred
#' before a station change (i.e., the animal can be assumed to be alive)
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `type="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param stnchange a dataframe with the start time and location of the most
#' recent station or location change. Must use the same column names as `data`.
#' @param drift indicates if drift residence events should be included in
#' determining the maximum residence duration
#' @param verbose option to display progress bar as function is run. Default
#' is TRUE.
#'
#' @return a dataframe with the residence information for the longest residence
#' for each tag ID that occurred before the most recent station/location change.
#' @export
#'
#' @examples
#' # Identify most recent station change
#' station.change<-stationchange(data=events,type="mort",ID="ID",
#' station="Station.Name",verbose=FALSE)
#'
#' longest_res_events<-resmax(data=events,ID="ID",station="Station.Name",
#' res.start="ResidenceStart",residences="ResidenceLength.days",
#' stnchange=station.change,verbose=FALSE)
#' head(longest_res_events)
resmax<-function(data,ID,station,res.start,
                 residences,stnchange,drift=FALSE,verbose=TRUE){
  res.max<-data[0,]

  if (verbose==TRUE){
    pb<-txtProgressBar(1,nrow(stnchange),style=3)
  }
  for (i in 1:nrow(stnchange)){
    # Subset residences for ID i, where res.start < res.start of stnchange
    res.temp<-data[data[[ID]]==stnchange[[ID]][i]&
                     data[[res.start]]<stnchange[[res.start]][i],]
    if (nrow(res.temp)>0){
      if (drift==FALSE&
          is(data[[station]],"list")){
        del<-as.numeric()
        for (j in 1:nrow(res.temp)){
          if (length(res.temp[[station]][[j]])>1){
            del<-c(del,j)
          }
        }
        if (length(del)>0){
          res.temp<-res.temp[-del,]
        }
      }
      if (nrow(res.temp)>0){
        j<-which(res.temp[[residences]]==max(res.temp[[residences]],na.rm=TRUE))
        for (k in 1:length(j)){
          res.max[nrow(res.max)+1,]<-res.temp[j[k],]
        }
      }
    }
    if (verbose==TRUE){
      setTxtProgressBar(pb,i)
    }
  }

  res.max
}


#' Maximum cumulative residence duration
#' @description Find the maximum duration that an animal spent at a single
#' station/location before a station change (i.e., the animal can be assumed
#' to be alive). Differs from `resmax` in that the duration is cumulative -
#' the time of residence events and intervals between residence events are all
#' included, provided there are no intervening residence events at other
#' stations/locations.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `type="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt if `type="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param units units of the duration of the residence events in `data`.
#' @param stnchange a dataframe with the start time and location of the most
#' recent station or location change. Must use the same column names as `data`.
#' @param verbose option to display progress bar as function is run. Default
#' is TRUE.
#'
#' @return a dataframe with the cumulative residence information for each
#' period where an animal was consecutively detected at a single station/location.
#' Records are only given for cumulative residences that occurred before the most
#' recent station/location change (i.e., the animal can be assumed to be alive).
#' @export
#'
#' @examples
#' # Identify most recent station change
#' station.change<-stationchange(data=events[events$ID=="A",],type="mort",
#' ID="ID",station="Station.Name",verbose=FALSE)
#'
#' cumulative_events<-resmaxcml(data=events[events$ID=="A",],ID="ID",
#' station="Station.Name",res.start="ResidenceStart",res.end="ResidenceEnd",
#' residences="ResidenceLength.days",units="days",
#' stnchange=station.change,verbose=FALSE)
resmaxcml<-function(data,ID,station,res.start,res.end,
                    residences,units,stnchange,verbose=TRUE){
  res.maxcml<-data[0,]

  if (verbose==TRUE){
    pb<-txtProgressBar(1,nrow(stnchange),style=3)
  }
  for (i in 1:nrow(stnchange)){
    # Subset residences for ID i, where res.start < res.start of stnchange
    res.temp<-data[data[[ID]]==stnchange[[ID]][i]&
                     data[[res.start]]<stnchange[[res.start]][i],]
    # If station is a list, then drift was included
    if (is(res.temp[[station]],"list")){
      j<-1
      repeat {
        # If there are more than or equal (single residence) to j rows
        if (nrow(res.temp)>=j){
          # Set k = j to mark beginning
          k<-j
          repeat{
            # If there are multiple residences
            if (nrow(res.temp)>j){
              # If the last station of the first record matches the first
              # station of the next record
              if (res.temp[[station]][[j]][length(res.temp[[station]][[j]])]==
                  res.temp[[station]][[j+1]][1]){
                # Go to next row
                j<-j+1
              }
              # Otherwise, there is a station change after j
              else {
                # Add record j (before station change)
                res.maxcml[nrow(res.maxcml)+1,]<-res.temp[j,]
                # If the previous station change was before j (as marked by k)
                if (k!=j){
                  # Adjust StartUTC so it is the start of the cumulative residence
                  res.maxcml[[res.start]][nrow(res.maxcml)]<-res.temp[[res.start]][k]
                  # Add list of any stations that might not be part
                  res.maxcml[[station]][[nrow(res.maxcml)]]<-unique(unlist(res.temp[[station]][j:k]))
                }
                # Go to next row
                j<-j+1
                break
              }
            }
            # There is just one residence, so add directly
            else {
              res.maxcml[nrow(res.maxcml)+1,]<-res.temp[j,]
              j<-j+1
              break
            }
          }
        }
        else {break}
      }
    }

    # If station is not a list (no drift)
    else {
      j<-1
      repeat {
        # If there are more than or equal (single residence) to j rows
        if (nrow(res.temp)>=j){
          # Set k = j to mark beginning
          k<-j
          repeat{
            # If there are multiple residences and the first station
            # name is the same as the next record
            if (nrow(res.temp)>j&
                (res.temp[[station]][j]==res.temp[[station]][j+1])){
              # Go to next row
              j<-j+1
            }
            # Otherwise, there is a station change after j
            else {
              # Add record j (before station change)
              res.maxcml[nrow(res.maxcml)+1,]<-res.temp[j,]
              # If the previous station change was before j (as marked by k)
              if (k!=j){
                # Adust StartUTC so it is the start of the cumulative residence
                res.maxcml[[res.start]][nrow(res.maxcml)]<-res.temp[[res.start]][k]
              }
              # Go to next row
              j<-j+1
              break
            }
          }
        }
        else {break}
      }
    }
    if (verbose==TRUE){
      setTxtProgressBar(pb,i)
    }
  }

  res.maxcml[[residences]]<-difftime(res.maxcml[[res.end]],
                                     res.maxcml[[res.start]],
                                     units=units)

  res.maxcml
}

