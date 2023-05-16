#### Need to add season option

#' Identify most recent station change
#' @description Identify the most recent station or location change from passive
#' acoustic telemetry data.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration.
#' @param format the format used to generate the residence events. Options are
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
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `format="manual"`.
#' @param drift option to account for potential drifting in identifying
#' station changes.
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
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start at the most recent station or location, the date/time of
#' the residence end, and duration of the residence event. All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
#' \dontrun{stationchange(data=res.events,format="manual",ID="TagID",
#' station="Receiver",res.start="StartUTC",residences="ResidencesLength.days")}
stationchange<-function(data,format="mort",ID,station,res.start,res.end,residences,
                        singles=TRUE,drift=FALSE,ddd=NULL,from.station=NULL,
                        to.station=NULL,drift.cutoff=NULL,drift.units=NULL){

  # Check that if drift==TRUE, then drift.units are given

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  stn.change<-data[0,]

  if (drift==FALSE){
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
            if (is(res.temp[[station]],"list")){
              if (any(res.temp[[station]][[j]] %in% res.temp[[station]][[j+1]])){
                j<-j-1
              }
              # This means that there is a station change between row j and j+1
              else {break}
            }
            else {
              if (res.temp[[station]][j]==res.temp[[station]][j+1]){
                j<-j-1
              }
              # This means that there is a station change between row j and j+1
              else {break}
            }
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
    }
  }

  #### Delete if get data=drift.data to work
  else {
    data.drift<-drift(data=data,ID=ID,station=station,
             res.start=res.start,res.end=res.end,residences=residences,
             ddd=ddd,from.station=from.station,to.station=to.station,
             units=drift.units,
             if(!is.null(drift.cutoff)){cutoff=drift.cutoff})

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

        # If there is a cutoff
        # a - b
        # b - c - would show up as the same (no station change)
        # a - b
        # a - b - would show up as different (station change)
        # could get around this by using any:
        # if any match, then it is not a station change

        repeat {
          if (j>0){
            # If residences at j and j+1 are at the same station
            if (any(res.temp[[station]][[j]] %in% res.temp[[station]][[j+1]])){
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
    }
  }

  stn.change
}




#### Need to add season option

#' Maximum residence duration
#' @description Find the maximum duration of a single residence in the dataset that occurred
#' before a station change (i.e., the animal can be assumed to be alive)
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and duration. Residence events must also
#' include end time if `season` is provided.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param stnchange a dataframe with the start time and location of the most
#' recent station or location change. Must use the same column names as `data`.
#'
#' @return a dataframe with the residence information for the longest residence
#' for each tag ID that occurred before the most recent station/location change.
#' @export
#'
#' @examples
#' \dontrun{resmax(data=res.events,ID="TagID",station="Receiver",
#' res.start="StartUTC",residences="ResidencesLength.days",stnchange=station.change)}
resmax<-function(data,ID,station,res.start,
                 residences,stnchange){
  res.max<-data[0,]

  for (i in 1:nrow(stnchange)){
    # Subset residences for ID i, where res.start < res.start of stnchange
    res.temp<-data[data[[ID]]==stnchange[[ID]][i]&
                     data[[res.start]]<stnchange[[res.start]][i],]
    if (nrow(res.temp)>0){
      j<-which(res.temp[[residences]]==max(res.temp[[residences]]))
      for (k in 1:length(j)){
        res.max[nrow(res.max)+1,]<-res.temp[j[k],]
      }
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
#' tag ID, location name, start time, and duration. Residence events must also
#' include end time if `season` is provided.
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param units units of the duration of the residence events in `data`.
#' @param stnchange a dataframe with the start time and location of the most
#' recent station or location change. Must use the same column names as `data`.
#'
#' @return a dataframe with the cumulative residence information for each
#' period where an animal was consecutively detected at a single station/location.
#' Records are only given for cumulative residences that occurred before the most
#' recent station/location change (i.e., the animal can be assumed to be alive).
#' @export
#'
#' @examples
#' \dontrun{resmaxcml(data=res.events,ID="TagID",station="Receiver",
#' res.start="StartUTC",res.end="EndUTC",residences="ResidencesLength.days",
#' units="days",stnchange=station.change)}
resmaxcml<-function(data,ID,station,res.start,res.end,
                    residences,units,stnchange){
  res.maxcml<-data[0,]

  for (i in 1:nrow(stnchange)){
    # Subset residences for ID i, where res.start < res.start of stnchange
    res.temp<-data[data[[ID]]==stnchange[[ID]][i]&
                     data[[res.start]]<stnchange[[res.start]][i],]
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
              # If any of the first station
              # name(s) matches any of the second station name(s)
              if (any(res.temp[[station]][[j]] %in% res.temp[[station]][[j+1]])){
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
                  # Add list of any stations that might not be part
                  res.maxcml[[station]][[nrow(res.maxcml)]]<-unique(unlist(res.temp[[station]][j:k]))
                }
                # Go to next row
                j<-j+1
                break
              }
            }
            # If there is just one residence, so add directly
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

    # If class(station) is not a list (no drift)
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

  }

  res.maxcml[[residences]]<-difftime(res.maxcml[[res.end]],
                                     res.maxcml[[res.start]],
                                     units=units)

  res.maxcml
}

