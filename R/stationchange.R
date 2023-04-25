#### Need to add season option
# Also need to add ddd or detection overlap



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
#' should be removed. Removing single detections is the most conservative method,
#' so chance or potentially invalid detections do not affect mortality estimates.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#'
#' @return a dataframe with one row for each tag ID, including the date/time of
#' the residence start at the most recent station or location, the date/time of
#' the residence end, and duration of the residence event. All input data fields
#' (e.g., any name, location, or species information that was included with the
#' input data) will be retained.
#' @export
#'
#' @examples
stationchange<-function(data,format="mort",ID,station,res.start,residences,
                        singles=FALSE){
  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  stn.change<-data[0,]

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
          if (res.temp[[station]][j]==res.temp[[station]][j+1]){
            j<-j-1
          }
          # # Dead drift direction or overlapping station
          # else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
          #   # Find which dead drifting station
          #   k<-which(ddd$FrStation==res.temp$Station.Name[j])
          #   # If the next station is one of the matching ToStations
          #   if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
          #     # Move to the previous row
          #     j<-j-1
          #   }
          #   else {
          #     # Otherwise, there is a station change between j and j+1
          #     break
          #   }
          # }
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

  stn.change
}




#### Need to add season option

#' Maximum residence duration
#' @description
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
#' @return a dataframe with...
#' @export
#'
#' @examples
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








