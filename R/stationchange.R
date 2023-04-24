#### Need to add season option
# Also need to add ddd or detection overlap



#' Title
#'
#' @param data
#' @param format
#' @param ID
#' @param station
#' @param res.start
#' @param res.end
#' @param method
#' @param units
#' @param residences
#' @param season
#'
#' @return
#' @export
#'
#' @examples
stationchange<-function(data,ID,station,residences,
                        singles){
  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  stn.change<-data[0,]

  for (i in 1:length(tag)){
    res.temp<-data[data[[ID]]==tag[i],]
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
    }

    # # Don't want this (code below), but is there value in adding the tag ID and then an NA
    # # if there are no station changes?
    # else if (nrow(res.temp==1)){
    #   stn.change.summer[nrow(stn.change.summer)+1,]<-res.temp[1,]
    # }
  }

  stn.change
}




#### Need to add season option

#' Title
#'
#' @param data
#' @param ID
#' @param station
#' @param res.start
#' @param residences
#' @param stnchange
#'
#' @return
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
    j<-which(res.temp[[residences]]==max(res.temp[[residences]]))
    for (k in 1:length(j)){
      res.max[nrow(res.max)+1,]<-res.temp[j[k],]
    }
  }

  res.max
}


# max(res.max.summer$ResTime)
# # 37.4 in 2021 and spring 2022 - same after changing residences/Station.B
# # Before 2021 data, was 34.2








