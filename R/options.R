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






#### DDD ####

#### Seasonal ####
