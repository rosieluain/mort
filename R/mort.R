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



#' MortTemp
#'
#' @param data a dataframe with residence events.
#' @param format the format used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify the `units` and `residences`.
#' @param units units of the duration of the residence events in `data`. Must be
#' specified if `format="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events. Must be specified if `format="manual"`.
#' @param method the method to be used in flagging mortalities. Options are
#' "last", "any", "cumulative", or "all"
#' @param season a dataframe with start and end dates of the season(s) of interest
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param singles specifies if single detections (length of residence event = 0)
#' should be removed. Removing single detections is the most conservative method,
#' so chance or potentially invalid detections do not affect mortality estimates.
#'
#' @return a dataframe with...
#' @export
#'
#' @examples
#' \dontrun{mort(data=res.events,format="manual",units="days",residences="ResidenceLength")}
mort<-function(data,format="mort",ID,station,res.start="auto",res.end="auto",
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
  # # Get list of unique tag IDs
  # tag<-unique(data[[ID]])

  stn.change=stationchange(data=data,ID=ID,station=station,
                           residences=residences,singles=singles)

  # Most recent residence longer than max residence before station change
  if (method %in% c("last","all")){
    max.res<-max(resmax(data=data,ID=ID,station=station,res.start=res.start,
                        residences=residences,stnchange=stn.change)[[residences]])

  }

  max.res

}





