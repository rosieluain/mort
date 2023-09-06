#' Review previously identified mortalities using new data
#' @description Uses new data to determine if an animal that was previously
#' flagged as a mortality made a station/location change and may therefore be
#' alive.
#'
#' @param morts a dataframe with previously flagged mortalities. Format does
#' not need to match `new.data` exactly, but the names and formats of `ID`, `station`,
#' and `res.start` fields must match in all input dataframes.
#' @param new.data a dataframe of new residence events (i.e., generated from
#' detection data that were not included in earlier `mort` analyses).
#' @param old.data optional dataframe of residence events that were used
#' in earlier `mort` analyses. If `drift` was applied in earlier analyses,
#' including `old.data` is recommended to avoid falsely identifying station
#' changes.
#' @param type the method used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then user
#' must specify `ID`, `station`, `res.start`, `res.end`, `residences`, and `units`.
#' @param ID a string of the name of the column in `morts` and `new.data` that
#' holds the tag or sample IDs.
#' @param station a string of the name of the column in `morts` and `new.data`
#' that holds the station name or receiver location.
#' @param res.start a string of the name of the column in `morts` and `new.data`
#' that holds the start date and time. Must be specified and in POSIXt or
#' character in the format YYYY-mm-dd HH:MM:SS.
#' @param ddd a dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param from.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may start from. Must
#' be identical to the station/location names in `morts` and `new.data`.
#' @param to.station a string of the name of the column in `ddd` that contains
#' the station/location names where drifting detections may move to. Must
#' be identical to the station/location names in `morts` and `new.data`.
#' @param res.end an optional string of the name of the column in `morts` and `new data`
#' that holds the end date and time. Only needed if drift is applied.
#' @param units optional units of the duration of the residence events in
#' `morts` and `new.data`. Only needed if drift is applied.
#' @param residences an optional character string with the name of the column
#' in `morts` and `new.data` that holds the duration of the residence events.
#' Only needed if drift is applied.
#' @param verbose option to display progress bar as function and called
#' functions are run. Default is TRUE.
#'
#' @return A dataframe with one row for each tag ID from `morts`
#' with a station/location change that was identified
#' in `new.data`. The remaining fields will include the information for the
#' residence event that was identified as the station change, so the station
#' change can be verified by the user before removing the animal from `morts`.
#' All input data fields (e.g., any name, location, or species information
#' that was included with the input data) will be retained.
#' @export
#'
#' @examples
#' morts<-morts(data=events,type="mort",ID="ID",station="Station.Name",
#' method="any",verbose=FALSE)
#'
#' undead<-review(morts=morts,new.data=new.data,
#' type="mort",ID="ID",station="Station.Name",verbose=FALSE)
review<-function(morts,new.data,old.data=NULL,type,ID,station,res.start="auto",
                 res.end=NULL,residences=NULL,units=NULL,
                 ddd=NULL,from.station=NULL,to.station=NULL,
                 verbose=TRUE){

  if (type %in% c("actel","vtrack")){
    new.data<-extractres(data=new.data,type=type)
    if (!is.null(old.data)){
      old.data<-extractres(data=old.data,type=type)
    }
  }

  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, and res.start. If drift is applied, then res.end, residences,
    and units must also be specified.")
  }

  # Check that ID and station are specified (not "auto") for type="mort"
  if (type=="mort"&(ID=="auto"|station=="auto")){
    stop("ID and station must be specified (i.e., cannot be 'auto') for type='mort'")
  }

  if (!is.null(ddd)){
    if (!(type %in% c("manual","actel"))&units!="auto"){
      unitcheck(type=type,units=units,data=data)
    }
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
  if (!is.null(res.end)){
    if (res.end=="auto"){
      res.end<-autofield(type=type,field="res.end")
    }
  }
  if (!is.null(residences)){
    if (residences=="auto"){
      residences<-autofield(type=type,field="residences")
    }
  }
  if (!is.null(units)){
    if (units=="auto"){
      units<-autofield(type=type,field="units")
    }
  }

  if (!is(morts[[res.start]],"POSIXt")){
    try(morts[[res.start]]<-as.POSIXct(morts[[res.start]],tz="UTC",silent=TRUE))
    if (!is(morts[[res.start]],"POSIXt")){
      stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is.null(res.end)){
    if (!is(morts[[res.end]],"POSIXt")){
      try(morts[[res.end]]<-as.POSIXct(morts[[res.end]],tz="UTC",silent=TRUE))
      if (!is(morts[[res.end]],"POSIXt")){
        stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
      }
    }
    if (!is(new.data[[res.end]],"POSIXt")){
      try(new.data[[res.end]]<-as.POSIXct(new.data[[res.end]],tz="UTC",silent=TRUE))
      if (!is(new.data[[res.end]],"POSIXt")){
        stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
      }
    }
  }
  if (!is(new.data[[res.start]],"POSIXt")){
    try(new.data[[res.start]]<-as.POSIXct(new.data[[res.start]],tz="UTC",silent=TRUE))
    if (!is(new.data[[res.start]],"POSIXt")){
      stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is.null(old.data)){
    if (!is(old.data[[res.start]],"POSIXt")){
      try(old.data[[res.start]]<-as.POSIXct(old.data[[res.start]],tz="UTC",silent=TRUE))
      if (!is(old.data[[res.start]],"POSIXt")){
        stop("res.start is not in the format YYYY-mm-dd HH:MM:SS")
      }
    }
    if (!is.null(res.end)){
      if (!is(old.data[[res.end]],"POSIXt")){
        try(old.data[[res.end]]<-as.POSIXct(old.data[[res.end]],tz="UTC",silent=TRUE))
        if (!is(old.data[[res.end]],"POSIXt")){
          stop("res.end is not in the format YYYY-mm-dd HH:MM:SS")
        }
      }
    }
  }

  unmorts<-new.data[0,]

  if (is.null(ddd)){
    for (i in 1:nrow(morts)){
      res.temp<-new.data[new.data[[ID]]==morts[[ID]][i],]
      if (nrow(res.temp)>0){
        if (any(res.temp[[station]]!=morts[[station]][i])){
          # Identify the station change
          j<-1
          if (res.temp[[station]][j]==morts[[station]][i]){
            repeat {
              if (nrow(res.temp)>j){
                if (res.temp[[station]][j+1]==res.temp[[station]][j]){
                  j<-j+1
                }
                else {
                  unmorts[nrow(unmorts)+1,]<-res.temp[(j+1),]
                  break
                }
              }
              else {break}
            }
          }
          else {unmorts[nrow(unmorts)+1,]<-res.temp[1,]}
        }
      }
    }
  }

  else {
    if (is.null(old.data)){
      warning("'old.data' not supplied. If drift events occurred between
              the identified mortality and 'new.data', an invalid station
              change may be generated.")
      data<-new.data[new.data[[ID]] %in% unique(morts[[ID]]),]
    }
    else {
      k<-as.numeric()
      for (j in 1:nrow(morts)){
        k<-c(k,which(old.data[[ID]]==morts[[ID]][j]&
                       old.data[[res.start]]>=morts[[res.start]][j]))
      }
      data<-rbind(old.data[k,],
                  new.data[new.data[[ID]] %in% unique(morts[[ID]]),])
    }
    data<-drift(data=data,ID=ID,station=station,
                res.start=res.start,res.end=res.end,
                ddd=ddd,from.station=from.station,to.station=to.station,
                residences=residences,
                units=units,verbose=verbose)
    for (i in 1:nrow(morts)){
      res.temp<-data[data[[ID]]==morts[[ID]][i],]
      j<-1
      repeat {
        if (nrow(res.temp)>j){
          if (res.temp[[station]][[j]][length(res.temp[[station]][[j]])]==
              res.temp[[station]][[j+1]][1]){
            j<-j+1
          }
          else {
            unmorts[nrow(unmorts)+1,]<-res.temp[(j+1),]
            break
          }
        }
        else {break}
      }
    }
  }

  unmorts

}
