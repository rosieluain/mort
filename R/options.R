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
backwards<-function(data,morts,ID,station,res.start,stnchange=NULL){
  if (any(data[[station]]=="Break")){
    warning("Either a station name was 'Break' or data included seasonal
            breaks. Breaks were removed.")
  }
  # If stnchange was provided, shift morts to stnchange
  if (!is.null(stnchange)){
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
        k<-which(res.temp[[station]]=="Break")
        if (length(k)>0){
          res.temp<-res.temp[-k,]
        }
        # Start at end and work backwards, looking for station change
        j<-nrow(res.temp)
        repeat{
          if (j>=2){
            if (is(res.temp[[station]],"list")){
              if (res.temp[[station]][[j-1]][length(res.temp[[station]][[j-1]])]==
                  res.temp[[station]][[j]][1]){
                j<-j-1
              }
              else {break}
            }
            else {
              if (res.temp[[station]][j]==res.temp[[station]][j-1]){
                j<-j-1
              }
              else {break}
            }
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

#' Dead drift
#' @description Identifies sequential residence events where detected movement
#' between stations may be due to drifting of an expelled tag or dead animal.
#'
#' @param data a data frame of residence events. Residence events must include
#' tag ID, location name, start time, and end time.
#' @param type the method used to generate the residence events in `data`.
#' Options are "mort", "actel", "glatos", "vtrack", or "manual". If "manual", then
#' user must specify `ID`, `station`, `res.start`, and `res.end`.
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
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param units units of the duration of the residence events in `data`.
#' @param ddd a dataframe of stations/locations where detected movement between
#' stations may be due to drifting of an expelled tag or dead animal.
#' @param from.station a string of the name of the column in `data` that contains
#' the station/location names where drifting detections may start from.
#' @param to.station a string of the name of the column in `data` that contains
#' the station/location names where drifting detections may move to.
#' @param cutoff the maximum allowable time difference between detections to be
#' considered a single residence event. Default is `NULL`.
#' @param cutoff.units the units of the cutoff. Options are "secs", "mins", "hours",
#' "days", and "weeks".
#' @param progress.bar option to display progress bar as `drift` is applied.
#' Default is TRUE.
#'
#' @return A data frame with one row for each residence event. Format is the
#' same as the input residence events, but events that may be due to dead drift
#' are combined into single residence events.
#' @export
#'
#' @examples
#' \dontrun{drift(data=data,format="manual",ID="TagID",station="Receiver",
#' res.start="ResidenceStart",res.end="ResidenceEnd",ddd=driftstations,
#' from.station="FrStation",to.station="ToStation")}
drift<-function(data,format,ID,station,res.start,res.end,
                residences,units,ddd,from.station,to.station,
                cutoff=NULL,cutoff.units=NULL,progress.bar=TRUE){

  if (type %in% c("actel","vtrack")){
    data<-extractres(data=data,type=type)
  }

  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, res.start, res.end, residences, and units")
  }

  # Check that ID and station are specified (not "auto") for format="mort"
  if (type=="mort"&(ID=="auto"|station=="auto")){
    stop("ID and station must be specified (i.e., cannot be 'auto') for format='mort'")
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
    residences<-autofield(type=type,field="residences")
  }
  if (units=="auto"){
    units<-autofield(type=type,field="units")
  }

  # Could make this check be a function
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

  # Convert station to list (to hold all stations, in order)
  data[[station]]<-as.list(data[[station]])

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  res.drift<-data[0,]

  if (progress.bar==TRUE){
    pb<-txtProgressBar(1,length(tag),style=3)
  }
  for (i in 1:length(tag)){
    # print(paste("first check i =",i))
    # Ok, so this counts up just fine
    res.temp<-data[data[[ID]]==tag[i],]
    if (nrow(res.temp)>1&
        any(res.temp[[station]][1:(nrow(res.temp)-1)] %in% ddd[[from.station]])){
      j<-which(res.temp[[station]] %in% ddd[[from.station]])
      # Exclude any where j is the last record
      if (nrow(res.temp) %in% j){
        j<-j[-which(j==nrow(res.temp))]
      }
      del<-as.numeric()
      if (is.null(cutoff)){
        for (k in 1:length(j)){
          if (res.temp[[station]][j[k]+1] %in%
              ddd[[to.station]][ddd[[from.station]]==res.temp[[station]][[j[k]]][length(res.temp[[station]][[j[k]]])]]){
            res.temp[[res.start]][j[k]+1]<-res.temp[[res.start]][j[k]]
            res.temp[[station]][[j[k]+1]]<-append(res.temp[[station]][[j[k]]],res.temp[[station]][[j[k]+1]])
            del<-c(del,j[k])
          }
        }
      }
      else {
        for (k in 1:length(j)){
          if ((res.temp[[station]][j[k]+1] %in%
               ddd[[to.station]][ddd[[from.station]]==res.temp[[station]][j[k]]])&
              difftime(res.temp[[res.start]][j[k]+1],
                       res.temp[[res.end]][j[k]],
                       units=cutoff.units)<cutoff){
            res.temp[[res.start]][j[k]+1]<-res.temp[[res.start]][j[k]]
            res.temp[[station]][j[k]+1]<-res.temp[[station]][j[k]]
            del<-c(del,j[k])
          }
        }
      }
      if (length(del)>0){
        res.temp<-res.temp[-del,]
      }
    }
    res.drift[(nrow(res.drift)+1):(nrow(res.drift)+nrow(res.temp)),]<-res.temp[,]
    if (progress.bar==TRUE){
      setTxtProgressBar(pb,i)
    }
  }

  res.drift[[residences]]<-difftime(res.drift[[res.end]],
                                    res.drift[[res.start]],
                                    units=units)

  res.drift
}

#' Select residence events from specified seasons
#' @description Select residence events from specified seasons, to be used to
#' identify potential mortalities or expelled tags. Useful when animals show
#' strong seasonal patterns in behaviour. For example, a reduction in movement during
#' winter may be falsely identified as a mortality, or
#' increase the threshold use to identify mortalities, which would then cause
#' potential mortalities to be missed.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, start time, end time, and duration.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `format="manual"`.
#' @param residences a character string with the name of the column in `data`
#' that holds the duration of the residence events.
#' @param units Units of the duration of the residence events in `data`. Options are "secs",
#' "mins", "hours", "days", and "weeks".
#' @param season.start the start date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param season.end the end date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param overlap option to include residence events that overlap either the
#' beginning or the end of the period of interest. If `TRUE`, the full overlapping
#' residence events will be retained. If `FALSE`, only the portion of the
#' residence events that is within the period of interest will be retained,
#' and `residences` will be recalculated, using specified `units`.
#' Default is `TRUE`.
#'
#' @return a dataframe in the same format as the input data, with residence
#' events limited to the period(s) of interest.
#' @export
#'
#' @examples
#' \dontrun{season(data=data,res.start="ResidenceStart",res.end="ResidenceEnd",
#' residences="ResidenceDuration",units="days",season.start="01-06",
#' season.end="31-10")}
season<-function(data,res.start,res.end,residences,units,season.start,
                 season.end,overlap=TRUE){

  if (type %in% c("actel","vtrack")){
    data<-extractres(data=data,type=type)
  }

  if (type=="manual"&"auto" %in% c(res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    res.start, res.end, residences, and units")
  }

  # Fill in auto fields
  if (res.start=="auto"){
    res.start<-autofield(type=type,field="res.start")
  }
  if (res.end=="auto"){
    res.end<-autofield(type=type,field="res.end")
  }
  if (residences=="auto"){
    residences<-autofield(type=type,field="residences")
  }
  if (units=="auto"){
    units<-autofield(type=type,field="units")
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

  # Checks for format of season
  if (length(season.start)!=length(season.end)){
    stop("season.start must be the same length as season.end")
  }
  if (!is(season.start,"POSIXt")){
    try(season.start<-as.POSIXct(season.start,tz="UTC"),silent=TRUE)
    if (!is(season.start,"POSIXt")&
        length(season.start)!=1&
        all(nchar(season.start)!=5)){
      stop("season.start is not in the format YYYY-mm-dd HH:MM:SS or dd-mm")
    }
  }
  if (!is(season.end,"POSIXt")){
    try(season.end<-as.POSIXct(season.end,tz="UTC"),silent=TRUE)
    if (!is(season.end,"POSIXt")&
        length(season.end)!=1&
        all(nchar(season.end)!=5)){
      stop("season.end is not in the format YYYY-mm-dd HH:MM:SS or dd-mm")
    }
  }
  if (all(class(season.start)!=class(season.end))){
    stop("season.start is not the same format as season.end")
  }

  # If start and end are in dd-mm format, convert to pairs of full dates
  if (!is(season.start,"POSIXt")){
    years<-unique(c(as.POSIXlt(data[[res.start]])$year+1900,
                    as.POSIXlt(data[[res.end]])$year+1900))
    start.m<-substr(season.start,4,5)
    start.d<-substr(season.start,1,2)
    end.m<-substr(season.end,4,5)
    end.d<-substr(season.end,1,2)
    season.start<-as.POSIXct(as.character(),tz="UTC")
    season.end<-as.POSIXct(as.character(),tz="UTC")
    for (i in 1:length(years)){
      season.start<-c(season.start,
                      as.POSIXct(paste0(years[i],"-",start.m,"-",start.d),
                                 tz="UTC"))
      season.end<-c(season.end,
                    as.POSIXct(paste0(years[i],"-",end.m,"-",end.d),
                               tz="UTC"))
    }
  }

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

  data.season<-data[0,]

  # season.break<-data[0,]
  # season.break[1,]<-NA
  # season.break$Station.Name<-"Break"

  for (i in 1:length(season.start)){
    print(paste("season/period",i,"of",length(season.start)))
    pb<-txtProgressBar(1,length(tag),style=3)
    for (j in 1:length(tag)){
      data.temp<-data[0,]
      # Four scenarios:
      # 1 - starts after season start, and ends before season end
      k<-which(data[[ID]]==tag[j]&
                 data[[res.start]]>=season.start[i]&
                 data[[res.end]]<=season.end[i])
      data.temp<-rbind(data.temp,data[k,])
      # 2 - starts before season start, and ends after season end
      k<-which(data[[ID]]==tag[j]&
                 data[[res.start]]<=season.start[i]&
                 data[[res.end]]>=season.end[i])
      data.temp<-rbind(data.temp,data[k,])
      # 3 - starts before season start, and ends after season start and before season end
      k<-which(data[[ID]]==tag[j]&
                 data[[res.start]]<=season.start[i]&
                 data[[res.end]]>=season.start[i]&
                 data[[res.end]]<season.end[i])
      data.temp<-rbind(data.temp,data[k,])
      # 4 - starts after season start and before season end, and ends after season end
      k<-which(data[[ID]]==tag[j]&
                 data[[res.start]]>season.start[i]&
                 data[[res.start]]<=season.end[i]&
                 data[[res.end]]>=season.end[i])
      data.temp<-rbind(data.temp,data[k,])
      if (overlap==FALSE){
        data.temp[[res.start]][data.temp[[res.start]]<season.start[i]]<-season.start[i]
        data.temp[[res.end]][data.temp[[res.start]]>season.end[i]]<-season.end[i]

      }
      data.temp<-data.temp[order(data.temp[[res.start]]),]
      if (nrow(data.temp)>0){
        k<-nrow(data.temp)+1
        data.temp[k,]<-NA
        data.temp[[ID]][k]<-tag[j]
        data.temp[[station]][k]<-"Break"
        data.temp[[res.start]][k]<-data.temp[[res.start]][k-1]+1
      }
      data.season<-rbind(data.season,data.temp)
      setTxtProgressBar(pb,j)
    }
  }

  # Recalculate residences
  data.season[[residences]]<-difftime(data.season[[res.end]],
                                    data.season[[res.start]],
                                    units=units)

  data.season
}
