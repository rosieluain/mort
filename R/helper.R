#' Fill in field names automatically
#' @description Fills in field names automatically for dataframes of residence
#' events that were generated using common packages for processing acoustic
#' telemetry data. Supported packages are `mort`, `actel`, `glatos`, and
#' `Vtrack`.
#'
#' @param type the type of dataset to be analyzed. Options are "mort",
#' "actel", "glatos", and "vtrack" (note lowercase "v").
#' @param field the argument for which a field name will be automatically
#' generated. Options are "res.start", "res.end", "residences", and "units".
#' @param data optional dataframe. Required for type="mort"
#'
#' @return a character string with the appropriate name of the field, to be used
#' as an argument in further analyses in `mort`.
#' @keywords internal
#' @noRd
#'
#' @examples
#' autofield("actel","units")
#' autofield("glatos","res.start")
#' autofield("mort","res.end")
#' autofield("vtrack","residences")
autofield<-function(type,field,data){
  # data is optional - just needed for format="mort", to get "units" and "residences"
  if (is.null(type)){
    stop("type must be specified if 'res.start', 'res.end', 'residences',
         or 'units' are 'auto'. Please specify the format.")
  }
  else if (type=="mort"){
    if (field=="res.start"){
      field.name<-"ResidenceStart"
    }
    if (field=="res.end"){
      field.name<-"ResidenceEnd"
    }
    if (field=="residences"){
      field.name<-colnames(data)[grep("ResidenceLength",colnames(data))]
    }
    if (field=="units"){
      field.name<-sub("ResidenceLength.","",colnames(data)[grep("ResidenceLength",colnames(data))])
    }
  }
  else if (type=="actel"){
    if (field=="res.start"){
      field.name<-"First.time"
    }
    if (field=="res.end"){
      field.name<-"Last.time"
    }
    if (field=="residences"){
      field.name<-"Time.in.array.s"
    }
    if (field=="units"){
      field.name<-"secs"
    }
    if (field=="ID"){
      field.name<-"ID"
    }
    if (field=="station"){
      field.name<-"Array"
    }
  }
  else if (type=="glatos"){
    if (field=="res.start"){
      field.name<-"first_detection"
    }
    if (field=="res.end"){
      field.name<-"last_detection"
    }
    if (field=="residences"){
      field.name<-"res_time_sec"
    }
    if (field=="units"){
      field.name<-"secs"
    }
    if (field=="ID"){
      field.name<-"animal_id"
    }
    if (field=="station"){
      field.name<-"location"
    }
  }
  else if (type=="vtrack"){
    if (field=="res.start"){
      field.name<-"STARTTIME"
    }
    if (field=="res.end"){
      field.name<-"ENDTIME"
    }
    if (field=="residences"){
      field.name<-"DURATION"
    }
    if (field=="units"){
      field.name<-"secs"
    }
    if (field=="ID"){
      field.name<-"TRANSMITTERID"
    }
    if (field=="station"){
      field.name<-"RECEIVERID"
    }
  }
  else {
    stop("type is not recognized. Please enter valid type.")
  }

  field.name

}


#' Extract residences from actel and Vtrack lists
#' @description Extracts residence information from lists that are generated
#' actel and Vtrack.
#'
#' @param data the output from either actel or Vtrack
#' @param type the type of `data`. Options are "actel" or "vtrack" (note
#' lowercase "v").
#'
#' @return the data from the list as a dataframe
#' @import lubridate
#' @importFrom lubridate hms period_to_seconds
#' @keywords internal
#' @noRd
#'
#' @examples
#' \dontrun{extractres(data=data,format="vtrack")}
extractres<-function(data,type){
  if (type=="actel"){
    data.unlisted<-cbind(ID=as.character(),as.data.frame(data$valid.movements[[1]][0,]))
    for (i in 1:length(names(data$valid.movements))){
      j<-nrow(data$valid.movements[[i]])
      k<-nrow(data.unlisted)
      data.unlisted[(k+1):(k+j),]<-NA
      data.unlisted$ID[(k+1):(k+j)]<-rep(names(data$valid.movements[i]),j)
      data.unlisted[(k+1):(k+j),2:ncol(data.unlisted)]<-data$valid.movements[[i]]
    }
    data.unlisted$Time.in.array.s<-period_to_seconds(hms(data.unlisted$Time.in.array))
    warning("If actel date/times are in local time, they will be converted to
            UTC. Verify that time zone in actel output is valid.")
    attributes(data.unlisted$First.time)$tzone<-"UTC"
    attributes(data.unlisted$Last.time)$tzone<-"UTC"
  }
  else if (type=="vtrack"){
    data.unlisted<-data$residences
    warning("Assuming that Vtrack date/times are in UTC. If they are in local
            time, please convert to UTC before running")
    warning("When the duration of an event is 0 s (a single detection) and
            the reason for ending the event (ENDREASON) is 'timeout', Vtrack
            gives DURATION the time between the current event and the next, not
            the duration of the current event. DURATION was recalculated as
            the duration of the events themselves, to better align with other
            packages.")
    data.unlisted$STARTTIME<-as.POSIXct(as.character(data.unlisted$STARTTIME),tz="UTC")
    data.unlisted$ENDTIME<-as.POSIXct(as.character(data.unlisted$ENDTIME),tz="UTC")
    data.unlisted$DURATION<-difftime(data.unlisted$ENDTIME,
                                     data.unlisted$STARTTIME,
                                     units="secs")
  }

  data.unlisted

}



#' Compare specified to default units
#' @description Compare specified units to package default units and issue
#' a warning if there is no match.
#'
#' @param type the method used to generate the residence events. Options are
#' "mort", "glatos", or "vtrack".
#' @param units units of the duration of the residence events in `data`.
#' @param data optional dataframe. Required for type="mort".
#'
#' @return a warning if the specified units do not match the default units.
#' @keywords internal
#' @noRd
#'
#' @examples
#' unitcheck(type="vtrack",units="secs")
#' unitcheck(type="glatos",units="mins")
unitcheck<-function(type,units,data){
  if (type=="mort"){
    if (units!=sub("ResidenceLength.","",colnames(data)[grep("ResidenceLength",colnames(data))])){
      warning("specified units do not match the units in the column name
              of ResidenceLength. Verify that units are correct.")
    }
  }
  else if (type=="glatos"){
    if (units!="secs"){
      warning("specified units do not match the default glatos units ('secs').
              Verify that units are correct")
    }
  }
  else if (type=="vtrack"){
    if (units!="secs"){
      warning("specified units do not match the default Vtrack units ('secs').
              Verify that units are correct")
    }
  }
}

