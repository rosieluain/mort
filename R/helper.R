# ID and station - not for mort
# Not sure of others

#' Fill in field names automatically
#' @description Fills in field names automatically for dataframes of residence
#' events that were generated using common packages for processing acoustic
#' telemetry data. Supported packages are `mort`, `actel`, `glatos`, and
#' `Vtrack`.
#'
#' @param format the format of the dataset to be analyzed. Options are "mort",
#' "actel", "glatos", and "vtrack" (note lowercase "v").
#' @param field the argument for which a field name will be automatically
#' generated. Options are "res.start", "res.end", "residences", and "units".
#' @param data
#'
#' @return a character string with the appropriate name of the field, to be used
#' as an argument in further analyses in `mort`.
#' @export
#'
#' @examples
#' format.auto("actel","units")
#' format.auto("glatos","res.start")
#' format.auto("mort","res.end")
#' format.auto("vtrack","residences")
format.auto<-function(format,field,data){
  # data is optional - just for format="mort" and "units" and "residences"
  # also for actel and vtrack?
  if (is.null(format)){
    stop("format must be specified if 'res.start', 'res.end', 'residences',
         or 'units' are 'auto'. Please specify the format.")
  }
  else if (format=="mort"){
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
  else if (format=="actel"){
    # if (field=="res.start"){
    #   field.name<-"ResidenceStart"
    # }
    # if (field=="res.end"){
    #   field.name<-"ResidenceEnd"
    # }
    # if (field=="residences"){
    #   field.name<-colnames(data)[grep("ResidenceLength",colnames(data))]
    # }
    if (field=="units"){
      field.name<-"mins"
    }
    # # ID?
    # # station?
  }
  else if (format=="glatos"){
    # if (field=="res.start"){
    #   field.name<-"ResidenceStart"
    # }
    # if (field=="res.end"){
    #   field.name<-"ResidenceEnd"
    # }
    # if (field=="residences"){
    #   field.name<-colnames(data)[grep("ResidenceLength",colnames(data))]
    # }
    if (field=="units"){
      field.name<-"secs"
    }
    # # ID?
    # # station?
  }
  else if (format=="vtrack"){
    # if (field=="res.start"){
    #   field.name<-"ResidenceStart"
    # }
    # if (field=="res.end"){
    #   field.name<-"ResidenceEnd"
    # }
    # if (field=="residences"){
    #   field.name<-colnames(data)[grep("ResidenceLength",colnames(data))]
    # }
    if (field=="units"){
      field.name<-"secs"
    }
    # # ID?
    # # station?
  }
  else {
    stop("format is not recognized. Please enter valid format.")
  }

  field.name

}

#### Since actel and Vtrack store residence events in lists, will also need
# to convert the lists to a dataframe to be used... need a function to
# do that
