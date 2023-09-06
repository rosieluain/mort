
#' Plot residence events
#' @description Plot residence events, with the option of plotting identified
#' mortalities. Plotting uses ggplot2. Interactive option also uses plotly.
#'
#' @param data a dataframe of residence events. Residence events must include
#' tag ID, location name, start time, and end time.
#' @param type the method used to generate the residence events. Options are
#' "mort", "actel", "glatos", "vtrack", or "manual".
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location.
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt or character in the format
#' YYYY-mm-dd HH:MM:SS if `type="manual"`.
#' @param morts a dataframe containing potential mortalities. The dataframe must
#' have the same ID, station, res.start, res.end, and residences column names
#' as `data`.
#' @param singles option to adjust the end times of residence events so single
#' detection events are visible. Adjustment is for visualization purposes only
#' and should not obscure or impact the visualization of other residence events.
#' @param interactive option to generate an interactive plot.
#' @param residences an optional character string with the name of the column
#' in `data` that holds the duration of the residence events. Required if
#' generating an interactive plot or applying season.
#' @param units units of the duration of the residence events in `data`.
#' Required if applying season.
#' @param season.start the start date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param season.end the end date/time(s) of the period of interest. If the
#' period of interest is the same in all study years, must be a character string
#' in format "dd-mm". Otherwise, must be in POSIXt, or a character string in
#' format YYYY-mm-dd HH:MM:SS.
#' @param facet option to facet by year or season. If `TRUE`, then `season.start`
#' and `season.end` must be provided.
#' @param facet.axis option to position facets along x or y axis. Options are
#' "x" and "y". Default is "x". Note that `facet.axis` can only be "y" if
#' `facet.by="year"`.
#' @param facet.by option to facet by "season" (as defined with `season.start`
#' and `season.end`) or "year". Default is "season".
#' @param verbose option to display updates and progress bar as function is run.
#' Default is TRUE.
#'
#' @return a ggplot2 plot. Additional arguments (e.g., formatting axes,
#' legend, aes, manual colour scales) can be added as for any ggplot2 plot.
#' If `interactive=TRUE`,
#' returns a plotly plot.
#' @export
#' @importFrom rlang .data
#'
#' @examples
#' plot<-mortsplot(data=events,type="mort",ID="ID",station="Station.Name")
#' plot
#'
#' # With mortalities plotted over residences:
#' morts<-morts(data=events,type="mort",ID="ID",station="Station.Name",
#' method="any",verbose=FALSE)
#'
#' plot<-mortsplot(data=events,type="mort",ID="ID",station="Station.Name",
#' morts=morts)
#' plot
mortsplot<-function(data,type,ID,station,res.start="auto",res.end="auto",
                    morts=NULL,singles=TRUE,interactive=FALSE,residences=NULL,
                    units=NULL,
                    season.start=NULL,season.end=NULL,facet=FALSE,
                    facet.axis="x",facet.by="season",
                    verbose=TRUE){

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" must be installed to use this function.",
         call. = FALSE)
  }
  if (interactive==TRUE){
    if (!requireNamespace("plotly", quietly = TRUE)) {
      stop("Package \"plotly\" must be installed to generate an interactive plot.",
           call. = FALSE)
    }
    ResStart<-ResEnd<-NULL
  }

  if (!(type %in% c("actel","glatos","manual","mort","vtrack"))){
    stop("invalid type. Please enter valid type: 'actel', 'glatos',
    'manual', 'mort', or 'vtrack' (note lowercase 'v')")
  }
  if (type %in% c("actel","vtrack")){
    data<-extractres(data=data,type=type)
  }
  if (type=="manual"&"auto" %in% c(ID,station,res.start,res.end,residences,units)){
    stop("For type='manual', all the following parameters must be specified:
    ID, station, res.start, res.end")
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
  if (!is.null(residences)){
    if (residences=="auto"){
      residences<-autofield(type=type,field="residences",data=data)
    }
  }
  if (!is(data[[res.start]],"POSIXt")){
    try(data[[res.start]]<-as.POSIXct(data[[res.start]],tz="UTC",silent=TRUE))
    if (!is(data[[res.start]],"POSIXt")){
      stop("res.start in data is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is(data[[res.end]],"POSIXt")){
    try(data[[res.end]]<-as.POSIXct(data[[res.end]],tz="UTC",silent=TRUE))
    if (!is(data[[res.end]],"POSIXt")){
      stop("res.end in data is not in the format YYYY-mm-dd HH:MM:SS")
    }
  }
  if (!is.null(morts)){
    if (!is(morts[[res.start]],"POSIXt")){
      try(morts[[res.start]]<-as.POSIXct(morts[[res.start]],tz="UTC",silent=TRUE))
      if (!is(morts[[res.start]],"POSIXt")){
        stop("res.start in morts is not in the format YYYY-mm-dd HH:MM:SS")
      }
    }
    if (!is(morts[[res.end]],"POSIXt")){
      try(morts[[res.end]]<-as.POSIXct(morts[[res.end]],tz="UTC",silent=TRUE))
      if (!is(morts[[res.end]],"POSIXt")){
        stop("res.end in morts is not in the format YYYY-mm-dd HH:MM:SS")
      }
    }
  }

  if (is(data[[station]],"list")){
    data$station2<-as.character(NA)
    for (i in 1:nrow(data)){
      if (length(data[[station]][[i]])==1){
        data$station2[i]<-data[[station]][[i]][1]
      }
      else {
        stn.temp<-unique(data[[station]][[i]])
        stn.temp<-stn.temp[order(stn.temp)]
        stn.temp<-paste("Drift",paste(stn.temp,collapse=" "))
        stn.temp<-gsub(" ","-",stn.temp)
        data$station2[i]<-stn.temp
      }
    }
    station<-"station2"
  }

  if (!is.null(season.start)|
      !is.null(season.end)|
      facet.by=="year"){
    if (is.null(residences)){
      residences<-autofield(type=type,field="residences",data=data)
    }
    if (residences=="auto"){
      residences<-autofield(type=type,field="residences",data=data)
    }
    if (is.null(units)){
      units<-autofield(type=type,field="units",data=data)
    }
    if (units=="auto"){
      units<-autofield(type=type,field="units",data=data)
    }
    if (verbose==TRUE){
      print("Extracting data from the period/season(s) of interest")
    }
    if (is.null(season.start)&is.null(season.end)&facet.by=="year"){
      season.start<-"01-01"
      season.end<-"31-12"
    }
    data<-season(data=data,type=type,ID=ID,station=station,res.start=res.start,res.end=res.end,
                 residences=residences,units=units,season.start=season.start,
                 season.end=season.end,overlap=FALSE,verbose=verbose)
    data<-data[data[[station]]!="Break",]
    if (!is(season.start,"POSIXt")){
      try(season.start<-as.POSIXct(season.start,tz="UTC"),silent=TRUE)
    }
    if (!is(season.end,"POSIXt")){
      try(season.end<-as.POSIXct(season.end,tz="UTC"),silent=TRUE)
    }
    years<-unique(c(as.POSIXlt(data[[res.start]])$year+1900,
                    as.POSIXlt(data[[res.end]])$year+1900))
    if (!is(season.start,"POSIXt")){
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
    ssn<-data.frame(Start=season.start,End=season.end)
    ssn<-ssn[order(ssn$Start),]
    if (!is.null(morts)){
      if (verbose==TRUE){
        print("Extracting morts from the period/season(s) of interest")
      }
      morts.ssn<-season(data=morts,ID=ID,station=station,res.start=res.start,
                        res.end=res.end,
                   residences=residences,units=units,season.start=season.start,
                   season.end=season.end,overlap=FALSE)
      morts.ssn<-morts.ssn[morts.ssn[[station]]!="Break",]
      if (nrow(morts.ssn)<nrow(morts)){
        # Then some morts were identified outside of the season of interest
        # Need to shift mort to next season start
        j<-which(!(morts[[ID]] %in% morts.ssn[[ID]]))
        for (i in 1:length(j)){
          k<-which(ssn$Start>=morts[[res.start]][i])
          if (length(k)>0){
            k<-min(k)
            m<-nrow(morts.ssn)+1
            morts.ssn[m,]<-morts[j[i],]
            morts.ssn[[res.start]][m]<-ssn$Start[k]
            morts.ssn[[res.end]][m]<-ssn$End[k]
          }
        }
      }
      morts<-morts.ssn
    }

    if (facet==TRUE){
      # If there is more than one season
      if (nrow(ssn)>1){
        if (facet.by=="year"){
          data$Year<-as.numeric(NA)
          if (!is.null(morts)){
            morts$Year<-as.numeric(NA)
          }
          for (i in 1:length(years)){
            data$Year[(as.POSIXlt(data[[res.start]])$year+1900)==years[i]]<-years[i]
            if (!is.null(morts)){
              morts$Year[(as.POSIXlt(morts[[res.start]])$year+1900)==years[i]]<-years[i]
            }
          }
          if (facet.axis=="y"){
            # Make a date with the same year for plotting
            start.og<-paste0(res.start,".og")
            end.og<-paste0(res.end,".og")
            data$Start.og<-data[[res.start]]
            data$End.og<-data[[res.end]]
            data[[res.start]]<-as.character(data[[res.start]])
            data[[res.end]]<-as.character(data[[res.end]])
            if (!is.null(morts)){
              morts$Start.og<-morts[[res.start]]
              morts$End.og<-morts[[res.end]]
              morts[[res.start]]<-as.character(morts[[res.start]])
              morts[[res.end]]<-as.character(morts[[res.end]])
            }
            for (i in 1:length(years)){
              if (any(leap_year(years))){
                k<-which(leap_year(years))
                if (length(k)>1){
                  k<-min(k)
                }
              }
              else {k<-1}
              if (k!=i){
                data[[res.start]]<-sub(years[i],years[k],data[[res.start]])
                data[[res.end]]<-sub(years[i],years[k],data[[res.start]])
                if (!is.null(morts)){
                  morts[[res.start]]<-sub(years[i],years[k],morts[[res.start]])
                  morts[[res.end]]<-sub(years[i],years[k],morts[[res.start]])
                }
              }
            }
            data[[res.start]]<-as.POSIXct(data[[res.start]],tz="UTC")
            data[[res.end]]<-as.POSIXct(data[[res.end]],tz="UTC")
            if (!is.null(morts)){
              morts[[res.start]]<-as.POSIXct(morts[[res.start]],tz="UTC")
              morts[[res.end]]<-as.POSIXct(morts[[res.end]],tz="UTC")
            }
          }
        }
        # Otherwise, there are multiple seasons of interest within a year
        # or user wants to facet by season
        else {
          data$Season<-as.numeric(NA)
          if (!is.null(morts)){
            morts$Season<-as.numeric(NA)
          }
          for (i in 1:nrow(ssn)){
            data$Season[data[[res.start]]>=ssn$Start[i]&
                          data[[res.end]]<=ssn$End[i]]<-i
            if (!is.null(morts)){
              morts$Season[morts[[res.start]]>=ssn$Start[i]&
                             morts[[res.end]]<=ssn$End[i]]<-i
            }
          }
          data$Season<-paste("Season",data$Season)
          if (!is.null(morts)){
            morts$Season<-paste("Season",morts$Season)
          }
        }
      }
    }
  }

  # Scale linewidth based on number of IDs
  if (facet==TRUE){
    if (facet.axis=="y"){
      if ((length(unique(data[[ID]]))*length(years))<150){
        lw<-((length(unique(data[[ID]]))*length(years))-200)/-50
      }
      else {lw<-1}
    }
    else {
      if (length(unique(data[[ID]]))<150){
        lw<-(length(unique(data[[ID]]))-200)/-50
      }
      else {
        lw<-1
      }
    }
  }
  else {
    if (length(unique(data[[ID]]))<150){
      lw<-(length(unique(data[[ID]]))-200)/-50
    }
    else {
      lw<-1
    }
  }

  if (singles==TRUE){
    # Add a brief period to res.end, so residences with 0 detections are visible
    if (facet==TRUE){
      ssn$Length<-difftime(ssn$End,ssn$Start,units="days")
      if (facet.axis=="y"){
        resadd<-max(as.numeric(ssn$Length))*10
      }
      else {
        resadd<-sum(as.numeric(ssn$Length))*10
      }
    }
    else {
      resadd<-as.numeric(difftime(max(data[[res.end]]),
                                  min(data[[res.start]]),
                                  units="days"))*10
    }
  }
  else {
    resadd<-0
  }

  if (interactive==TRUE){
    data$ResStart<-as.character(data[[res.start]])
    data$ResEnd<-as.character(data[[res.end]])
    if (!is.null(morts)){
      morts$ResStart<-as.character(morts[[res.start]])
      morts$ResEnd<-as.character(morts[[res.end]])
    }
  }

  plot<-ggplot2::ggplot(data,ggplot2::aes(x=.data[[res.start]],y=as.factor(.data[[ID]])))+
                 ggplot2::geom_segment(ggplot2::aes(xend=.data[[res.end]]+resadd,
                                  yend=as.factor(.data[[ID]]),
                                  colour=.data[[station]]),
                              linewidth=lw)+
    ggplot2::ylab("")+
    ggplot2::xlab("")+
    ggplot2::theme_bw()

  if (facet==TRUE){
    if (!is.null(data$Year)&facet.axis=="y"){
      plot<-plot+
        ggplot2::facet_grid(Year~.,scales="free",space="free_y")+
        ggplot2::scale_x_datetime(date_labels = "%b %d")
    }
    else if (!is.null(data$Year)&facet.axis=="x"){
      plot<-plot+
        ggplot2::facet_grid(.~Year,scales="free",space="free_x")+
        ggplot2::scale_x_datetime(date_labels = "%b %d")
    }
    else if (!is.null(data$Season)){
      plot<-plot+
        ggplot2::facet_grid(.~Season,scales="free",space="free_x")
    }
  }

  if (!is.null(morts)){
    plot<-plot+
      ggplot2::geom_point(data=morts,ggplot2::aes(x=.data[[res.start]],y=as.factor(.data[[ID]])))

  }

  if (interactive==TRUE){
    if (is.null(residences)){
      plot<-plot+ggplot2::aes(text=paste("ID:",.data[[ID]],"</br>",
                                         "</br>Start:",ResStart,
                                         "</br>End:",ResEnd,
                                         "</br>Station:",.data[[station]]))
    }
    else {
      plot<-plot+ggplot2::aes(text=paste("ID:",.data[[ID]],"</br>",
                                         "</br>Start:",ResStart,
                                         "</br>End:",ResEnd,
                                         "</br>Duration:",.data[[residences]],
                                         "</br>Station:",.data[[station]]))
    }
    plot.int<-plotly::ggplotly(plot,tooltip="text")
    plot.int
  }
  else {
    plot
  }

}

