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
#' "", "", or "all"
#' @param season a dataframe with start and end dates of the season(s) of interest
#' @param ID a string of the name of the column in `data` that holds the tag or
#' sample IDs.
#' @param station a string of the name of the column in `data` that holds the
#' station name or receiver location
#' @param res.start a string of the name of the column in `data` that holds the
#' start date and time. Must be specified and in POSIXt if `format="manual"`.
#' @param res.end a string of the name of the column in `data` that holds the
#' end date and time. Must be specified and in POSIXt if `format="manual"`.
#'
#' @return a dataframe with...
#' @export
#'
#' @examples
#' \dontrun{mort(data=res.events,format="manual",units="days",residences="ResidenceLength")}
mort<-function(data,format="mort",ID,station,res.start="auto",res.end="auto",
               method="all",units="auto",residences="auto",season=NULL){
  if (type=="mort"){
    units=sub("ResidenceLength.","",colnames(data)[grep("ResidenceLength",colnames(data))])
  }
  else if (type=="actel"){
    units="mins"
  }
  else if (type=="glatos"){
    units="secs"
  }
  else if (type=="vtrack"){
    units="secs"
  }
  else if (type=="manual"){
    # Check that units were provided
  }
  # if (is.null(season)){
  #
  # }
  # else {
  #
  # }

  # Get list of unique tag IDs
  tag<-unique(data[[ID]])

}

#### Hold off on this until see how it is used ####
# Make dataframe of last detections for char
char.lastdetect<-lastdetect[lastdetect$Species=="ARCH",]
# Find length of residence at last detection location
lastres<-residences[0,]
for (i in 1:nrow(char.lastdetect)){
  lastres[nrow(lastres)+1,]<-residences[residences$ID==char.lastdetect$ID[i]&
                                          residences$EndUTC==char.lastdetect$DateTimeUTC[i],]
}

#### Hold off on this until see how it used ####
# Read in dead drift direction file
ddd<-read.csv("Data/Dead drift direction.csv",stringsAsFactors=FALSE)
colnames(ddd)[grepl("FrStation",colnames(ddd))]<-"FrStation"

#### Update resmorts file ####
# If resmorts.csv exists from earlier data download
if ("resmorts.csv" %in% list.files(path="Data")){
  resmorts.old<-read.csv("Data/resmorts.csv")
  resmorts.old$StartUTC<-as.POSIXct(resmorts.old$StartUTC,
                                    format="%Y-%m-%d %H:%M:%S",
                                    tz="UTC")
  resmorts.old$EndUTC<-as.POSIXct(resmorts.old$EndUTC,
                                  format="%Y-%m-%d %H:%M:%S",
                                  tz="UTC")
  # Go through each entry
  for (i in 1:nrow(resmorts.old)){
    res.temp<-residences[residences$ID==resmorts.old$ID[i],]
    # If there are detections since EndUTC
    if (res.temp$EndUTC[nrow(res.temp)]>resmorts.old$EndUTC[i]){
      # Find which residence matches previously identified mortality
      j<-which((res.temp$EndUTC==resmorts.old$EndUTC[i])|
                 (res.temp$StartUTC==resmorts.old$EndUTC[i]))
      # If there are no matches, match starts (if detected continuously,
      # and single residence, will have same start but new end)
      if (length(j)==0){
        j<-which(res.temp$StartUTC==resmorts.old$StartUTC[i])
      }
      # Identify if station change after this residence
      repeat{
        if (nrow(res.temp)>=(j+1)){
          # If station in next row is the same as the identified mortality
          if (res.temp$Station.Name[j+1]==res.temp$Station.Name[j]){
            # Move to the next row
            j<-j+1
          }
          # If the current station has dead drifting direction assigned
          else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
            # Find which dead drifting directions were assigned
            k<-which(ddd$FrStation==res.temp$Station.Name[j])
            # If the next station is one of the matching ToStations
            if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
              # Move to the next row
              j<-j+1
            }
            else {
              print(paste(resmorts.old$ID[i],"has a station change"))
              break
            }
          }
          # Otherwise, there is a station change
          else {
            # If these are on a different station
            # Then flag for review
            print(paste(resmorts.old$ID[i],"has a station change"))
            break
          }
        }
        # At the end of the residence record, with no station change
        # Break loop. Identified mortality is unchanged
        else {break}
      }
    }
  }
}

#### Longest residence time followed by station change ####
# Set up empty data frame that will hold maximum residence periods
# for each char
res.max<-residences[0,]

pb<-txtProgressBar(1,length(char),style=3)
for (i in 1:length(char)){
  # Subset residences from char ID i
  res.temp<-residences[residences$ID==char[i],]
  # Remove single detections, except for manual record types
  # This is so chance detections don't affect mortality
  # estimate (conservative)
  res.temp<-res.temp[((res.temp$ResTime!=0)|(res.temp$Receiver %in% manual.record.types)),]
  # If there are at least two records (i.e., not just a tag record)
  if (nrow(res.temp)>=2){
    # Set j to second last record
    j<-nrow(res.temp)-1
    repeat {
      if (j>0){ # Redundant for first run-through, but back-up for other runs
        # If residences from j to the end are at the same station,
        # change j to previous record
        if (length(unique(res.temp$Station.Name[j:nrow(res.temp)]))==1){
          j<-j-1
        }
        # Dead drift direction or overlapping station
        else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
          # Find which dead drifting station
          k<-which(ddd$FrStation==res.temp$Station.Name[j])
          # If the next station is one of the matching ToStations
          if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
            # Move to the previous row
            j<-j-1
          }
          else {
            # Otherwise, there is a station change between j and j+1
            break
          }
        }
        # # If the last Station.Name is in either Station A or B in row j
        # else if (res.temp$Station.Name[nrow(res.temp)] %in% res.temp[j,]){
        #   j<-j-1
        # }
        # # If there is a Station.NameB in the last row, and
        # # last Station.NameB is in either Station A or B in row j
        # else if (!(is.na(res.temp$Station.NameB[nrow(res.temp)]))&
        #          res.temp$Station.NameB[nrow(res.temp)] %in% res.temp[j,]){
        #   j<-j-1
        # }
        # # Station names (A and/or B) in last row do not match any
        # # station names in row j
        # Otherwise, there is a station change between row j and j+1
        else {
          break
        }
      }
      else {break}
    }
    # If at least one residence time before the station change is
    # > 0 (not a single detection, and not a manual record type)
    # Could change to just manual record type, because already
    # removed single detections above
    if (max(res.temp$ResTime[1:(j)])!=0){
      # Identify the longest residence time before the station change
      k<-which(res.temp$ResTime==max(res.temp$ResTime[1:(j)]))
      # Add longest residence time to res.max
      res.max[nrow(res.max)+1,]<-res.temp[k,]
    }
  }
  setTxtProgressBar(pb,i)
}
attributes(res.max$StartUTC)$tzone<-"UTC"
attributes(res.max$EndUTC)$tzone<-"UTC"

max(res.max$ResTime)
# 93 days - same after changing residences/Station.B

#### Longest residence time total ####
if ("resmorts.old" %in% names(.GlobalEnv)){
  res.morts<-resmorts.old
} else {res.morts<-lastres[0,]}
# List cutoff in days
res.total.cutoff<-200
# If any residences are longer than the cutoff
if (any(residences$ResTime[!(residences$ID %in% res.morts$ID)]>=res.total.cutoff)){
  # Identify which are longer and run through each one
  for (i in which(residences$ResTime>=res.total.cutoff)){
    res.temp<-residences[residences$ID==residences$ID[i],]
    j<-which(res.temp$ResTime==max(res.temp$ResTime))
    repeat{
      if (nrow(res.temp)>=j+1){
        # If station in next row is the same as the identified mortality
        if (res.temp$Station.Name[j+1]==res.temp$Station.Name[j]){
          # Move to the next row
          j<-j+1
        }
        # If the current station has dead drifting direction assigned
        else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
          # Find which dead drifting directions were assigned
          k<-which(ddd$FrStation==res.temp$Station.Name[j])
          # If the next station is one of the matching ToStations
          if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
            # Move to the next row
            j<-j+1
          }
        }
        else {break}
      }
      else {
        # Add to res.morts
        res.morts[nrow(res.morts)+1,]<-res.temp[j,]
        break
      }
    }
  }
}


#### Identify most recent ice-free station change ####
# # Restrict residences to ice-free season (June to September, inclusive)
# residences.char.summer<-residences[(residences$Species=="ARCH")&
#                                      (as.POSIXlt(residences$StartUTC)$mon %in% c(5:8))|
#                                      (as.POSIXlt(residences$EndUTC)$mon %in% c(5:8)),]

# Restrict residences to ice-free season, using freeze/break-up dates
# Read in file of river freeze-up and break-up dates
river.winter<-read.csv("Data/river_ice.csv")
river.winter$Break<-as.POSIXct(river.winter$Break,format="%d-%b-%y",
                               tz="UTC",usetz=TRUE)
river.winter$Freeze<-as.POSIXct(river.winter$Freeze,format="%d-%b-%y",
                                tz="UTC",usetz=TRUE)
# river.winter was meant for subsetting winter detections
# Need to re-arrange data frame so it works for summer detections
river.summer<-data.frame(Year=c(river.winter$Year,(river.winter$Year[nrow(river.winter)]+1)),
                         Break=c(as.POSIXct(paste0(river.winter$Year[1],"-01-01"),format="%Y-%m-%d",tz="UTC",usetz=TRUE),river.winter$Break),
                         Freeze=c(river.winter$Freeze,as.POSIXct(paste0((river.winter$Year[nrow(river.winter)]+1),"-12-31"),format="%Y-%m-%d",tz="UTC",usetz=TRUE)))
attributes(river.summer$Break)$tzone<-"UTC"
attributes(river.summer$Freeze)$tzone<-"UTC"
# If there are placeholders, there may be more years included than
# is possible
j<-as.numeric()
for (i in 1:nrow(river.summer)){
  if (river.summer$Year[i]>max(as.POSIXlt(river.summer$Break)$year+1900)){
    j<-c(j,i)
  }
}
if (length(j)>0){
  river.summer<-river.summer[-j,]
}

# Make separate data frame with ice-free/summer residences only
# Note that residences must be wholly within the ice-free season
residences.char.summer<-residences[0,]
for (i in 1:nrow(river.summer)){
  residences.char.summer<-rbind(residences.char.summer,
                                residences[residences$Species=="ARCH"&
                                             (as.POSIXlt(residences$StartUTC)$year+1900)==river.summer$Year[i]&
                                             residences$StartUTC>=river.summer$Break[i]&
                                             residences$EndUTC<=river.summer$Freeze[i],])
}

stn.change.summer<-residences[0,]
pb<-txtProgressBar(1,length(char),style=3)
for (i in 1:length(char)){
  # Subset summer residences for char ID i
  res.temp<-residences.char.summer[residences.char.summer$ID==char[i],]
  # Remove single detections, including manual detection types
  # This is so chance detections don't affect mortality
  # estimate (conservative)
  res.temp<-res.temp[res.temp$ResTime!=0,]
  # # If a Station.Name ends with a W or WB (indicating winter location),
  # # remove the W or WB, so it does not look like a station change
  # # Find which end in W
  # k<-which(substr(res.temp$Station.Name,nchar(res.temp$Station.Name),nchar(res.temp$Station.Name))=="W")
  # for (m in 1:length(k)){
  #   # Remove the W from each
  #   res.temp$Station.Name[k[m]]<-substr(res.temp$Station.Name[k[m]],1,nchar(res.temp$Station.Name[k[m]])-1)
  # }
  # # Find which end in WB
  # k<-which(substr(res.temp$Station.Name,(nchar(res.temp$Station.Name)-1),nchar(res.temp$Station.Name))=="WB")
  # for (m in 1:length(k)){
  #   # Remove the W from each
  #   res.temp$Station.Name[k[m]]<-substr(res.temp$Station.Name[k[m]],1,nchar(res.temp$Station.Name[k[m]])-2)
  # }
  # # Do the same for Station.NameB
  # k<-which(substr(res.temp$Station.NameB,nchar(res.temp$Station.NameB),nchar(res.temp$Station.NameB))=="W")
  # for (m in 1:length(k)){
  #   res.temp$Station.NameB[k[m]]<-substr(res.temp$Station.NameB[k[m]],1,nchar(res.temp$Station.NameB[k[m]])-1)
  # }
  # k<-which(substr(res.temp$Station.NameB,(nchar(res.temp$Station.NameB)-1),nchar(res.temp$Station.NameB))=="WB")
  # for (m in 1:length(k)){
  #   res.temp$Station.NameB[k[m]]<-substr(res.temp$Station.NameB[k[m]],1,nchar(res.temp$Station.NameB[k[m]])-2)
  # }
  # If there are at least two records
  if (nrow(res.temp)>=2){
    # Set j to second last record
    j<-nrow(res.temp)-1
    repeat {
      if (j>0){
        # If residences at j and j+1 are at the same station
        if (res.temp$Station.Name[j]==res.temp$Station.Name[j+1]){
          j<-j-1
        }
        # Dead drift direction or overlapping station
        else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
          # Find which dead drifting station
          k<-which(ddd$FrStation==res.temp$Station.Name[j])
          # If the next station is one of the matching ToStations
          if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
            # Move to the previous row
            j<-j-1
          }
          else {
            # Otherwise, there is a station change between j and j+1
            break
          }
        }
        # # If the last Station.Name is in either Station A or B in row j
        # else if (res.temp$Station.Name[nrow(res.temp)] %in% res.temp[j,]){
        #   j<-j-1
        # }
        # # If there is a Station.NameB in the last row, and
        # # last Station.NameB is in either Station A or B in row j
        # else if (!(is.na(res.temp$Station.NameB[nrow(res.temp)]))&
        #          res.temp$Station.NameB[nrow(res.temp)] %in% res.temp[j,]){
        #   j<-j-1
        # }
        # # Station names (A and/or B) in last row do not match any
        # # station names in row j
        # This means that there is a station change between row j and j+1
        else {break}
      }
      else {break}
    }
    # # If at least one residence time before the station change is
    # # > 0 (not a single detection, and not a manual record type)
    # # Don't think this is necessary
    # if (max(res.temp$ResTime[1:j])!=0){
    stn.change.summer[nrow(stn.change.summer)+1,]<-res.temp[(j+1),]
    # }
  }
  else if (nrow(res.temp==1)){
    stn.change.summer[nrow(stn.change.summer)+1,]<-res.temp[1,]
  }
  setTxtProgressBar(pb,i)
}

#### Longest ice-free residence time followed by a station change ####
res.max.summer<-residences.char.summer[0,]

for (i in 1:nrow(stn.change.summer)){
  # Subset summer residences for char ID i, where
  # StartUTC < StartUTC of station change
  res.temp<-residences.char.summer[residences.char.summer$ID==stn.change.summer$ID[i]&
                                     residences.char.summer$StartUTC<stn.change.summer$StartUTC[i],]
  j<-which(res.temp$ResTime==max(res.temp$ResTime))
  for (k in 1:length(j)){
    res.max.summer[nrow(res.max.summer)+1,]<-res.temp[j[k],]
  }
}

max(res.max.summer$ResTime)
# 37.4 in 2021 and spring 2022 - same after changing residences/Station.B
# Before 2021 data, was 34.2


#### Identify mortalities based on length of most recent residence ####
# # If the most recent summer residence time is greater than the maximum
# # summer residence time (that was later followed by a station change)
# # and if it started in June to September
# # or if it ended after June,
# # then identify it as a mortality
# res.morts<-lastres[lastres$ResTime>max(res.max.summer$ResTime)&
#                      (as.POSIXlt(lastres$StartUTC)$mon %in% c(5:8)|
#                      as.POSIXlt(lastres$EndUTC)$mon>5),]
# # 8305 is not much more than the cutoff, and it was in late
# # fall, but then was never seen again
# # 8856 probably died overwinter, and then was found again in 2021
# # (slightly downstream)
# # 8921 probably died earlier (and drifted slightly downstream)
# # ### No longer identifies 8304 as a mortality - moved downstream
# # and only observed briefly at downstream location ####

# If the most recent summer residence time is greater than the maximum
# summer residence time (that was later followed by a station change)
# and if it started after break-up in that year and ended before
# freeze-up in that year, then identify as a mortality
for (i in 1:nrow(lastres)){
  if (!(lastres$ID[i] %in% res.morts$ID)&
      lastres$ResTime[i]>max(res.max.summer$ResTime)&
      lastres$StartUTC[i]>river.summer$Break[(as.POSIXlt(lastres$StartUTC[i])$year+1900)==river.summer$Year]&
      lastres$EndUTC[i]<river.summer$Freeze[(as.POSIXlt(lastres$StartUTC[i])$year+1900)==river.summer$Year]){
    res.morts[(nrow(res.morts)+1),]<-lastres[i,]
  }
}
# Fall 2022 - adds 16224
#### Note this does not identify some that were previously identified
# because residences must be wholly within ice-free period
# If these are not identified later on (see notes in Word doc),
# then could change to be Start after Break and before Freeze OR
# End after Break and before Freeze
# res.mortsb has the residences that used to be identified at this point

#### Long residence, break, short residence at same station ####
# This is different than the cumulative residence period, because
# it is possible to have a long residence, then a break, then a
# short residence that, in total length, is less than the cumulative
# residence cut-off
# Note this code was not updated for combined residences

# Identify char that have summer residences longer than
# the cutoff (identified above)
char.longres<-unique(residences.char.summer$ID[residences.char.summer$ResTime>max(res.max.summer$ResTime)])
if (length(char.longres)>0){
  for (i in 1:length(char.longres)){
    #### Should this be restricted to summer residences at this point?
    res.temp<-residences[residences$ID==char.longres[i],]
    # res.temp<-residences.char.summer[residences.char.summer$ID==char.longres[i],]
    # # If a Station.Name ends with a W (indicating winter location),
    # # remove the W, so it does not look like a station change
    # # Find which end in W
    # k<-which(substr(res.temp$Station.Name,nchar(res.temp$Station.Name),nchar(res.temp$Station.Name))=="W")
    # for (m in 1:length(k)){
    #   # Remove the W from each
    #   res.temp$Station.Name[k[m]]<-substr(res.temp$Station.Name[k[m]],1,nchar(res.temp$Station.Name[k[m]])-1)
    # }
    # Find which summer residence has the maximum ResTime
    j<-which(res.temp$ResTime==max(residences.char.summer$ResTime[residences.char.summer$ID==char.longres[i]]))
    # If the longest ResTime is not the final residence
    if (j<nrow(res.temp)){
      # If all subsequent residences are at the same station
      if (all(res.temp$Station.Name[(j+1):nrow(res.temp)]==res.temp$Station.Name[j])){
        # If ID is already in res.morts
        if (char.longres[i] %in% unique(res.morts$ID)){
          # Identify which row ID is in res.morts
          k<-which(res.morts$ID==char.longres[i])
          # If longest residence is earlier than the one
          # in res.morts
          if (res.temp$StartUTC[j]<res.morts$StartUTC[k])
            # Adjust the StartUTC of the mortality period
            res.morts[k,]<-res.temp[j,]
        }
        # If ID is not yet in res.morts
        else {
          # Add to res.morts
          res.morts[nrow(res.morts)+1,]<-res.temp[j,]
        }
      }
      # Otherwise, there are station changes
      # Check if station changes are from dead drift direction
      else {
        # Mark starting row as m, in case need to add to res.morts
        m<-j
        repeat {
          if (j<nrow(res.temp)){
            if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
              # Find which dead drifting directions were assigned
              k<-which(ddd$FrStation==res.temp$Station.Name[j])
              # If the next station is one of the matching ToStations
              if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
                # Move to the next row
                j<-j+1
              }
              # If there is a station not in ddd, then break and do nothing
              else {break}
            }
            # If the current station is not in ddd, then break
            else {break}
          }
          # If at the last station (and everything ddd), then add to
          # res.morts
          else {
            # If ID is already in res.morts
            if (char.longres[i] %in% unique(res.morts$ID)){
              # Identify which row ID is in res.morts
              k<-which(res.morts$ID==char.longres[i])
              # If longest residence is earlier than the one
              # in res.morts
              if (res.temp$StartUTC[m]<res.morts$StartUTC[k])
                # Adjust the StartUTC of the mortality period
                res.morts[k,]<-res.temp[m,]
            }
            # If ID is not yet in res.morts
            else {
              # Add to res.morts
              res.morts[nrow(res.morts)+1,]<-res.temp[m,]
            }
            break
          }
        }
      }
    }
  }
}
# Moves 8857 earlier
# Adds 8280, 8571, 8879, 8270,
# Adds 8304, 8537 (not previously added)

# Did not use this next part for revamp, because think redundant
# # Find all residences that are longer than the cutoff
# # Different than above, in that it is looking for all
# # residences that are longer (not just the longest)
# # Probably redundant (because of later code that goes
# # backwards in time) - but needs testing
# if (length(char.longres)>0){
#   for (i in 1:length(char.longres)){
#     res.temp<-residences.char.summer[residences.char.summer$ID==char.longres[i],]
#     # Identify all residences longer than the cutoff
#     longres<-which(res.temp$ResTime>max(res.max.summer$ResTime))
#     # Loop through all residences that are longer than the cutoff
#     for (j in 1:length(longres)){
#       # If the longest residence is not the last residence and if all
#       # subsequent residences are at the same station
#       if (longres[j]<nrow(res.temp)&
#           all(res.temp$Station.Name[(longres[j]+1):nrow(res.temp)]==res.temp$Station.Name[longres[j]])){
#         # If ID already in res.morts
#         if (char.longres[i] %in% unique(res.morts$ID)){
#           # Identify which row ID is in res.morts
#           k<-which(res.morts$ID==char.longres[i])
#           # If the first long residence is earlier than the one in res.morts
#           if (res.temp$StartUTC[longres[j]]<res.morts$StartUTC[k]){
#             res.morts[k,]<-res.temp[longres[j],]
#           }
#         }
#         # If ID is not yet in res.morts
#         else {
#           # Add to res.morts
#           res.morts[nrow(res.morts)+1,]<-res.temp[longres[j],]
#         }
#       }
#     }
#   }
# }
# # Moves 8879 earlier (correct)
# # Adds 8541 (should be earlier - but combined stations)
# # Used to add fish 8571

#### Cumulative ice-free residence time (total time at one station before station change) ####
res.summer.cum<-residences[0,]
# # Original (commented out) included Station B
# pb<-txtProgressBar(1,nrow(stn.change.summer),style=3)
# for (i in 1:nrow(stn.change.summer)){
#   # Subset summer residences for char ID i, where
#   # StartUTC < StartUTC of station change
#   res.temp<-residences.char.summer[residences.char.summer$ID==stn.change.summer$ID[i]&
#                                      residences.char.summer$StartUTC<stn.change.summer$StartUTC[i],]
#   # Remove single detections (includes manual record types)
#   res.temp<-res.temp[res.temp$ResTime!=0,]
#   # Start at beginning and see if station name (either A or B) is
#   # the same and also if the year is the same
#   # Set j = 1 (for row 1)
#   j<-1
#   repeat {
#     # If there are more than j rows
#     if (nrow(res.temp)>=j){
#       # Set k = j to mark beginning
#       k<-j
#       repeat{
#         # If the first station name is in station A or B of
#         # the next record, and both records are in the same year
#         if ((res.temp$Station.Name[j] %in% c(res.temp$Station.Name[j+1],res.temp$Station.NameB[j+1]))&
#             (as.POSIXlt(res.temp$StartUTC[j])$year==as.POSIXlt(res.temp$StartUTC[j+1])$year)){
#           # Go to next row
#           j<-j+1
#         }
#         # If there is a station B in the first record and that station
#         # is in station A or B of the next record, and both records
#         # are in the same year
#         else if ((!(is.na(res.temp$Station.NameB[j])))&
#                  (res.temp$Station.NameB[j] %in% c(res.temp$Station.Name[j+1],res.temp$Station.NameB[j+1]))&
#                  (as.POSIXlt(res.temp$StartUTC[j])$year==as.POSIXlt(res.temp$StartUTC[j+1])$year)){
#           # Go to next row
#           j<-j+1
#         }
#         # Otherwise, there is station change after j
#         else {
#           # Add record j (before station change)
#           res.summer.cum[nrow(res.summer.cum)+1,]<-res.temp[j,]
#           # If the last station change was before j (as marked by k)
#           if (k!=j){
#             # Adust StartUTC so it is the start of the cumulative residence
#             res.summer.cum$StartUTC[nrow(res.summer.cum)]<-res.temp$StartUTC[k]
#           }
#           # Go to next row
#           j<-j+1
#           break
#         }
#       }
#     }
#     else {break}
#   }
#   setTxtProgressBar(pb,i)
# }
# res.summer.cum<-res.summer.cum[,-which(colnames(res.summer.cum)=="ResTime")]
# res.summer.cum$ResTimeCum<-difftime(res.summer.cum$EndUTC,
#                                     res.summer.cum$StartUTC,
#                                     units="days")

pb<-txtProgressBar(1,nrow(stn.change.summer),style=3)
for (i in 1:nrow(stn.change.summer)){
  # Subset summer residences for char ID i, where
  # StartUTC < StartUTC of station change
  res.temp<-residences.char.summer[residences.char.summer$ID==stn.change.summer$ID[i]&
                                     residences.char.summer$StartUTC<stn.change.summer$StartUTC[i],]
  # Remove single detections (includes manual record types)
  res.temp<-res.temp[res.temp$ResTime!=0,]
  # Start at beginning and see if station name (either A or B) is
  # the same and also if the year is the same
  # Set j = 1 (for row 1)
  j<-1
  repeat {
    # If there are more than or equal (single residence) to j rows
    if (nrow(res.temp)>=j){
      # Set k = j to mark beginning
      k<-j
      repeat{
        # If there are multiple residences and the first station
        # name is the same as the next record, and both records
        # are in the same year
        if (nrow(res.temp)>j&
            (res.temp$Station.Name[j]==res.temp$Station.Name[j+1])&
            (as.POSIXlt(res.temp$StartUTC[j])$year==as.POSIXlt(res.temp$StartUTC[j+1])$year)){
          # Go to next row
          j<-j+1
        }
        # Otherwise, there is station change after j
        else {
          # Add record j (before station change)
          res.summer.cum[nrow(res.summer.cum)+1,]<-res.temp[j,]
          # If the last station change was before j (as marked by k)
          if (k!=j){
            # Adust StartUTC so it is the start of the cumulative residence
            res.summer.cum$StartUTC[nrow(res.summer.cum)]<-res.temp$StartUTC[k]
          }
          # Go to next row
          j<-j+1
          break
        }
      }
    }
    else {break}
  }
  setTxtProgressBar(pb,i)
}
res.summer.cum<-res.summer.cum[,-which(colnames(res.summer.cum)=="ResTime")]
res.summer.cum$ResTimeCum<-difftime(res.summer.cum$EndUTC,
                                    res.summer.cum$StartUTC,
                                    units="days")

max(res.summer.cum$ResTimeCum)
# 63.2 days
# Valid, but at the Kugaryuak - may want to exclude certain
# locations such as this or COP-U-R-X
# Longest excluding these sites is 47.8 - a weird fish at COP-L-R-6,
# so leave for now

#### Identify cumulative residence time at last station ####
# Calculate length of time from last station change to end of
# observations
# Make new stn.change.summer dataframe, because will be adjusting
# the end times (to calculate cumulative residence times)
stn.change.summer.cum<-stn.change.summer
# Change name of column ResTime to ResTimeCum
colnames(stn.change.summer.cum)[which(colnames(stn.change.summer.cum)=="ResTime")]<-"ResTimeCum"

for (i in 1:nrow(stn.change.summer.cum)){
  # Subset summer residences for char ID i
  res.temp<-residences.char.summer[residences.char.summer$ID==stn.change.summer.cum$ID[i],]
  # Remove single detections (includes manual record types)
  res.temp<-res.temp[res.temp$ResTime!=0,]
  # Identify which residence marks the start of the last
  # recorded station (i.e., after the station change)
  j<-which(res.temp$StartUTC==stn.change.summer.cum$StartUTC[i])
  # If there are more residences after residence j
  if (j<nrow(res.temp)){
    # Adjust the end of the cumulative residence to the final EndUTC
    stn.change.summer.cum$EndUTC[i]<-res.temp$EndUTC[nrow(res.temp)]
    # Recalculate the cumulative residence time
    stn.change.summer.cum$ResTimeCum[i]<-difftime(stn.change.summer.cum$EndUTC[i],
                                                  stn.change.summer.cum$StartUTC[i],
                                                  units="days")
  }
  # If the end of the cumulative residence is in a different
  # year than the start
  if (as.POSIXlt(stn.change.summer.cum$StartUTC[i])$year!=as.POSIXlt(stn.change.summer.cum$EndUTC[i])$year){
    # Subset whole dataset and see if no station change overwinter
    res.temp.all<-residences[residences$ID==stn.change.summer.cum$ID[i],]
    # Identify the residence at the start of the final station change
    k<-which(res.temp.all$StartUTC==stn.change.summer.cum$StartUTC[i])
    # If all residences (including winter) after the start are from
    # the same station
    if (all(res.temp.all$Station.Name[k:nrow(res.temp.all)]==stn.change.summer.cum$Station.Name[i])){
      # Then do nothing
    }
    # If not all the stations overwinter are the same
    else {
      # Start at end and go backwards to find station change
      # Set j to second last record
      j<-nrow(res.temp.all)-1
      repeat {
        if (j>=k){
          # If residences at j and j+1 are at the same station
          if (res.temp.all$Station.Name[j]==res.temp.all$Station.Name[j+1]){
            j<-j-1
          }
          # Dead drift direction or overlapping station
          else if (res.temp.all$Station.Name[j] %in% unique(ddd$FrStation)){
            # Find which dead drifting station
            m<-which(ddd$FrStation==res.temp.all$Station.Name[j])
            # If the next station is one of the matching ToStations
            if (res.temp.all$Station.Name[j+1] %in% ddd$ToStation[m]){
              # Move to the previous row
              j<-j-1
            }
            else {
              # Otherwise, there is a station change between j and j+1
              break
            }
          }
          else {break}
        }
        else {break}
      }
      # If a station change is observed overwinter (so time of
      # residence j+1 is later than the summer station change)
      if (stn.change.summer.cum$StartUTC[i]<res.temp.all$StartUTC[j+1]){
        # Adjust the cumulative summer station change
        stn.change.summer.cum$StartUTC[i]<-res.temp.all$StartUTC[j+1]
      }
    }
  }
  # If the most recent cumulative residence time is greater
  # than the maximum summer cumulative residence time that was
  # observed for a known living fish (i.e., before a station change)
  if (stn.change.summer.cum$ResTimeCum[i]>max(res.summer.cum$ResTimeCum)){
    # If the fish ID is already in res.morts
    if (stn.change.summer.cum$ID[i] %in% unique(res.morts$ID)){
      # Identify which row that ID is in res.morts
      m<-which(res.morts$ID==stn.change.summer.cum$ID[i])
      # If the residence currently in res.morts ocurred later
      # than the cumulative residence period
      if (res.morts$StartUTC[m]>stn.change.summer.cum$StartUTC[i]){
        # Adjust the start time to the earlier date
        res.morts[m,]<-stn.change.summer.cum[i,]
      }
    }
    # If the fish ID is not yet in res.morts
    else {
      res.morts[nrow(res.morts)+1,]<-stn.change.summer.cum[i,]
    }
  }
}
rm(res.temp.all)
# Fall 2022 - adds 21027
# Moves 8856, 8270, 8938, 8304, 8270, 8537 earlier
# Adds 8302, 8318, 8574, 8854, 8885, 8922, 8948
# Adds 8314, 8541, 8574, 8921
#### 8885 not early enough - need to add SND-W-R-1 to YCO-W-R-1 to ddd

# May need to add a requirement that the final year residence
# period is greater than X - otherwise, could get fish that was
# alive in fall and overwintered in place, and then just disappeared
# the next spring


#### Work backwards from identified mortalities ####
# Look back in time, to see if previous stations were also the same
# as the identified mortality station - include the winter
# Note that cannot use stn.change.summer, because now looking
# at winter residences as well
if (nrow(res.morts)>0){
  for (i in 1:nrow(res.morts)){
    # Subset residences for each identified mortality that were at or
    # prior to the identified mortality StartUTC
    morts.subset<-residences[(residences$ID==res.morts$ID[i])&
                               (residences$StartUTC<=res.morts$StartUTC[i]),]
    # # If a Station.Name ends with a W (indicating winter location),
    # # remove the W, so it does not look like a station change
    # # Find which end in W
    # k<-which(substr(morts.subset$Station.Name,nchar(morts.subset$Station.Name),nchar(morts.subset$Station.Name))=="W")
    # for (m in 1:length(k)){
    #   # Remove the W from each
    #   morts.subset$Station.Name[k[m]]<-substr(morts.subset$Station.Name[k[m]],1,nchar(morts.subset$Station.Name[k[m]])-1)
    # }
    # If more than 2 records (i.e., more than tag and mortality)
    if (nrow(morts.subset)>2){
      # Start at end and go backwards, seeing if station is the same
      j<-nrow(morts.subset)-1
      repeat {
        # If residences at j and j+1 are at the same station
        if (morts.subset$Station.Name[j]==morts.subset$Station.Name[j+1]){
          j<-j-1
        }
        # Dead drift direction or overlapping station
        else if (morts.subset$Station.Name[j] %in% unique(ddd$FrStation)){
          # Find which dead drifting station
          k<-which(ddd$FrStation==morts.subset$Station.Name[j])
          # If the next station is one of the matching ToStations
          if (morts.subset$Station.Name[j+1] %in% ddd$ToStation[k]){
            # Move to the previous row
            j<-j-1
          }
          else {
            # Otherwise, there is a station change between j and j+1
            break
          }
        }
        else {break}
      }
      # If the identified station change is earlier than the
      # previously identified mortality
      if (morts.subset$StartUTC[j+1]<res.morts$StartUTC[i]){
        # Update res.morts
        res.morts[i,]<-morts.subset[(j+1),]
      }
    }
  }
}
# Moves 8879, 8270, 8885 earlier


# #### Dead, drifting ####
# if (nrow(res.morts)>0){
#   for (i in 1:nrow(res.morts)){
#     # If a mortality station is in ddd$ToStation (the
#     # station it could have moved to, if dead and drifting)
#     if (res.morts$Station.Name[i] %in% unique(ddd$ToStation)){
#       # Find which ToSTation
#       j<-which(ddd$ToStation==res.morts$Station.Name[i])
#       # Subset residences for that ID
#       res.temp<-residences[residences$ID==res.morts$ID[i],]
#       # Find which residence is the mortality residence
#       m<-which(res.temp$StartUTC==res.morts$StartUTC[i])
#       # In case more than one station matches, loop through
#       # matching ToStations
#       for (k in 1:length(j)){
#         # If the residence before m was at ddd$FrStation
#         if (res.temp$Station.Name[m-1]==ddd$FrStation[j[k]]&
#             # And if there were > 275 days (greater than the maximum
#             # recorded ice-covered period) between the end of the
#             # previous residence and the start of the mortality residence
#             as.numeric(difftime(res.temp$StartUTC[m],res.temp$EndUTC[m-1],units="days"))>275){
#           # Change the mortality residence to the previous residence
#           res.morts[i,]<-res.temp[m-1,]
#         }
#       }
#     }
#   }
# }
# # 8948 moves u/s
# # 8856, 8921 moves u/s (not early enough)


#### Infrequent detections within previous year ####
# See if any fish with < 60 minutes (arbitrary) detection
# over the full year before last detection
# List all char IDs that do not have identified mortalities
char.nonmorts<-char[!(char %in% unique(res.morts$ID))]
for (i in 1:length(char.nonmorts)){
  # Subset residences for nonmort char i
  res.temp<-residences[residences$ID==char.nonmorts[i],]
  # Remove manual record types
  res.temp<-res.temp[!(res.temp$Receiver %in% manual.record.types[manual.record.types!="Tag"]),]
  # If there is more than 1 row (i.e., not just a tagging record)
  if (nrow(res.temp)>1){
    # If there is more than 1 year between tagging and the end of the
    # last residence
    if (difftime(res.temp$EndUTC[nrow(res.temp)],
                 res.temp$StartUTC[1],units="days")>365){
      # Find the most recent detection that is at least one year old
      m<-max(which(as.numeric(difftime(res.temp$EndUTC[nrow(res.temp)],
                                       res.temp$EndUTC,
                                       units="days"))>=365))
      # m should always exist, because there will always be a tagging
      # event
      # Sum their residence time
      # If the sum of their residence time is < 60 minutes (arbitrary),
      if (sum(res.temp$ResTime[(m+1):nrow(res.temp)])<(1/24/60*60)){
        # Start at end and go backwards to find out if all
        # records are at the same station or dead drifting
        j<-nrow(res.temp)-1
        repeat {
          # If j is greater than 1 (so never includes tagging event)
          if (j>1){
            # If residences at j and j+1 are at the same station
            if (res.temp$Station.Name[j]==res.temp$Station.Name[j+1]){
              j<-j-1
            }
            # Dead drift direction or overlapping station
            else if (res.temp$Station.Name[j] %in% unique(ddd$FrStation)){
              # Find which dead drifting station
              k<-which(ddd$FrStation==res.temp$Station.Name[j])
              # If the next station is one of the matching ToStations
              if (res.temp$Station.Name[j+1] %in% ddd$ToStation[k]){
                # Move to the previous row
                j<-j-1
              }
              else {
                # Otherwise, there is a station change between j and j+1
                break
              }
            }
            else {break}
          }
          else {break}
        }
        if (res.temp$StartUTC[j]<=res.temp$StartUTC[m]){
          res.morts[nrow(res.morts)+1,]<-res.temp[(j+1),]
        }
      }
    }
  }
}
rm(res.temp)
# Adds 8276, 8552

# Update list of char with no identified mortalities
char.nonmorts<-char[!(char %in% unique(res.morts$ID))]

write.csv(res.morts,file="Data/resmorts.csv",row.names=FALSE)

#### Adjust fish detections so they do not include the identified mortalities ####
# Make blank detection dataframe
char.live<-fish[0,]
pb<-txtProgressBar(1,length(char),style=3)
for (i in 1:length(char)){
  # If no mortality has been identified for char ID i
  if (!(char[i] %in% unique(res.morts$ID))){
    # Then include all detection records in char.live
    char.live<-rbind(char.live,fish[fish$ID==char[i],])
  }
  # If a mortality has been identified
  else {
    # Subset all detections leading up to and including the mortality
    char.live.subset<-fish[fish$ID==char[i]&
                             fish$DateTimeUTC<=res.morts$StartUTC[res.morts$ID==char[i]],]
    # Order the detections based on time
    char.live.subset<-char.live.subset[order(char.live.subset$DateTimeUTC),]
    # Identify the last detection as Mortality/Shed
    # Note this will be in the Receiver column, so it will be a
    # manual record type
    char.live.subset$Receiver[nrow(char.live.subset)]<-"Mortality/Shed"
    # Include these detections in char.live
    char.live<-rbind(char.live,char.live.subset)
  }
  setTxtProgressBar(pb,i)
}

# Update fish dataframe so detections after identified mortalities
# are not included (but other species are)
fish<-rbind(char.live,fish[fish$Species!="ARCH",])







