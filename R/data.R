#'Sample acoustic telemetry detection data
#'
#'These detection data were subset from a real acoustic telemetry dataset of Arctic
#'Char near Kugluktuk, Nunavut. The data were from a project that was a
#'collaboration between the Kugluktuk Hunters and Trappers Organization, the
#'University of Waterloo, and Fisheries and Oceans Canada. The data are valuable
#'to the community of Kugluktuk, and are also sensitive due to their pertinence
#'to the local fishery for Arctic Char. For this reason, the detections have been
#'given generic station names and fish IDs so locations and sample information
#'is anonymous. The year has also been changed, but otherwise, the detections are
#'real, and therefore have all the challenges and intricacies of biological
#'data.
#'
#'The variables are as follows:
#'
#'\itemize{
#'\item DateTimeUTC. The date and time of each detection, in POSIXct format. The
#'data are in UTC, as downloaded from the receivers.
#'\item Station.Name Name of the receiver location.
#'\item ID Unique ID of the fish.
#'}
#'
#'@docType data
#'@keywords datasets
#'@name detections
#'@usage data(detections)
#'@format A data frame with 447 627 rows (observations) and 3 variables.
NULL

#'Sample residence events
#'
#'Events generated from the sample detection file provided. Events were generated
#'using the residences() function in mort.
#'
#'The detection data were subset from a real acoustic telemetry dataset of Arctic
#'Char near Kugluktuk, Nunavut. The data were from a project that was a
#'collaboration between the Kugluktuk Hunters and Trappers Organization, the
#'University of Waterloo, and Fisheries and Oceans Canada. The data are valuable
#'to the community of Kugluktuk, and are also sensitive due to their pertinence
#'to the local fishery for Arctic Char. For this reason, the detections have been
#'given generic station names and fish IDs so locations and sample information
#'is anonymous. The year has also been changed, but otherwise, the detections are
#'real, and therefore have all the challenges and intricacies of biological
#'data.
#'
#'The variables are as follows:
#'
#'\itemize{
#'\item ResidenceStart. The start time of the residence events, in POSIXct.
#'\item Station.Name. Name of the receiver location.
#'\item ID. Unique ID of the fish.
#'\item ResidenceEnd. The end time of the residence events, in POSIXct.
#'\item ResidenceLength.days. The duration of the residence events. The units are
#'days, as indicated by the variable name (which is automatically generated
#'by mort::residences())
#'}
#'
#'@docType data
#'@keywords datasets
#'@name events
#'@usage data(events)
#'@format A data frame with 11 487 rows (events) and 5 variables.
NULL

#'Sample dead drift direction dataframe
#'
#'The locations have been given generic station names, due to the sensitive nature
#'of the dataset. The connections between stations (i.e., which stations are
#'connected by drift and in which direction) are real.
#'
#'The variables are as follows:
#'
#'\itemize{
#'\item From. The station where a tag may drift from.
#'\item To. The station where a tag may drift to, from the station in From.
#'}
#'
#'@docType data
#'@keywords datasets
#'@name ddd
#'@usage data(ddd)
#'@format A data frame with 18 rows and 2 variables.
NULL

#'Ice-free seasons
#'
#'The year has been changed, due to the sensitive nature of the dataset. The
#'years have been changed in the same manner as the detection dates, so
#'the ice-free dates correspond to the dataset in the same way as the original
#'dataset.
#'
#'The variables are as follows:
#'
#'\itemize{
#'\item Start. The date of ice break-up (the start of the period of interest).
#'\item End. The date of ice freeze-up (the end of the period of interest).
#'}
#'
#'@docType data
#'@keywords datasets
#'@name seasons
#'@usage data(seasons)
#'@format A data frame with 5 rows (years) and 2 variables.
NULL

#'Example of new acoustic telemetry detection data
#'
#'These are fabricated detection data to demonstrate the use of the review
#'function.
#'
#'The variables are as follows:
#'
#'\itemize{
#'\item ResidenceStart. The start time of the residence events, in POSIXct.
#'\item Station.Name. Name of the receiver location.
#'\item ID. Unique ID of the fish.
#'\item ResidenceEnd. The end time of the residence events, in POSIXct.
#'\item ResidenceLength.days. The duration of the residence events. The units are
#'days, as indicated by the variable name (which is automatically generated
#'by mort::residences())
#'}
#'
#'@docType data
#'@keywords datasets
#'@name new.data
#'@usage data(new.data)
#'@format A data frame with 35 rows (observations) and 3 variables.
NULL
