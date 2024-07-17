## functions that primarily work on studyData

# Return true if the study is a valid OxTAR study with at least one patient, false otherwise
ifValidOxTARStudy <- function(stopIfNotValid = F) {
  if ("pt_rx_count" %in% colnames(studyData)) {
    if (nrow(patientData) > 0 ) {
      return(TRUE)
    }
  }
  # FIXME: Does this need to stop if not valid?
  return(FALSE)
}

# Return the indexed data otherwise return NA
# Note that NA may be returned if the field does not exist in the data
# or if stopIfNotFound = T the code will terminate with an error
# The issue with having stopIfNotFound = T as default is that even if the field exists but has no data,
# it is not returned in the studyData (despite being seen in the Excel export data from the web client)
getDataEntry <- function(field, index, stopIfNotFound = F) {
  if (field %in% colnames(studyData)) {
    return(studyData[[field]][index])
  } 
  
  if (stopIfNotFound == T) {
    msg = paste("Field '", field, "' not found in the data - stopping", sep = '')
    logger(msg)
    stop(msg)
  }
  else
  {
    logger(paste("Field '", field, "' not found in the data, but this may be because data not populated rather than missing field", sep = ''))
  }
  return(NA)
}