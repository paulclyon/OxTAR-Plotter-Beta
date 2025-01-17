# reloadStudyData.R - functions to load/reload study data from Castor
# 
# The castor_api and castor_open_api globals needs to be present and set (castorAPI.R)


# Return an array of study name strings
getStudyNames <- function()
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return()
  }
  
  studies <- castor_api$getStudies()
  studies$name
}


# Find study by name and return index of it, if not found return 0
findStudyIndex <- function(studies, studyName, stopIfNotFound=F) {
  index <- which(studies$name == studyName)
  if (length(index) == 0) {
    msg <- paste("Could not find study with name", studyName)
    if (stopIfNotFound==T) {
      stop(msg)
    }
    
    logger(msg)
    return(0)
  }

  logger(paste("Found study '", studyName,"' Index=", index, sep=''))
  return(index)
}

# FIXME - not sure this function is correct or indeed is ever called?
#getPatientData <- function(studyID) {
#  patientData <- (studyID)
#  return(patientData)
#}

# The basic (and slow!) Castor EDC method to get the data as a data.frame which comes with metadata and field names
getStudyData <- function(studyID) {
  studyData <- castor_api$getStudyData(studyID)
  return(studyData)
}

# Get the study name for a particular study ID, NA if not found
getStudyName <- function(id)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return(NA)
  }
  studies <- castor_api$getStudies()
  index <- which(studies$study_id == id)
  if (length(index) == 0) {
    return(NA)
  }
  return(studies$name[[index]])
}

# Pull in the patient data...
pullPatientData <- function (studyID) {
  studyName <- getStudyName(studyID)
  logger(paste("Loading patient data for study ID '", studyID,"' (",studyName,")",sep=""))
  patientData <- castor_api$getRecords(studyID)
  logger (paste ("Found ", nrow(patientData), " patients in study ID '",studyID,"' (",studyName,")", sep=""))
  return(patientData)
}

# Pull in the study data...
pullStudyData <- function (studyID) {
  studyName = getStudyName(studyID)
  logger(paste("Loading study data for study ID '", studyID,"' (",studyName,")...",sep=""))
  studyData <- getStudyData(studyID)
  logger(paste("Study data loaded successfully"))
  logger(paste ("Found ", nrow(studyData), " rows of data in studyID '...",studyID,"' (",studyName,")", sep=""))
  return(studyData)
}

# Pull in the study data via Open API...
pullStudyDataOpenAPI <- function (studyID) {
  studyName = getStudyName(studyID)
  logger(paste("Loading study data via Open API for study ID '", studyID,"' (",studyName,")...",sep=""))
  studyDataOpenAPI <- getStudyDataOpenAPI(studyID)
  logger(paste("Study data loaded successfully via Open API"))
  return(studyDataOpenAPI)
}

# reloadStudyData interrogates castor to load the study data.
# It sets the global values:
#
# patientData 
# studyData
reloadStudyData <- function(studyName)
{
  if (length(castor_api)==0) {
    logger("Nothing to do here until we can get into the API...")
    return()
  }
  
  if(is.na(studyName)) {
    logger("No study with the name '",studyName,"' - cannot load study...")
    return()
  }
  
  studies <- castor_api$getStudies()
  studyIndex <- findStudyIndex(studies, studyName)
  if (studyIndex==0)
  {
    logger(paste("Could not find study '", studyName, "' to load...",sep=""))
    return()
  }
  
  # Pull in the data for the OxTAR study
  oxtarStudyID     <<- studies[["study_id"]][studyIndex]
  oxtar_study_name <- studies[["name"]][studyIndex]
  patientData      <<- pullPatientData(oxtarStudyID)
  studyData        <<- pullStudyData(oxtarStudyID)
}

