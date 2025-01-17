## Functions to load the castor API

# Connect to the Castor API...
connectCastorAPI <- function()
{
  #logger("   Attempting to open Castor API...") 
  castor_api <<- tryCatch(
    { CastorData$new(key =      Sys.getenv("CASTOR_USER_KEY"),
                     secret =   Sys.getenv("CASTOR_SECRET"),
                     base_url = Sys.getenv("CASTOR_URL"))
    },
    
    error=function(e) {
      logger(paste("   An error occurred: Could not access Castor API..."))
      logger(e)
      castor_api <<- NA # Its an environment so we can't use is.na() function, but we can use is.environment()
    },
    
    #if a warning occurs, tell me the warning
    warning=function(w) {
      logger(paste("   A warning occurred during access to Castor API"))
      logger(w)
    }
  )
  
  if (!is.environment(castor_api))
  {
    logger("   Failed to access API with those settings")
  }
  else
  {
    logger("   Accessed Castor API successfully")
  }
  return(castor_api)
}

# Disconnect from the Castor API...
disconnectCastorAPI <- function()
{
  logger("Disconnecting from Castor API...")
  castor_api <<- new.env()
  castor_open_api <<- new.env()
}

# We can use the openAPI to write data, in fact its the only way I can work out how to do it!
# Largely borrowed from:
# https://git.lumc.nl/egjvonasmuth/castor-api-tutorial/-/blob/main/example_rapiclient.R?ref_type=heads
connectCastorOpenAPI <- function()
{
  base_url <- Sys.getenv("CASTOR_URL")
  tryCatch(
    { 
      # Authenticate using oauth
      app <- oauth_app("CastorEDC", key = Sys.getenv("CASTOR_USER_KEY"), secret = Sys.getenv("CASTOR_SECRET"))
      endpoint <- oauth_endpoint(request = NULL, base_url = paste0(base_url, "/oauth"), access = "token", "authorize")
      token <- httr::oauth2.0_token(endpoint, app, client_credentials = TRUE, cache = FALSE)
      castor_open_api <<- get_api(paste0(base_url, "/api/swagger"), httr::config(token = token))
    },
    
    error=function(e) {
      logger(paste("An error occurred: Could not access Castor Open API..."))
      logger(e)
      castor_open_api <<- NA # Its an special rapiclient environment which is essentially a list, so we can't use is.na() function or is.environment()
    },
    
    #if a warning occurs, tell me the warning
    warning=function(w) {
      logger(paste("A warning occurred during access to Castor Open API"))
      logger(w)
    }
  )
  
  # If its not a list it hasn't worked...
  if (typeof(castor_open_api) != "list") 
  {
    logger("Failed to access Castor Open API with those settings")
  }
  else
  {
    # As a global, retrieve the possible operations which is needed to then do the magic later on!
    openAPIoperations <<- get_operations(castor_open_api)
    
    # ...And the possible schemas; we use schemas again below
    openAPIschemas <<- get_schemas(castor_open_api)
    
    # Castor API uses labels like descriptions so this results in awful labels
    # We can use the path instead of the label
    # Need the <<- so it makes permanent change globally otherwise change is temporary
    names(openAPIoperations) <<- sapply(openAPIoperations, function(x) 
      paste(attributes(x)$definition$action, attributes(x)$definition$path))
    
    # A lot easier - do we need this too not sure?
    #openAPIoperations$`get /user`() |> content(as = "text") |> fromJSON()
    
    logger("Accessed Castor Open API successfully")
  }
  return(castor_open_api)
}

# Write actions using OpenAPI interface - this is the only way I can work out how to write using any API...
# The Open API method to get the study data, which is in a different format...
# Its the bare bones of : https://git.lumc.nl/egjvonasmuth/castor-api-tutorial/-/blob/main/example_rapiclient.R?ref_type=heads
# Interestingly it is very fast to load this way and it may be we switch use this method for all moving forward!
# To see how to use it just type 'operations$' into the Console of R and it will show you all available methods
updateStudyDataOpenAPI <- function(studyID, patientID, fieldID, newData, reasonForUpdate="Unspecified Reason")
{
  # Connect to the API here
  if (!is.environment(castorOpenAPI))
  {
    castorOpenAPI <<- connectCastorOpenAPI()
  }

  # If have haven't previously pulled in the Open API study data, pull it in now
  if (typeof(studyDataOpenAPI) != "list")
  {
    studyDataOpenAPI <<- openAPIoperations$`get /study/{study_id}/data-points/study`(studyID) |>
      content(as = "text") |>
      fromJSON()
  }
  
  # Do the magic to update
  openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`
  formals(openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`)
  write_result_openapi <- openAPIoperations$`post /study/{study_id}/participant/{participant_id}/data-points/study`(
    study_id = studyID,
    participant_id = patientID,
    common = openAPIschemas$DataPointCollectionCommonParameters(
      change_reason = reasonForUpdate,
      confirmed_changes = "true"
    ),
    data = list(
      openAPIschemas$StudyDataPointCollectionFieldValueParameter(
        field_id = fieldID,
        field_value = newData
      )
    )
  )
  write_result_openapi
  content(write_result_openapi, as = "text") |> fromJSON()
  logger("<< OpenAPI write completed without error")
}

# Get field ID for field name using the meta-data which is secretly hidden in with R in the Castor data.frame!
# Don't know why Castor API couldn't have a few more helping functions like this - surely everyone using API needs this?
getFieldIDForName <- function(data,fieldName)
{
  meta <- attr(data,"field_metadata")
  i <- which (meta$field_variable_name == fieldName)
  if (!is.na(i) && i==0)
  {
    msg <- paste("Field name '", fieldName, "' not found in the metadata for the study data!", sep = '')
    logger(msg)
    stop(msg)
  }
  return (meta$field_id[i])
}



