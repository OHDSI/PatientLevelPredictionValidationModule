# Copyright 2023 Observational Health Data Sciences and Informatics
#
# This file is part of "PatientLevelPredictionValidationModule
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.


# function to enable simple GLM to be validated using PLP
predictGLM <- function(plpModel, data, cohort){
  require('dplyr')
  start <- Sys.time()
  
  ParallelLogger::logTrace('predictProbabilities using predictGLM')
  
  data$covariateData$coefficients <- plpModel$model$coefficients
  on.exit(data$covariateData$coefficients <- NULL)
  
  prediction <- data$covariateData$covariates %>%
    dplyr::inner_join(data$covariateData$coefficients, by= 'covariateId') %>%
    dplyr::mutate(values = .data$covariateValue*.data$coefficient) %>%
    dplyr::group_by(.data$rowId) %>%
    dplyr::summarise(value = sum(.data$values, na.rm = TRUE)) %>%
    dplyr::select("rowId", "value")
  
  prediction <- as.data.frame(prediction)
  prediction <- merge(cohort, prediction, by ="rowId", all.x = TRUE, fill = 0)
  prediction$value[is.na(prediction$value)] <- 0
  prediction$value <- prediction$value + plpModel$model$intercept
  
  # linear/logistic/square/exponential
  if(plpModel$model$finalMapping == 'linear'){
    prediction$value <- prediction$value
  } else if(plpModel$model$finalMapping == 'logistic'){
    prediction$value <- 1/(1+exp(-prediction$value))
  } else if(plpModel$model$finalMapping == 'square'){
    prediction$value <- prediction$value^2
  } else if(plpModel$model$finalMapping == 'exponential'){
    prediction$value <- exp(prediction$value)
  }
  
  attr(prediction, "metaData")$modelType <- "binary"
  
  delta <- Sys.time() - start
  ParallelLogger::logInfo("Prediction took ", signif(delta, 3), " ", attr(delta, "units"))
  return(prediction)
}

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

getModelInfo <- function(strategusOutputPath) {
  
  combinedData <- NULL
  subdirs <- list.files(strategusOutputPath, full.names = TRUE)
  
  for (dir in subdirs) {
    folder <- basename(dir)
    print(basename(folder))
    
    allFiles <- list.files(dir, pattern = "models.csv", full.names = TRUE, recursive = TRUE)
    
    for(modelFilePath in allFiles) {
      directoryPath <- dirname(modelFilePath)
      databaseDetailsPath <- file.path(directoryPath, "database_details.csv")
      databaseMetaDataPath <- file.path(directoryPath, "database_meta_data.csv")
      modelDesign <- file.path(directoryPath, "model_designs.csv")
      cohorts <- file.path(directoryPath, "cohorts.csv")
      
      modelData <- read.csv(modelFilePath)
      databaseDetails <- read.csv(databaseDetailsPath)
      databaseMetaData <- read.csv(databaseMetaDataPath)
      modelDesign <- read.csv(modelDesign)
      cohorts <- read.csv(cohorts)
      
      modelData$plp_model_file <- file.path(directoryPath, "models", basename(modelData$plp_model_file))
      
      enrichedData <- merge(modelData, databaseDetails, by = "database_id")
      finalModelData <- merge(enrichedData, databaseMetaData, by.y = "database_id", by.x = "database_meta_data_id")
      finalModelData <- merge(finalModelData, modelDesign, by = "model_design_id")
      finalModelData <- merge(finalModelData, cohorts, by.x = "outcome_id", by.y = "cohort_id")
      finalModelData <- within(finalModelData, {
        outcome_id <- cohort_definition_id
      })
      finalModelData$cohort_definition_id <- NULL
      
      finalModelData <- merge(finalModelData, cohorts, by.x = "target_id", by.y = "cohort_id")
      finalModelData <- within(finalModelData, {
        target_id <- cohort_definition_id
      })
      
      if(is.null(combinedData)) {
        combinedData <- finalModelData
      } else {
        combinedData <- rbind(combinedData, finalModelData)
      }
    }
  }
  finalSelectedData <- combinedData %>%
    select(cdm_source_abbreviation, analysis_id, model_design_id, model_type, target_id, outcome_id, plp_model_file)
}

getSharedResourceByClassName <- function(sharedResources, className) {
  returnVal <- NULL
  for (i in 1:length(sharedResources)) {
    if (className %in% class(sharedResources[[i]])) {
      returnVal <- sharedResources[[i]]
      break
    }
  }
  invisible(returnVal)
}

# this updates the cohort table details in covariates
updateCovariates <- function(plpModel, cohortTable, cohortDatabaseSchema){
  
  covSettings <- plpModel$modelDesign$covariateSettings
  # if a single setting make it into a list to force consistency
  if(inherits(covSettings, 'covariateSettings')){
    covSettings <- list(covSettings)
  }
  
  for(i in 1:length(covSettings)){
    if('cohortTable' %in% names(covSettings[[i]])){
      covSettings[[i]]$cohortTable <- cohortTable
    }
    if('cohortDatabaseSchema' %in% names(covSettings[[i]])){
      covSettings[[i]]$cohortDatabaseSchema <- cohortDatabaseSchema
    }
  }
  
  plpModel$modelDesign$covariateSettings <- covSettings
  
  return(plpModel)
}

createCohortDefinitionSetFromJobContext <- function(sharedResources, settings) {
  cohortDefinitions <- list()
  if (length(sharedResources) <= 0) {
    stop("No shared resources found")
  }
  cohortDefinitionSharedResource <- getSharedResourceByClassName(sharedResources = sharedResources, 
                                                                 class = "CohortDefinitionSharedResources")
  if (is.null(cohortDefinitionSharedResource)) {
    stop("Cohort definition shared resource not found!")
  }
  cohortDefinitions <- cohortDefinitionSharedResource$cohortDefinitions
  if (length(cohortDefinitions) <= 0) {
    stop("No cohort definitions found")
  }
  cohortDefinitionSet <- CohortGenerator::createEmptyCohortDefinitionSet()
  for (i in 1:length(cohortDefinitions)) {
    cohortJson <- cohortDefinitions[[i]]$cohortDefinition
    cohortDefinitionSet <- rbind(cohortDefinitionSet, data.frame(
      cohortId = as.integer(cohortDefinitions[[i]]$cohortId),
      cohortName = cohortDefinitions[[i]]$cohortName,
      json = cohortJson,
      stringsAsFactors = FALSE
    ))
  }
  return(cohortDefinitionSet)
}

# Module methods -------------------------
execute <- function(jobContext) {
  library(PatientLevelPrediction)
  rlang::inform("Validating inputs")
  inherits(jobContext, 'list')
  
  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }
  
  workFolder <- jobContext$moduleExecutionSettings$workSubFolder
  resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  
  rlang::inform("Executing PLP Validation")
  moduleInfo <- getModuleInfo()
  
  # find where cohortDefinitions are as sharedResources is a list
  cohortDefinitionSet <- createCohortDefinitionSetFromJobContext(
    sharedResources = jobContext$sharedResources,
    settings = jobContext$settings
  )
  
  # check the model locations are valid and apply model
  upperWorkDir <- dirname(workFolder)
  modelTransferFolder <- sort(dir(upperWorkDir, pattern = 'ModelTransferModule'), decreasing = T)[1]
  
  modelSaveLocation <- file.path( upperWorkDir, modelTransferFolder, 'models') # hack to use work folder for model transfer 
  modelInfo <- getModelInfo(modelSaveLocation)
  
  
  groupedModelInfo <- modelInfo %>%
    filter(!(model_type %in% c("ResNet", "Transformer"))) %>%
    group_by(target_id, outcome_id)
  
  splitModelInfo <- split(groupedModelInfo, list(groupedModelInfo$target_id, groupedModelInfo$outcome_id), drop = TRUE)
  
  designs <- list()
  for (i in seq_along(splitModelInfo)) {
    df <- splitModelInfo[[i]]
    
    design <- PatientLevelPrediction::createValidationDesign(
      targetId = df$target_id[1],
      outcomeId = df$outcome_id[1],
      populationSettings = PatientLevelPrediction:::createStudyPopulationSettings(),
      restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
      plpModelList = as.list(df$plp_model_file)
    )
    designs[[i]] <- design  # Adding elements to a list
  }
  
  databaseDetails <- list()
  databaseNames <- c()
  databaseNames <- c(databaseNames, paste0(jobContext$moduleExecutionSettings$connectionDetailsReference))
  
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cdmDatabaseName = paste0(jobContext$moduleExecutionSettings$connectionDetailsReference),
    cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
    #tempEmulationSchema =  , is there s temp schema specified anywhere?
    cohortTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable,
    outcomeDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    outcomeTable = jobContext$moduleExecutionSettings$cohortTableNames$cohortTable
  )
  
  PatientLevelPrediction::validateExternal(
    validationDesignList = designs,
    databaseDetails = databaseDetails,
    logSettings = PatientLevelPrediction::createLogSettings(verbosity = 'INFO', logName = 'validatePLP'),
    outputFolder = workFolder
  )
  
  sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
    dbms = 'sqlite',
    server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
  )
  
  PatientLevelPrediction::extractDatabaseToCsv(
    connectionDetails = sqliteConnectionDetails, 
    databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
      resultSchema = 'main', # sqlite settings
      tablePrefix = '', # sqlite settings
      targetDialect = 'sqlite', 
      tempEmulationSchema = NULL
    ), 
    csvFolder = resultsFolder,
    fileAppend = NULL
  )
}
