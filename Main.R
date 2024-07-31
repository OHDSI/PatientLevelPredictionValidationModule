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

# Module methods -------------------------
getModuleInfo <- function() {
  checkmate::assert_file_exists("MetaData.json")
  return(ParallelLogger::loadSettingsFromJson("MetaData.json"))
}

getModelInfo <- function(strategusOutputPath) {
  modelDesigns <- list.files(strategusOutputPath, pattern = "modelDesign.json",
                             recursive = TRUE, full.names = TRUE)
  model <- NULL
  for (modelFilePath in modelDesigns) {
    directory <- dirname(modelFilePath)
    modelDesign <- ParallelLogger::loadSettingsFromJson(modelFilePath)
    
    if (is.null(model)) {
       model <- data.frame(
        target_id = modelDesign$targetId,
        outcome_id = modelDesign$outcomeId,
        modelPath = directory)
    } else {
      model <- rbind(model,
                     data.frame(
                       target_id = modelDesign$targetId,
                       outcome_id = modelDesign$outcomeId,
                       modelPath = directory))
    }
  }
  
  models <- model |>
    dplyr::group_by(.data$target_id, .data$outcome_id) |>
    dplyr::summarise(modelPath = list(.data$modelPath), .groups = "drop")
  return(models)
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
  if (inherits(covSettings, 'covariateSettings')) {
    covSettings <- list(covSettings)
  }
  
  for (i in 1:length(covSettings)) {
    if ('cohortTable' %in% names(covSettings[[i]])) {
      covSettings[[i]]$cohortTable <- cohortTable
    }
    if ('cohortDatabaseSchema' %in% names(covSettings[[i]])) {
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
                                                                 className = "CohortDefinitionSharedResources")
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
  
  designs <- list()
  for (i in seq_len(nrow(modelInfo))) {
    df <- modelInfo[i, ]
    
    design <- PatientLevelPrediction::createValidationDesign(
      targetId = df$target_id[1],
      outcomeId = df$outcome_id[1],
      plpModelList = as.list(df$modelPath)
      restrictPlpDataSettings = ifelse(!is.null(jobContext$settings[[1]]$restrictPlpDataSettings), jobContext$settings[[1]]$restrictPlpDataSettings, NULL),
      populationSettings = ifelse(!is.null(jobContext$settings[[1]]$populationSettings), jobContext$settings[[1]]$populationSettings, NULL)
    )
    designs[[i]] <- design 
  }
  databaseNames <- c()
  databaseNames <- c(databaseNames, paste0(jobContext$moduleExecutionSettings$connectionDetailsReference))
  
  databaseDetails <- PatientLevelPrediction::createDatabaseDetails(
    connectionDetails = jobContext$moduleExecutionSettings$connectionDetails,
    cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
    cohortDatabaseSchema = jobContext$moduleExecutionSettings$workDatabaseSchema,
    cdmDatabaseName = jobContext$moduleExecutionSettings$connectionDetailsReference,
    cdmDatabaseId = jobContext$moduleExecutionSettings$databaseId,
    tempEmulationSchema = jobContext$moduleExecutionSettings$tempEmulationSchema,
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
      resultSchema = 'main', 
      tablePrefix = '', 
      targetDialect = 'sqlite', 
      tempEmulationSchema = NULL
    ), 
    csvFolder = resultsFolder,
    fileAppend = NULL
  )
}
