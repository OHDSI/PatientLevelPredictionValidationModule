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


PatientLevelPredictionValidationModule <- R6::R6Class(
  classname = "PatientLevelPredictionValidationModule",
  inherit = Strategus::StrategusModule,
  public = list(
    tablePrefix = "plp",
    initialize = function() {
      super$initialize()
    },
    execute = function(connectionDetails, analysisSpecifications, executionSettings) {
      super$execute(connectionDetails, analysisSpecifications, executionSettings)
      checkmate::assertClass(executionSettings, "CdmExecutionSettings")

      private$.message("Executing PatientLevelPrediction Validation")
      jobContext <- private$jobContext
      # check the model locations are valid and apply model

      workFolder <- jobContext$moduleExecutionSettings$workSubFolder
      resultsFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
      upperResultDir <- dirname(workFolder)
      modelTransferFolder <- sort(dir(upperResultDir,
        pattern = "ModelTransferModule"
      ), decreasing = TRUE)[1]

      # hack to use output folder for model transfer
      modelSaveLocation <- file.path(upperResultDir, modelTransferFolder, "models")
      modelInfo <- private$getModelInfo(modelSaveLocation)

      designs <- list()
      for (i in seq_len(nrow(modelInfo))) {
        df <- modelInfo[i, ]

        design <- PatientLevelPrediction::createValidationDesign(
          targetId = df$target_id[1],
          outcomeId = df$outcome_id[1],
          plpModelList = as.list(df$modelPath),
          restrictPlpDataSettings = jobContext$settings[[1]]$restrictPlpDataSettings,
          populationSettings = jobContext$settings[[1]]$populationSettings
        )
        designs <- c(designs, design)
      }
      databaseNames <- c()
      databaseNames <- c(
        databaseNames,
        paste0(jobContext$moduleExecutionSettings$connectionDetailsReference)
      )

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
        logSettings = PatientLevelPrediction::createLogSettings(
          verbosity = "INFO",
          logName = "validatePLP"
        ),
        outputFolder = workFolder
      )

      sqliteConnectionDetails <- DatabaseConnector::createConnectionDetails(
        dbms = "sqlite",
        server = file.path(workFolder, "sqlite", "databaseFile.sqlite")
      )

      PatientLevelPrediction::extractDatabaseToCsv(
        connectionDetails = sqliteConnectionDetails,
        databaseSchemaSettings = PatientLevelPrediction::createDatabaseSchemaSettings(
          resultSchema = "main",
          tablePrefix = "",
          targetDialect = "sqlite",
          tempEmulationSchema = NULL
        ),
        csvFolder = resultsFolder,
        fileAppend = NULL
      )
    },
    createModuleSpecifications = function(settings) {
      specifications <- super$createModuleSpecifications(settings)
      return(specifications)
    }
  ),
  private = list(
    getModelInfo = function(modelLocations) {
      modelDesigns <- list.files(modelLocations,
        pattern = "modelDesign.json",
        recursive = TRUE, full.names = TRUE
      )
      model <- NULL
      for (modelFilePath in modelDesigns) {
        directory <- dirname(modelFilePath)
        modelDesign <- ParallelLogger::loadSettingsFromJson(modelFilePath)

        if (is.null(model)) {
          model <- data.frame(
            targetId = modelDesign$targetId,
            outcomeId = modelDesign$outcomeId,
            modelPath = directory
          )
        } else {
          model <- rbind(
            model,
            data.frame(
              targetId = modelDesign$targetId,
              outcomeId = modelDesign$outcomeId,
              modelPath = directory
            )
          )
        }
      }

      models <- model %>%
        dplyr::group_by(.data$targetId, .data$outcomeId) %>%
        dplyr::summarise(modelPath = list(.data$modelPath), .groups = "drop")
      return(models)
    }
  )
)
