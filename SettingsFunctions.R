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

#' Create specifications for the PatientLevelPredictionValidationModule
#'
#' @param validationComponentList a list of the components neccesary for the validationDesigns
#' to be created. Each component in the list is a list with the following list items:
#' - restrictPlpDataSettings: created with `PatientLevelPrediction::createRestrictPlpDataSettings` or 
#' a list of such objects
#' - populationSettings: created with `PatientLevelPrediction::createStudyPopulationSettings` or a list
#' of such objects
#' - recalibrate: a string with the recalibration method to use, either `weakRecalibration` pr `calibrationInTheLarge`
#' - runCovariateSummary: `TRUE`/`FALSE` indicating if the `covariateSummary` should be run
#' 
#' @return
#' An object of type `PatientLevelPredictionValidationModuleSpecifications`.
#'
#' @export
createPatientLevelPredictionValidationModuleSpecifications <- function(
    validationComponentsList = list(
      list(
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), 
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),
        recalibrate = "weakRecalibration",
        runCovariateSummary = TRUE
      ), 
      list(
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(),
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),    
        recalibrate = "calibrationInTheLarge",
        runCovariateSummary = FALSE
      )
    )
) {
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.12",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = validationComponentsList
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}

