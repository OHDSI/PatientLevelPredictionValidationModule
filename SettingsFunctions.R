createPatientLevelPredictionValidationModuleSpecifications <- function(
    validationComponentsList = list(
      list(
        targetId = 1,
        oucomeId = 4,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), # vector
        validationSettings = PatientLevelPrediction::createValidationSettings(
          recalibrate = "weakRecalibration"
          ),
        populationSettings = PatientLevelPrediction::createStudyPopulationSettings(
          riskWindowStart = 90, 
          riskWindowEnd = 360,
          requireTimeAtRisk = F
          )
      ), 
      list(
        targetId = 3,
        oucomeId = 4,
        restrictPlpDataSettings = PatientLevelPrediction::createRestrictPlpDataSettings(), # vector
        validationSettings = PatientLevelPrediction::createValidationSettings(
          recalibrate = "weakRecalibration"
          )
        
      )
    )
) {
  
  specifications <- list(
    module = "PatientLevelPredictionValidationModule",
    version = "0.0.7",
    remoteRepo = "github.com",
    remoteUsername = "ohdsi",
    settings = validationComponentsList
  )
  class(specifications) <- c("PatientLevelPredictionValidationModuleSpecifications", "ModuleSpecifications")
  return(specifications)
}