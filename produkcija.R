library(plumber)
library(ranger)
# attack model
model_attack <- readRDS("attack_best_model.rds")

# mid model
model_mid <- readRDS("rf_mid_model.rds")

#* @post /predict_attack
function(
    Finishing,
    Ball.Control,
    Volleys,
    Reactions,
    Dribbling,
    AgeGroup,
    Stamina,
    Shot.Power
) {
  
  new_data <- data.frame(
    Finishing = as.numeric(Finishing),
    Ball.Control = as.numeric(Ball.Control),
    Volleys = as.numeric(Volleys),
    Reactions = as.numeric(Reactions),
    Dribbling = as.numeric(Dribbling),
    AgeGroup = as.factor(AgeGroup),
    Stamina = as.numeric(Stamina),
    Shot.Power = as.numeric(Shot.Power)
  )
  
  prediction <- predict(model_attack, new_data)
  
  return(list(predicted_OVR = prediction))
}

#* @post /predict_mid
function(
    Vision,
    Short.Passing,
    Ball.Control,
    Composure,
    Reactions,
    AgeGroup,
    Long.Passing,
    Dribbling,
    Stamina
) {
  
  new_data <- data.frame(
    Vision = as.numeric(Vision),
    Short.Passing = as.numeric(Short.Passing),
    Ball.Control = as.numeric(Ball.Control),
    Composure = as.numeric(Composure),
    Reactions = as.numeric(Reactions),
    AgeGroup = as.factor(AgeGroup),
    Long.Passing = as.numeric(Long.Passing),
    Dribbling = as.numeric(Dribbling),
    Stamina = as.numeric(Stamina)
  )
  
  prediction <- predict(model_mid, new_data)$predictions
  
  return(list(predicted_OVR = prediction))
}