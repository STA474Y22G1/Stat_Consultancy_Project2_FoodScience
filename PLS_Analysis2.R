# Importing Data setets
Training<-read_csv("Training Data.csv") # This is for training set
Validation <- read_csv("Prediction Data.csv") # This is for validation set

#====================================================================================

# Function 
PLS_Analysis2 <-function(Training, Validation){
  ## Training Data 
  Training <- rename(Training, Concentration = `Palm olein concentration(C)`, 
                     Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelenghts
  filtertraindata2<-Training %>% filter(W>=3000 & W<=3010)
  filtertraindata3<-Training %>% filter(W>=1650 & W<=1660)
  filtertraindata4<-Training %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  filtertraindata5<-rbind(filtertraindata2, filtertraindata3, filtertraindata4) 
  
  # Selecting adulterated
  Adulttraindata <-filtertraindata5 
  
  
  # Putting PCA data in wider format
  PLStraindata<-pivot_wider(Adulttraindata, names_from = W, values_from = A)
  PLStraindata <-PLStraindata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  # Converting concentration to numeric vector in training data
  PLStraindata <- PLStraindata %>% separate(Concentration, into = c("Concentration", "percent"))
  PLStraindata$Concentration <- as.numeric(PLStraindata$Concentration)/100
  
  PLStraindata <- PLStraindata %>% subset(select = -c(Index, Series, percent, Replicate))
  
  
  ########################################################################################
  
  # Fitting model for training data set
  
  model <- train(
    Concentration ~ .,
    data = PLStraindata,
    method = 'pls'
  )
  
  
  ####################################################################################
  
  # Prediction for validation data set (Validation --> Prediction Data)
  
  ## Validation Data 
  Validation <- rename(Validation, Concentration = `Palm olein concentration(C)`, 
                       Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelenghts
  filterValidationdata2<-Validation %>% filter(W>=3000 & W<=3010)
  filterValidationdata3<-Validation %>% filter(W>=1650 & W<=1660)
  filterValidationdata4<-Validation %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  filterValidationdata5<-rbind(filterValidationdata2, filterValidationdata3, filterValidationdata4) 
  
  # Selecting adulterated
  AdultValidationdata <-filterValidationdata5 
  
  
  # Putting PCA data in wider format
  PLSValidationdata<-pivot_wider(AdultValidationdata, names_from = W, values_from = A)
  PLSValidationdata <-PLSValidationdata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  # Converting concentration to numeric vector in training data
  PLSValidationdata <- PLSValidationdata %>% separate(Concentration, into = c("Concentration", "percent"))
  PLSValidationdata$Concentration <- as.numeric(PLSValidationdata$Concentration)/100
  
  PLSValidationdata <- PLSValidationdata %>% subset(select = -c(Index, percent, Replicate))
  ncolplsvalidation<-ncol(PLSValidationdata)
  
  ########################################################################################
  
  ## Prediction for validation dataset
  
  predictions = predict(model, newdata = PLSValidationdata[,2:ncolplsvalidation])
  predicted_PCR <- predictions*100
  
  predictionTable <- data.frame(`Series Label` = PLSValidationdata$Series, `Predicted Concentration` = predicted_PCR)
  
  #########################################################################################
  
  ## Outputs
  
  list(`Predicted values for validation set` = predictionTable)
}


#====================================================================================

# Inputs for function

PLS_Analysis2(Training, Validation)

