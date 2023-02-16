# Importing Data setets
Training<-read_csv("Training Data.csv") # This is for training set
Testing<-read_csv("Testing Data.csv") # This is for testing set

#====================================================================================

# Function 
PLS_Analysis1<-function(Training, Testing){
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
  Adulttraindata <-filtertraindata5 #%>% filter(Series=="Adulterated")
  
  
  # Putting PCA data in wider format
  PLStraindata<-pivot_wider(Adulttraindata, names_from = W, values_from = A)
  PLStraindata <-PLStraindata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  # Converting concentration to numeric vector in training data
  PLStraindata <- PLStraindata %>% separate(Concentration, into = c("Concentration", "percent"))
  PLStraindata$Concentration <- as.numeric(PLStraindata$Concentration)/100
  
  PLStraindata <- PLStraindata %>% subset(select = -c(Index, Series, percent, Replicate))
  
  
  ########################################################################################
  
  ## Testing Data prep
  Testing <- rename(Testing, Concentration = `Palm olein concentration(C)`, 
                    Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelenghts
  filtertestdata2<- Testing %>% filter(W>=3000 & W<=3010)
  filtertestdata3<- Testing %>% filter(W>=1650 & W<=1660)
  filtertestdata4<- Testing %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  filtertestdata5<-rbind(filtertestdata2, filtertestdata3, filtertestdata4) 
  
  # Selecting adulterated
  Adulttestdata <-filtertestdata5 #%>% filter(Series=="Adulterated")
  
  
  # Putting PCA data in wider format
  PLStestdata<-pivot_wider(Adulttestdata, names_from = W, values_from = A)
  PLStestdata <-PLStestdata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  # Coverting concentration to numeric vector
  PLStestdata <- PLStestdata %>% separate(Concentration, into = c("Concentration", "percent"))
  PLStestdata$Concentration <- as.numeric(PLStestdata$Concentration)/100
  
  PLStestdata <- PLStestdata %>% subset(select = -c(Index, percent, Replicate))
  ncolplstest<-ncol(PLStestdata)
  
  ########################################################################################
  
  # Fitting model for training data set
  
  model <- train(
    Concentration ~ .,
    data = PLStraindata,
    method = 'pls'
  )
 
  # Summarize the final model
  s <- summary(model$finalModel)
  
  ####################################################################################
  ## Prediction for testing dataset
  
  predictions = predict(model, newdata = PLStestdata[,3: ncolplstest])
  predicted_PLS <- predictions*100
  
  predictionTable <- data.frame(Series = PLStestdata$Series, `Predicted Concentration` = predicted_PLS)
  
  
  # Model performance metrics
  peformance_values <- data.frame(
    RMSE = caret::RMSE(predictions, PLStestdata$Concentration),
    Rsquare = caret::R2(predictions, PLStestdata$Concentration)
  )
  
  #########################################################################################
  
  ## Outputs
  
  list(PLS_Model = model, `PLS_Summary`= s, `Predicted values for testing set` = predictionTable, 
       `Model Performance` = peformance_values)
  
  
}

#====================================================================================

# Inputs for function

PLS_Analysis1(Training, Testing)

