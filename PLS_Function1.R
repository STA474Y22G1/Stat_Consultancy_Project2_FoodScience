# Importing Data setets
Training<-read_csv("Training Data.csv") # This is for training set
Testing<-read_csv("Testing Data.csv") # This is for testing set

#====================================================================================

# Function 
PLS_Function1<-function(Training, Testing){
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
  model
  
  plot(model)
  
  ####################################################################################
  ## Prediction for testing dataset
  
  predictions = predict(model, newdata = PLStestdata[,3: ncolplstest])
  predicted_PCR <- predictions*100
  
  predictionTable <- data.frame(Series = PLStestdata$Series, `Predicted Concentration` = predicted_PCR)
  
  
  # RMSE
  RMSE <- sqrt(mean((PLStestdata$Concentration - predictions)^2))
  
  # R2
  R <- cor(PLStestdata$Concentration, predictions) ^ 2
  
  #########################################################################################
  
  ## Outputs
  
  list(`PLS_Summary`= summary, PLS_Model = model, `Predicted values for testing set` = predictionTable, 
       RMSE = RMSE, `R squared` = R)
  
  
}

#====================================================================================

# Inputs for function

PLS_Function1(Training, Testing)

