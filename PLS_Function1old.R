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
  Adulttraindata <-filtertraindata5 %>% filter(Series=="Adulterated")
  
  
  # Putting PCA data in wider format
 PLStraindata<-pivot_wider(Adulttraindata, names_from = W, values_from = A)
 PLStraindata <-PLStraindata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
 
 # Converting concentration to numeric vector in training data
 PLStraindata <- PLStraindata %>% separate(Concentration, into = c("Concentration", "percent"))
 PLStraindata$Concentration <- as.numeric(PLStraindata$Concentration)/100
 
 PLStraindata <- PLStraindata %>% filter(Series == "Adulterated")  
 
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
 Adulttestdata <-filtertestdata5 %>% filter(Series=="Adulterated")
 
 
 # Putting PCA data in wider format
 PLStestdata<-pivot_wider(Adulttestdata, names_from = W, values_from = A)
 PLStestdata <-PLStestdata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
 
 # Coverting concentration to numeric vector
 PLStestdata <- PLStestdata %>% separate(Concentration, into = c("Concentration", "percent"))
 PLStestdata$Concentration <- as.numeric(PLStestdata$Concentration)/100
 
 PLStestdata <- PLStestdata %>% filter(Series == "Adulterated")  
 
 PLStestdata <- PLStestdata %>% subset(select = -c(Index, Series, percent, Replicate))
 
 ########################################################################################
 
 # Fitting model for training data set
 
 # Split the column names in X and Y
 X_colnames <- colnames(PLStraindata)[2:39]
 Y_colnames <- colnames(PLStraindata)[1]
 
 # Split train data into matrices
 X_train_matrix <- as.matrix(PLStraindata[X_colnames])
 Y_train_matrix <- as.matrix(PLStraindata[Y_colnames])
 
 # PLS Regression
 pls <- plsr(Y_train_matrix ~ X_train_matrix, scale=TRUE, validation="CV")
 
 summary <- summary(pls)
 
 # Create a plot to define the number of components
 plot(RMSEP(pls))
 
 ####################################################################################
 ## Prediction for testing dataset
 
 # prediction
 pcr_pred <- predict(pls, PLStestdata[,2:39], ncomp=8)
 predicted_PCR <- pcr_pred*100
 
 #calculate RMSE
 RMSE <- sqrt(mean((pcr_pred - PLStestdata$Concentration)^2))
 
 #########################################################################################
 
 ## Outputs
 
 list(`PLS Summary`= summary, `Predicted values for testing set` = predicted_PCR, 
      RMSE = RMSE)
 
 
}

#====================================================================================

# Inputs for function

PLS_Function1(Training, Testing)
