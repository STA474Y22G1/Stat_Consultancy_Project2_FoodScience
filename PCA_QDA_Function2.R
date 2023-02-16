# Importing Data sets
Training<-read_csv("Training Data.csv") # This is for training set
Prediction<-read_csv("Prediction Data.csv") # This is for testing set

#####################################################################################################

# Function for Data analysis
Data_Analysis<-function(Training, Prediction){
  
  ## Training Data prep
  Training <- rename(Training, Concentration = `Palm olein concentration(C)`, 
                     Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelenghts
  filtertraindata2<-Training %>% filter(W>=3000 & W<=3010)
  filtertraindata3<-Training %>% filter(W>=1650 & W<=1660)
  filtertraindata4<-Training %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  filtertraindata5<-rbind(filtertraindata2, filtertraindata3, filtertraindata4) 
  
  # Selecting VCO and adulterated
  VCOtraindata<-filtertraindata5 %>% filter(Series=="Pure VCO")
  Adulttraindata<-filtertraindata5 %>% filter(Series=="Adulterated")
  
  # Combining datasets
  PCAtraindata<-rbind(VCOtraindata, Adulttraindata)
  
  # Putting PCA data in wider format
  PCAtraindata<-pivot_wider(PCAtraindata, names_from = W, values_from = A)
  PCAtraindata<-PCAtraindata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  pcatraindata <-  PCAtraindata
  pcatraindata_v2 <- pcatraindata %>% subset(select = -c(Index, Series, Concentration, Replicate))
  
 ######################################################################################################
  
  # Validation data prep
 Prediction <- rename(Prediction, Concentration = `Palm olein concentration(C)`, 
                    Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelengths
  filterpredictiondata2<-Testing %>% filter(W>=3000 & W<=3010)
  filterpredictiondata3<-Testing %>% filter(W>=1650 & W<=1660)
  filterpredictiondata4<-Testing %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  PCApredictiondata<-rbind( filterpredictiondata2, filterpredictiondata3, filterpredictiondata4) 
 
  # Putting PCA data in wider format
  PCApredictiondata<-pivot_wider( PCApredictiondata, names_from = W, values_from = A)
  PCApredictiondata<- PCApredictiondata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
  pcapredictiondata <-  PCApredictiondata
  pcapredictiondata_v2 <-  pcapredictiondata %>% subset(select = -c(Index, Series, Concentration, Replicate))
  
  
  ######################################################################################################
  ## PCA Analysis
  
  # PC calculation
  pc <- prcomp(pcatraindata_v2,
               center = TRUE,
               scale. = TRUE)
  
  # extracting PCA scores
  Y1 <- pc$x[ , 1]
  Y2 <- pc$x[ , 2]
  Y3 <- pc$x[ , 3]
  
  # PC data
  PC_Scores <- cbind(pcatraindata[,1:4], Y1, Y2, Y3) %>% as.data.frame()
  
  
  ######################################################################################################
  
  ## DA Analysis
  
  # load the data
  DA_data <- PC_Scores
  
  # QDA
  QDA_test_data = subset(DA_data, select = -c(Index, Concentration, Replicate)) 
 
  #########################################################################################################    
  
  ## Prediction from testing set
  
  # Predicting PCs
  prediction_pcs <- predict(pc, newdata = pcapredictiondata_v2)
  prediction_pcs3 <- data.frame(Y1 = prediction_pcs[,1],
                          Y2 = prediction_pcs[,2],
                          Y3 = prediction_pcs[,3])
  
  
  # Prediction 
  pred <- predict(qda_results, prediction_pcs3)$class
  
  
  # Prediction Data Frame
  qda_prediction_df <- data.frame("Predicted_Group" = pred)
  
  
  #########################################################################################################    
  
  ## Outputs
  list( `QDA Prediction`= qda_prediction_df)
}

#######################################################################################################

Data_Analysis(Training, Prediction)



