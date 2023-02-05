# Importing Data set
dataset<-read_csv("Tidy Data.csv") # This is for training set

#####################################################################################################

# Function for Data analysis
Data_Analysis<-function(dataset){
  
  ## Getting data ready for analysis
  dataset <- rename(dataset, Concentration = `Palm olein concentration(C)`, 
                    Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)
  
  # Filtering Wavelenghts
  filterdata2<-dataset %>% filter(W>=3000 & W<=3010)
  filterdata3<-dataset %>% filter(W>=1650 & W<=1660)
  filterdata4<-dataset %>% filter(W>=1105 & W<=1120)
  
  # Combining filtered datasets
  filterdata5<-rbind(filterdata2, filterdata3, filterdata4) 
  
  # Selecting VCO and adulterated
  VCOdata<-filterdata5 %>% filter(Series=="Pure VCO")
  Adultdata<-filterdata5 %>% filter(Series=="Adulterated")
  
  # Combining datasets
  PCAdata<-rbind(VCOdata, Adultdata)
  
  # Putting PCA data in wider format
  PCAdata<-pivot_wider(PCAdata, names_from = W, values_from = A)
  PCAdata<-PCAdata %>% mutate(Index=1:n()) %>% relocate(Index, .before = Series)
  
######################################################################################################
  
  ## PCA Analysis
  pcadata <-  PCAdata
  pcadata_v2 <- pcadata %>% select(-c(Index:Replicate)) 
 
  # PC calculation
   pc <- prcomp(pcadata_v2,
               center = TRUE,
               scale. = TRUE)
   
   # Scree Plot
   #calculate total variance explained by each principal component
   var_explained <- pc$sdev^2 / sum(pc$sdev^2)
   
   #create scree plot
   p<-qplot(c(1:38), var_explained) + 
     geom_line() + 
     xlab("Principal Component") + 
     ylab("Variance Explained") +
     ggtitle("Scree Plot") +
     ylim(0, 1)
   
   # Summary of PCA
    Summary_PCA<-summary(pc)
    
    # extracting PCA scores
    Y1 <- pc$x[ , 1]
    Y2 <- pc$x[ , 2]
    Y3 <- pc$x[ , 3]
    
    # PC data
    PC_Scores <- cbind(pcadata[,1:4], Y1, Y2, Y3)
    
######################################################################################################
    
    ## LDA Analysis
   
     
    
    
    
    
    
    
    
    
    

#########################################################################################################    
  
    ## Outputs
    list(Scree_pot=p, Summary_PCA=Summary_PCA)
}

#######################################################################################################

Data_Analysis(dataset)



