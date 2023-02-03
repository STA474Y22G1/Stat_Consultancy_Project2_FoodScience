oildata<-read_csv("Tidy Data.csv")

oildata <- rename(oildata, Concentration = `Palm olein concentration(C)`, 
                  Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)

# Filtering Wavelenghts
filterdata2<-oildata %>% filter(W>=3000 & W<=3010)
filterdata3<-oildata %>% filter(W>=1650 & W<=1660)
filterdata4<-oildata %>% filter(W>=1105 & W<=1120)

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
View(PCAdata)

# Exporting dataset
write_csv(PCAdata,"PCAData.csv")


