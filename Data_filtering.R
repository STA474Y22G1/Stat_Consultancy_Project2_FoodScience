oildata<-read_csv("Tidy Data.csv")


oildata <- rename(oildata, Concentration = `Palm olein concentration(C)`, 
                  Replicate = `Replicate No`, W=`Wave Number (cm-1)(W)`, A=`Absorption (A)`)

# Selecting VCO and adulterated
filterdata1<-oildata %>% filter(Series==c("Pure VCO", "Adulterated" ))

# Filtering Wavelenghts
filterdata2<-filterdata1 %>% filter(W>=3000 & W<=3010)
filterdata3<-filterdata1 %>% filter(W>=1650 & W<=1660)
filterdata4<-filterdata1 %>% filter(W>=1105 & W<=1120)


#PCA
PCAdata<-rbind(filterdata2, filterdata3, filterdata4) 
PCAdata[is.na(PCAdata)] <- 0

PCAdata<-pivot_wider(PCAdata, names_from = W, values_from = A)

write_csv(PCAdata,"PCAData.csv")
View(PCAdata)

