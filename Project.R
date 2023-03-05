#1-Scarica e carica il dataset
real_estate_texas<-read.csv("realestate_texas.csv")


#2 --------------------------------------
 #-Tipo di variabile?
 #non essendo specificato, di seguito ho riporto sia la variabile in R che 
 #in Statistica(tipo di scala, discreta/continua etc.)

attach(real_estate_texas)

class(city)               #variabile nominale qualitativa  
class(year)               #mutabile ordinabile 
class(month)              #mutabile ordinabile ciclica
class(sales)              #variabile quantitativa discreta scala rapporti
class(volume)             #variabile quantitativa continua scala rapporti
class(median_price)       #variabile quantitativa continua scala rapporti
class(listings)           #variabile quantitativa discreta scala rapporti
class(months_inventory)   #variabile quantitativa continua scala rapporti


#3 --------------------------------------
 
 #medie analitiche e posizionali
summary(sales)
summary(volume)
summary(median_price)
summary(listings)
summary(months_inventory)
          

 #variabilità

variability <-function(object) {       #funzione identica a summary() ma per 
                                       #indici di variabilità
  IQR=IQR(object)
  range=max(object)-min(object)
  var=var(object)
  sd=sd(object)
  x=data.frame(IQR,range,var,sd)

  return(x)
}

variability(sales)                     
variability(volume)                    
variability(median_price)              
variability(listings)                  
variability(months_inventory)

 #variabilità_indici_di_forma

library(moments)

forma <- function(object) {
  library(moments)
  Adj_Kurt=kurtosis(object)-3
  Skew=skewness(object)
  x=data.frame(Adj_Kurt,Skew)
  
  return(x)
}

forma(sales)                       
forma(volume)                    
forma(median_price)              
forma(listings)                  
forma(months_inventory)

 #distribuzione di frequenze(city, year, month)

table(city, month, year)


#4 ---------------------------------

asimmetria<-c(skewness(sales),skewness(volume),skewness(median_price),skewness(listings),skewness(months_inventory))
which.max(asimmetria)


#5 --------------------------------- 

dati_cut<-cut(sales, breaks = 8, labels = c("very_low(78.7,122]","low(122,165]","fair(165,208]","medium(208,251]","good(251,294]","very_good(294,337]","high(337,380]","very_high(380,423]") )
ni<-table(dati_cut)
N<-dim(real_estate_texas)[1]
fi<-ni/N
Fi<-cumsum(fi)
Ni<-cumsum(ni)

tabella_frequenze_sales<-cbind(ni,fi,Ni,Fi)  #tabella 

barplot(ni,                                  #grafico a barre
        main = "Sales class distribution",
        xlab = "Sales Classes",
        ylab = "Absolute Frequencies",
        col = "blue",
        las = 1)

gini <- function(x){                    #creazione funzione per calcolare gini 
  ni = table(x)
  fi = ni/length(x)
  fi2 = fi^2
  J = length(table(x))
  
  gini = 1-sum(fi2)
  gini.norm = gini/((J-1)/J)
  
  return(gini.norm)
}

gini(dati_cut)                              #Indice di Gini


#6 ---------------------------------

table(city)
gini(city)


#7 ---------------------------------

freq_Beaumont<-length(real_estate_texas$city[city=="Beaumont"])  #probabilità estrazione Beaumont
probabilità_Bearumont<-freq_Beaumont/dim(real_estate_texas)[1]
signif(probabilità_Bearumont,3)

freq_Luglio<-length(real_estate_texas$month[month=="7"])     #probabilità estrazione Luglio
probabilità_Luglio<-freq_Luglio/dim(real_estate_texas)[1]
signif(probabilità_Luglio,3)

subset_1<-subset(real_estate_texas,        #probabilità estrazione Dicembre 2012
                 subset= year == 2012,
                 select = month)
probabilità_Dicembre_2012<-length(subset_1[subset_1==12])/dim(real_estate_texas)[1]
signif(probabilità_Dicembre_2012)

#oppure

xtabs(~year+month, real_estate_texas)   #frequenza individuabile visivamente
                                        #dividere per N

#oppure 

x_month<-real_estate_texas$month==12
probabilità_Dicembre_2012_1<-length(which(real_estate_texas[x_month,"year"]==2012))/dim(real_estate_texas)[1]
signif(probabilità_Dicembre_2012_1)


#8 ---------------------------------

average_price<-signif(volume/sales*1000000,4)           
real_estate_texas$average_price<-average_price 

library(dplyr)                                                                            #sposto "average_price" vicino a "median_price" 
real_estate_texas<- real_estate_texas %>% relocate(average_price, .before = listings)     #per aumentare la leggibilità del dato
detach(dplyr)

#9 --------------------------------- 

daily_inventory<-months_inventory*30
efficacy_advertising_time<-signif(listings/daily_inventory,3)
real_estate_texas$efficacy_advertising_time<-efficacy_advertising_time #espressa in giorni

efficacy_advertising_sales<-signif(sales/listings,3)*100
real_estate_texas$efficacy_advertising_sales<-paste(efficacy_advertising_sales,"%") 

#Considerazioni

Beaumont_efficacy<-real_estate_texas[city=="Beaumont","efficacy_advertising_sales"]
Beaumont_efficacy<-as.numeric(gsub('%','', Beaumont_efficacy))
summary(Beaumont_efficacy)

Bryan_College_Station_efficacy<-real_estate_texas[city=="Bryan-College Station","efficacy_advertising_sales"]
Bryan_College_Station_efficacy<-as.numeric(gsub('%','', Bryan_College_Station_efficacy))
summary(Bryan_College_Station_efficacy)

Tyler_efficacy<-real_estate_texas[city=="Tyler","efficacy_advertising_sales"]
Tyler_efficacy<-as.numeric(gsub('%','', Tyler_efficacy))
summary(Tyler_efficacy)

Wichita_Falls_efficacy<-real_estate_texas[city=="Wichita Falls","efficacy_advertising_sales"]
Wichita_Falls_efficacy<-as.numeric(gsub('%','', Wichita_Falls_efficacy))
summary(Wichita_Falls_efficacy)

efficacy_advertising_means<-c(mean(Tyler_efficacy),mean(Wichita_Falls_efficacy),
                            mean(Bryan_College_Station_efficacy),mean(Beaumont_efficacy))

barplot(efficacy_advertising_means,
        ylim = c(0,100),
        ylab = "Efficacy Advertising Percentage",
        main = "Efficacy Advertising per City",
        names.arg = c("Tyler","Wichita_Falls","Bryan_College_Station","Beaumont"),
        col = "blue",
        las= 1,
        axes = F)
axis(2,at=seq(0,100,by=5),labels=T)


#10 --------------------------------- 

# ------
  #creazione summary 

library(dplyr)

city_sales<-real_estate_texas %>%
            group_by(city) %>%
            summarise(av_sales=round(mean(sales)),
                      median_sales=median(sales),
                      sd_sales=sd(sales))

year_city_sales<-real_estate_texas %>%
                 group_by(year,city) %>%
                 summarise(av_sales=round(mean(sales),2),
                           median_sales=median(sales),
                           sd_sales=sd(sales))

month_city_sales<-real_estate_texas %>%
                  group_by(month,city) %>%
                  summarise(av_sales=round(mean(sales),2),
                            median_sales=median(sales),
                            sd_sales=sd(sales))

# ------
  #serie storica(line-chart) year_city_sales

sales_year_Beaumont<-filter(year_city_sales,city=="Beaumont")
sales_year_Tyler<-filter(year_city_sales,city=="Tyler")
sales_year_Wichita_Falls<-filter(year_city_sales,city=="Wichita Falls")
sales_year_Bryan_College_Station<-filter(year_city_sales,city=="Bryan-College Station")

library(ggplot2)
sales_year_city<-ggplot(sales_year_Beaumont)+
    
    geom_point(aes(x=year,y=av_sales,col="Beaumont"),lwd=4)+  
    geom_point(data=sales_year_Tyler,aes(x=year,y=av_sales,col="Tyler"),lwd=4)+
    geom_point(data=sales_year_Wichita_Falls,aes(x=year,y=av_sales,col="Wichita Falls"),lwd=4)+
    geom_point(data=sales_year_Bryan_College_Station,aes(x=year,y=av_sales,col="Bryan-College Station"),lwd=4)+
    
    geom_line(aes(x=year,y=av_sales,col="Beaumont"),lwd=1)+
    geom_line(data=sales_year_Tyler,aes(x=year,y=av_sales,col="Tyler"),lwd=1)+
    geom_line(data=sales_year_Wichita_Falls,aes(x=year,y=av_sales,col="Wichita Falls"),lwd=1)+
    geom_line(data=sales_year_Bryan_College_Station,aes(x=year,y=av_sales,col="Bryan-College Station"),lwd=1)+
    
    geom_text(data=sales_year_Tyler,aes(x=year,y=av_sales+10,label=av_sales),size=3.5)+
    geom_text(data=sales_year_Wichita_Falls,aes(x=year,y=av_sales+5,label=av_sales), size=3.5)+
    geom_text(data=sales_year_Bryan_College_Station,aes(x=year,y=av_sales+8,label=av_sales),size=3.5)+
    geom_text(aes(x=year,y=av_sales-8,label=av_sales),size=3.5)+
    
    labs(x="Years",
         y="Average Sales",
         title="Sales Trend by City")+
    scale_y_continuous(breaks = seq(0,350,25))+
    scale_color_manual(
      name   = 'City',
      breaks = c('Beaumont', 'Tyler',"Wichita Falls","Bryan-College Station"),
      values = c("red", "green3","cyan","orange"),
      labels = c('Beaumont', 'Tyler',"Wichita Falls","Bryan-College Station"))


# ------
 # #serie storica(line-chart) month_city_sales

sales_month_B<-filter(month_city_sales,city=="Beaumont")
sales_month_T<-filter(month_city_sales,city=="Tyler")
sales_year_WF<-filter(month_city_sales,city=="Wichita Falls")
sales_year_BCS<-filter(month_city_sales,city=="Bryan-College Station")

library(ggplot2)

sales_months_city<-ggplot(sales_month_B)+
  
  geom_point(aes(x=month,y=av_sales,col="Beaumont"),lwd=4)+  
  geom_point(data=sales_month_T,aes(x=month,y=av_sales,col="Tyler"),lwd=4)+
  geom_point(data=sales_year_WF,aes(x=month,y=av_sales,col="Wichita Falls"),lwd=4)+
  geom_point(data=sales_year_BCS,aes(x=month,y=av_sales,col="Bryan-College Station"),lwd=4)+
  
  geom_line(aes(x=month,y=av_sales,col="Beaumont"),lwd=1)+
  geom_line(data=sales_month_T,aes(x=month,y=av_sales,col="Tyler"),lwd=1)+
  geom_line(data=sales_year_WF,aes(x=month,y=av_sales,col="Wichita Falls"),lwd=1)+
  geom_line(data=sales_year_BCS,aes(x=month,y=av_sales,col="Bryan-College Station"),lwd=1)+
  
  geom_text(data=sales_month_T,aes(x=month,y=av_sales+10,label=av_sales),size=3.5)+
  geom_text(data=sales_year_WF,aes(x=month,y=av_sales+5,label=av_sales), size=3.5)+
  geom_text(data=sales_year_BCS,aes(x=month,y=av_sales-5,label=av_sales),size=3.5)+
  geom_text(aes(x=month,y=av_sales+5,label=av_sales),size=3.5)+
  
  labs(x="Months",
       y="Average Sales",
       title="Sales Trend by City")+
  scale_y_continuous(breaks = seq(0,350,25))+
  scale_x_continuous(breaks = seq(0,12,1))+
  scale_color_manual(
    name   = 'City',
    breaks = c('Beaumont', 'Tyler',"Wichita Falls","Bryan-College Station"),
    values = c("red", "green3","cyan","orange"),
    labels = c('Beaumont', 'Tyler',"Wichita Falls","Bryan-College Station"))
 
 # affianco i grafici per migliorarne l'interpretazione
 # avrei voluto sopprimere le leggende in quanto ridondanti
 # ma non sono riuscito a farlo
 

complete_line_chart<-gridExtra::grid.arrange(sales_year_city, sales_months_city, ncol=2)


#11 ---------------------------------

library(ggplot2)
ggplot(data = real_estate_texas)+
  geom_boxplot(aes(
    x=city,
    y=median_price),
    fill="lightblue")+
  stat_summary(aes(
    x=city,
    y=median_price),
   fun = "mean", geom = "point", shape = 8,
               size = 2, color = "red")

median_price_Beaumont<-real_estate_texas[city=="Beaumont","median_price"]
which(median_price_Beaumont>(quantile(median_price_Beaumont,0.75)+IQR(median_price_Beaumont*1.5)))
median_price_Beaumont[1]

#12 ---------------------------------

library(ggplot2)

ggplot(data = real_estate_texas)+    
  geom_boxplot(aes(                  
    x=as.factor(year),
    y=volume,
    fill=city))+
  labs(
    y="Total Value Sales",
    title = "Sales value by Years/City"
  )+
  labs(x="Years")

#13 ---------------------------------
  
library(ggplot2)

ggplot(real_estate_texas)+
  geom_col(aes(
    x=month,
    y=sales,
    fill=city))+
  scale_x_continuous(breaks= seq(1,12,1))+
  scale_y_continuous(breaks= seq(0,1200,100))+
  labs(x="Months",
       y="Total Sales",
       title="Real Estate Sales Trend over Time")+
  facet_wrap(~year)

# ---- 

library(dplyr)
x<-real_estate_texas %>%
  group_by(year,month,city) %>%
  summarise(sales=sales)

n=4
y=list()
while(n<=240){
 y = append(y,x$sales[(n-3):(0+n)]/sum(x$sales[(n-3):(0+n)]))
 n=4+n
}

x$sales = y

ggplot(x)+      #normalizzato
  geom_col(aes(
    x=month,
    y=sales,
    fill=city))+
  scale_x_continuous(breaks= seq(1,12,1))+
  labs(x="Mesi",
       y="Total Sales",
       title="Real Estate Sales Trend over Time")+
  facet_wrap(year)
