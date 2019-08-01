setwd("C:/Users/chaitanya/Data Science in R/project")

getwd()

a=read.csv("housing_test.csv",stringsAsFactors = F)

b=read.csv("housing_train.csv",stringsAsFactors = F)

View(a)

View(b)

a$Price=NA

a$data="test"
b$data="train"

c=rbind(a,b)

library(dplyr)

glimpse(c)


lapply(c,function(x) sum(is.na(x)))

var(c$Distance)

library(ggplot2)
df=data.frame(c)

unique(c$Postcode)

var(max(c$CouncilArea))


c$Bedroom2[is.na(c$Bedroom2)]=median(c$Bedroom2,na.rm=T)

c$Bathroom[is.na(c$Bathroom)]=median(c$Bathroom,na.rm=T)

c$Car[is.na(c$Car)]=median(c$Car,na.rm=T)

c$Landsize[is.na(c$Landsize)]=median(c$Landsize,na.rm=T)

c$BuildingArea[is.na(c$BuildingArea)]=median(c$BuildingArea,na.rm = T)

c$YearBuilt[is.na(c$YearBuilt)]=median(c$YearBuilt,na.rm = T)

glimpse(c)

p=table(c$Suburb)
View(p)
p1=round(tapply(c$Price,c$Suburb,mean,na.rm=T),0)
View(p1)
p1=sort(p1)

            v  =    table(c$Suburb)
            sort(v)
            
            max(c$SellerG)
           max(c$CouncilArea)
           
           var(c$CouncilArea)
          
c=c %>% 
  mutate(
    sub_1=as.numeric(Suburb%in%c("Campbellfield","Jacana")),
    sub_2=as.numeric(Suburb%in%c("Kealba","Brooklyn","Albion","Sunshine West","Ripponlea","Fawkner")),
    sub_3=as.numeric(Suburb%in%c("Glenroy","Southbank","Sunshine North","Keilor Park","Heidelberg West","Reservoir","Braybrook","Kingsbury","Gowanbrae","Hadfield","Watsonia","Footscray","South Kingsville","Balaclava","Melbourne","Maidstone","Sunshine")),
    sub_4=as.numeric(Suburb%in%c("Airport West","Heidelberg Heights","Pascoe Vale","West Footscray","Altona North","Williamstown North","Brunswick West","Keilor East","Oak Park","Maribyrnong","Altona","Flemington","Coburg North","Yallambie","Avondale Heights","Bellfield")),
    sub_5=as.numeric(Suburb%in%c("Strathmore Heights","Glen Huntly","Kensington","Essendon North","St Kilda","Preston","North Melbourne","Coburg","Kingsville","Collingwood","Brunswick East","Gardenvale","Thornbury","Niddrie","West Melbourne","Viewbank")),
    sub_6=as.numeric(Suburb%in%c("Spotswood","Carnegie","Elwood","Heidelberg","Moorabbin","Oakleigh","Rosanna","Docklands","Yarraville","Cremorne","Seddon","Brunswick","Oakleigh South","Ascot Vale","Windsor","Caulfield","Essendon West","Newport")),
    sub_7=as.numeric(Suburb%in%c("Chadstone","South Yarra","Essendon","Bentleigh East","Murrumbeena","Hughesdale","Fairfield","Ashwood","Clifton Hill","Caulfield North","Abbotsford","Carlton","Prahran","Fitzroy","Ivanhoe","Hampton East","Caulfield East")),
    sub_8=as.numeric(Suburb%in%c("Richmond","Travancore","Templestowe Lower","Ormond","Caulfield South","Moonee Ponds","Hawthorn","Box Hill","Bulleen","Burnley","Burwood","Strathmore","Port Melbourne","Fitzroy North","Alphington")),
    sub_9=as.numeric(Suburb%in%c("Doncaster","South Melbourne","Northcote","Aberfeldie","Elsternwick","Bentleigh","Kooyong","Parkville")),
    sub_10=as.numeric(Suburb%in%c("Williamstown","East Melbourne","Seaholme")),
    sub_11=as.numeric(Suburb%in%c("Malvern East","Carlton North","Hawthorn East","Surrey Hills")),
    sub_12=as.numeric(Suburb%in%c("Princes Hill","Mont Albert","Armadale","Kew East","Glen Iris","Ashburton")),
    sub_13=as.numeric(Suburb%in%c("Brighton East","Eaglemont","Hampton")),
    sub_14=as.numeric(Suburb%in%c("Toorak","Ivanhoe East","Camberwell","Balwyn North","Kew")),
    sub_15=as.numeric(Suburb%in%c("Brighton","Middle Park")),
    sub_16=as.numeric(Suburb%in%c("Albert Park","Balwyn","Malvern"))
  ) %>% 
  select(-Suburb)

glimpse(c)


#Unique variable
c=c %>% 
  select(-Address)


glimpse(c)
table(c$Type)

c=c%>%
  mutate(type_h=as.numeric(Type=="h"),
         type_t=as.numeric(Type=="t"))
c=c%>% 
  select(-Type)

glimpse(c)

table(c$Method)

c=c %>%
  mutate(Method_PI=as.numeric(Method=="PI"),
         Method_SA=as.numeric(Method=="SA"),
         Method_SP=as.numeric(Method=="SP"),
         Method_VB=as.numeric(Method=="VB")) %>% 
  select(-Method)

glimpse(c)

p=table(c$SellerG)
sort(p, decreasing = T)

library(dplyr)
c=c %>%
  mutate(Gnelson=as.numeric(SellerG=="Nelson"),
         GJellis=as.numeric(SellerG=="Jellis"),
         Ghstuart=as.numeric(SellerG=="hockingstuart"),
         Gbarry=as.numeric(SellerG=="Barry"),
         GMarshall=as.numeric(SellerG=="Marshall"),
         GWoodards=as.numeric(SellerG=="Woodards"),
         GBrad=as.numeric(SellerG=="Brad"),
         GBiggin=as.numeric(SellerG=="Biggin"),
         GRay=as.numeric(SellerG=="Ray"),
         GFletchers=as.numeric(SellerG=="Fletchers"),
         GRT=as.numeric(SellerG=="RT"),
         GSweeney=as.numeric(SellerG=="Sweeney"),
         GGreg=as.numeric(SellerG=="Greg"),
         GNoel=as.numeric(SellerG=="Noel"),
         GGary=as.numeric(SellerG=="Gary"),
         GJas=as.numeric(SellerG=="Jas"),
         GMiles=as.numeric(SellerG=="Miles"),
         GMcGrath=as.numeric(SellerG=="McGrath"),
         GHodges=as.numeric(SellerG=="Hodges"),
         GKay=as.numeric(SellerG=="Kay"),
         GStockdale=as.numeric(SellerG=="Stockdale"),
         GLove=as.numeric(SellerG=="Love"),
         GDouglas=as.numeric(SellerG=="Douglas"),
         GWilliams=as.numeric(SellerG=="Williams"),
         GVillage=as.numeric(SellerG=="Village"),
         GRaine=as.numeric(SellerG=="Raine"),
         GRendina=as.numeric(SellerG=="Rendina"),
         GChisholm=as.numeric(SellerG=="Chisholm"),
         GCollins=as.numeric(SellerG=="Collins"),
         GLITTLE=as.numeric(SellerG=="LITTLE"),
         GNick=as.numeric(SellerG=="Nick"),
         GHarcourts=as.numeric(SellerG=="Harcourts"),
         GCayzer=as.numeric(SellerG=="Cayzer"),
         GMoonee=as.numeric(SellerG=="Moonee"),
         GYPA=as.numeric(SellerG=="YPA")
  ) %>% 
  select(-SellerG)


          glimpse(c)

          table(c$CouncilArea)          

          c= c %>%
            mutate(CA_Banyule=as.numeric(CouncilArea=="Banyule"),
                   CA_Bayside=as.numeric(CouncilArea=="Bayside"),
                   CA_Boroondara=as.numeric(CouncilArea=="Boroondara"),
                   CA_Brimbank=as.numeric(CouncilArea=="Brimbank"),
                   CA_Darebin=as.numeric(CouncilArea=="Darebin"),
                   CA_Glen_Eira=as.numeric(CouncilArea=="Glen Eira"),
                   CA_Monash=as.numeric(CouncilArea=="Monash"),
                   CA_Melbourne=as.numeric(CouncilArea=="Melbourne"),
                   CA_Maribyrnong=as.numeric(CouncilArea=="Maribyrnong"),
                   CA_Manningham=as.numeric(CouncilArea=="Manningham"),
                   CA_Kingston=as.numeric(CouncilArea=="Kingston"),
                   CA_Hume=as.numeric(CouncilArea=="Hume"),
                   CA_HobsonsB=as.numeric(CouncilArea=="Hobsons Bay"),
                   CA_MoonValley=as.numeric(CouncilArea=="Moonee Valley"),
                   CA_Moreland=as.numeric(CouncilArea=="Moreland"),
                   CA_PortP=as.numeric(CouncilArea=="Port Phillip"),
                   CA_Stonnington=as.numeric(CouncilArea=="Stonnington"),
                   CA_Whitehorse=as.numeric(CouncilArea=="Whitehorse"),
                   CA_Yarra=as.numeric(CouncilArea=="Yarra")) %>% 
            select(-CouncilArea)          

          glimpse(c)   
          
          o=sum(c$type_h)/2
          p=sum(c$type_t)/2
          
          o-p
          
          library(ggplot2) 
          ggplot(c,aes(x=c$Distance))+geom_density(color="red")
          
          
          train=c%>% 
            filter(data=='train') %>% 
            select(-data)          

          test=c %>% 
            filter(data=='test') %>% 
            select(-data,-Price)         
          
          glimpse(train)

          set.seed(123)
          s=sample(1:nrow(train),0.75*nrow(train))
          train_75=train[s,]
          test_25=train[-s,]

          library(car)

          LRf=lm(Price ~ .,data=train_75)
          summary(LRf)          

          a=vif(LRf)
          sort(a,decreasing = T)[1:3]          

          
          LRf=lm(Price ~ .-Postcode-sub_3,data=train_75)
          summary(LRf)
          
          a=vif(LRf)
          sort(a,decreasing = T)[1:3]

          summary(LRf)
          
         # p=step(LRf)
          
        #  formula(p)
          
          LRf=lm(Price ~ Rooms + Distance + Bedroom2 + Bathroom + Car + Landsize + 
                   BuildingArea + YearBuilt + sub_1 + sub_2 + sub_5 + sub_6 + 
                   sub_7 + sub_8 + sub_9 + sub_10 + sub_11 + sub_12 + sub_13 + 
                   sub_14 + sub_15 + sub_16 + type_h + type_t + Method_PI + 
                   Method_SP + Method_VB + GJellis + GMarshall + GRT + GMiles + 
                   GMcGrath + GKay + GDouglas + GWilliams + GRaine + GCollins + 
                   CA_Banyule + CA_Bayside + CA_Boroondara + CA_Brimbank + CA_Darebin + 
                   CA_Monash + CA_Melbourne + CA_Maribyrnong + CA_Manningham + 
                   CA_Kingston + CA_HobsonsB + CA_MoonValley + CA_Moreland + 
                   CA_Stonnington + CA_Whitehorse + CA_Yarra,data=train_75)
          
          summary(LRf)
          
          a_test_25=predict(LRf,newdata =test_25)
          
          View(a_test_25)
          
          a_test_25=round(a_test_25,1)
          
          class(a_test_25)  
          
          plot(test_25$Price,a_test_25)
          
          var(a_test_25)
          
          
          #RMSE
          
          res=test_25$Price-a_test_25 #(real value-predicted value)
          #root mean square error is as follows
          RMSE_test_25=sqrt(mean(res^2))
          RMSE_test_25
          
          212467/RMSE_test_25

          a_test_final=predict(LRf,newdata =test)
          a_test_final=round(a_test_final,1)
          class(a_test_final)          
            
          write.csv(a_test_final, "a_test_final.csv")
          
          summary(LRf)
          
          View(a_test_final)
          
          range(a_test_final)

           table(c$type_h,c$Price)          
          