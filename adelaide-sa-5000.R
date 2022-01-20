#NOTE: codes are same for all four cities, the only different is the input of city 
packages = c("rvest", "tidyverse","ggmap","mgcv")
      
#######################################################################################################################################
#library packages, if package is no installed ,installed and then libray
#######################################################################################################################################
## Now load or install&load all
package.check <- lapply( packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#######################################################################################################################################
#define which city will be run
#######################################################################################################################################
#define list of cities
citylist<-data.frame(adelaide="adelaide-sa-5000",perth="perth-wa-6000",sydney="sydney-nsw-2000",melbourne="melbourne-vic-3000")


i=1

strRemove <- str_sub(as.character(citylist[1,i]) , start= -4)
                                  

#######################################################################################################################################
#collect property individual website
#######################################################################################################################################


url_left <- "https://www.domain.com.au/sale/"
url_right <- "/apartment/?excludeunderoffer=1&ssubs=0&sort=dateupdated-desc&page=1"

url <- paste0(url_left,as.character(citylist[1,i]),url_right)
#grab total number of apartment on market
totalapartment <- read_html(url) %>% html_nodes("strong")%>%str_extract("[[:digit:]]+")%>%as.numeric()
#calculate how many pages to list 
totalpage=round(totalapartment/20)+1
tmppaste<-str_sub(url,1,str_length(url)-1)


#collect property individual website
for(i in 1:totalpage){
#cat(i,"")
    if(i==1){
    
tmpurl<-paste(tmppaste,i,sep="")

try <- read_html(tmpurl) %>% html_nodes(".css-1b4kfhp,a") %>% html_attr(name = "href")

        endnote=paste0(tmppaste,i+1)
        end=which(try==endnote)[1]
        
tmp<-try[53:(end-1)]%>%as.data.frame()%>%drop_na()

tmp<-tmp[!duplicated(tmp[,1]),1]

    subsettry<-as.vector(tmp)
    
    }else{
        tmpurl<-paste(tmppaste,i,sep="")

try <- read_html(tmpurl) %>% html_nodes(".css-1b4kfhp,a") %>% html_attr(name = "href")

        endnote=paste0(tmppaste,i-1)
end=which(try==endnote)[1]
tmp<-try[53:(end-1)]%>%as.data.frame()%>%drop_na()

tmp<-tmp[!duplicated(tmp[,1]),1]

    subsettry<-c(subsettry,as.vector(tmp))
    
    }
    }

subsettry=subsettry[1:totalapartment]

#######################################################################################################################################
#collect basic information for each property#
#######################################################################################################################################

#basic information 
allinfo=matrix(NA,length(subsettry),7)
colnames(allinfo)=c("price","address","roominfo","bathinfo","parkinginfo","description","agent")

for(i in 1:length(subsettry)){
   # cat(i,"")
    tmpurl=subsettry[i]
    #check whether price is empty or not 
    if(length(read_html(tmpurl) %>% html_nodes(".css-1texeil") %>% html_text())!=0){
        #collect price        
        allinfo[i,"price"] <- read_html(tmpurl) %>% html_nodes(".css-1texeil") %>% html_text()
        #collect address        

allinfo[i,"address"] <- read_html(tmpurl) %>% html_node (".css-164r41r") %>% html_text()
        tmp<- read_html(tmpurl) %>% html_nodes(".css-1rzse3v") %>% html_text()
    #collect bed/bath/parking information  
    allinfo[i,"roominfo"]<-str_sub(tmp,start=1,end=1)[1]
    allinfo[i,"bathinfo"]<-str_sub(tmp,start=1,end=1)[2]
    allinfo[i,"parkinginfo"]<-str_sub(tmp,start=1,end=1)[3]

    
#collect description
    tmp2=read_html(tmpurl) %>% html_nodes("p") %>% html_text()
    if(length(str_which(tmp2, c("First listed")))!=0){
        tmp2<-tmp2[1:(str_which(tmp2, c("First listed"))-1)]
    }else{
        if(length(str_which(tmp2, c("\n")))!=0){
            tmp2<-tmp2[1:(str_which(tmp2, c("\n"))-1)]
        }else{
            tmp2<-tmp2[1:(str_which(tmp2, c("home inspections"))-1)]
        }
    }
    
    if(length(str_which(tmp2, c("AGENT")))!=0){
        tmp2<-tmp2[-(str_which(tmp2, c("AGENT")))]
    }
        tmp3=read_html(tmpurl) %>% html_nodes("p.listing-details__unverified-feature-tooltip") %>% html_text()
        if(length(tmp3)!=0){
            tmp2=tmp2[-1]
        }else{
            tmp2=tmp2
            }
        
        
    allinfo[i,"description"]<-str_c(tmp2,collapse="")
 #collect agent information    
allinfo[i,"agent"]<- read_html(tmpurl) %>% html_nodes(".css-wrndla") %>% html_text()
    }
}

allinfo=data.frame(allinfo,link=subsettry)
#######################################################################################################################################
#clean up advertised price #
#######################################################################################################################################
                                 
#remove incomplete address
address=str_remove(allinfo[,"address"],strRemove)
fulladdress=str_detect(address,"[[:digit:]]")
allinfo=allinfo[fulladdress,]

allinfo=drop_na(as.data.frame(allinfo))
                                 
#split data into two part: one with price, other without price information 
priceinfo=str_detect(allinfo[,1], "\\$") &str_detect(allinfo[,1], "[[:digit:]]")

actualprice=allinfo[priceinfo,]

noprice=allinfo[!priceinfo,]
                  
#try to make price format more consistently 
actualprice[,1]=str_squish(actualprice[,1])
actualprice[,1]=str_replace(actualprice[,1],pattern="~",replacement="-")
actualprice[,1]=str_replace(actualprice[,1],pattern="to",replacement="-")
actualprice[,1]=str_replace_all(actualprice[,1],pattern="k",replacement=",000")
actualprice[,1]=str_replace_all(actualprice[,1],pattern="K",replacement=",000")



options(scipen = 999)

#extract price and calulate mean of price if price is in range format 

grabpriceMean=rep(NA,nrow(actualprice))
for(i in 1:nrow(actualprice)){
    #cat(i,"")

    if(length(str_extract_all(actualprice[i,1],"[[:digit:]]+\\,[[:digit:]]+\\,[[:digit:]]+")[[1]])!=0){
        tmp=str_extract_all(actualprice[i,1],"[[:digit:]]+\\,[[:digit:]]+\\,[[:digit:]]+")[[1]]
        tmp=tmp[1:length(tmp)]
        if(length(tmp)==1){
                grabpriceMean[i]= as.numeric(str_c(str_extract_all(tmp,"[0-9]")[[1]],collapse=""))

        }else{
            grabpriceMean[i]= mean(as.numeric(c(str_c(str_extract_all(tmp,"[0-9]")[[1]],collapse=""),str_c(str_extract_all(tmp,"[0-9]")[[2]],collapse=""))))
        }
        }else{

            tmp=str_extract_all(actualprice[i,1],"[[:digit:]]+\\,[[:digit:]]+")[[1]]
            if(length(tmp)==1){
                grabpriceMean[i]= as.numeric(str_c(str_extract_all(tmp,"[0-9]")[[1]],collapse=""))
            }
            if(length(tmp)==2){
                
     grabpriceMean[i]= mean(as.numeric(c(str_c(str_extract_all(tmp,"[0-9]")[[1]],collapse=""),str_c(str_extract_all(tmp,"[0-9]")[[2]],collapse=""))))


            }
            if(length(tmp)==0 && length(str_extract_all(actualprice[i,1],"[[:digit:]]+")[[1]])==1){
               grabpriceMean[i]=as.numeric(str_extract_all(actualprice[i,1],"[[:digit:]]+")[[1]])
                
            }
            
}
}
#move these properties which are unsuccessfully convert price to numeric numbers

isna=is.na(grabpriceMean)


noprice=rbind(noprice,actualprice[isna,])
actualprice=actualprice[!isna,]
grabpriceMean=grabpriceMean[!isna]

#double check if there is any price less than 100000, move to noprice group 
doublecheck=grabpriceMean<=100000
noprice=rbind(noprice,actualprice[doublecheck,])
actualprice=actualprice[!doublecheck,]
grabpriceMean=grabpriceMean[!doublecheck]

#replace price with new numeric format
actualprice[,"price"]=grabpriceMean


#######################################################################################################################################
#fit linear model with bed/bath/parking information only #
#######################################################################################################################################

mydata=actualprice
mydata[,"price"]=grabpriceMean

#convert any information with "-" to 0 
mydata[,"roominfo"]=as.character(mydata[,"roominfo"])
mydata[mydata[,"roominfo"]=="−","roominfo"]=0
mydata[,"roominfo"]= as.numeric(mydata[,"roominfo"])


mydata[,"bathinfo"]=as.character(mydata[,"bathinfo"])
mydata[mydata[,"bathinfo"]=="−","bathinfo"]=0
mydata[,"bathinfo"]= as.numeric(mydata[,"bathinfo"])

mydata[,"parkinginfo"]=as.character(mydata[,"parkinginfo"])
mydata[mydata[,"parkinginfo"]=="−","parkinginfo"]=0
mydata[,"parkinginfo"]= as.numeric(mydata[,"parkinginfo"])

#combine bed_bath_parking information together and treat them as factor 
roomfit=as.factor(apply(mydata[,3:5],1,function(x){paste(x,collapse="_")}))

newmydata=data.frame(room=roomfit,mydata)
#fit linear model with  bed_bath_parking information 
model1 <- lm(price~ room,data = newmydata)

#######################################################################################################################################
#fit generalised additive model  with bed/bath/parking information and level #
#######################################################################################################################################
#becaue analysis is for apartment only, property in higher level will be more expensive than these in lower level 

newmydata2=mydata
#detect which apartment may have level information 
list1=str_detect(newmydata2[,"address"],"/")
newmydata2=newmydata2[list1,]

levelinfo=str_detect(newmydata2[,"address"],"Level")
temp1=newmydata2[levelinfo,]

temp2=newmydata2[!levelinfo,]

#remove  apartment which contain character in address before "/", for example: 2c/11 ** street, Adelaide  
list2=apply(as.matrix(temp2[,"address"]),1,function(x){str_split(x,"/")[[1]][1]})
needremove=list2%>%str_to_upper()%>%str_detect("[A-Z]")
temp2=temp2[!needremove,]
list2=list2[!needremove]

#only retain apartment with level in more than 3 digit 
list3=str_length(str_squish(list2))>=3
temp2=temp2[list3,]

list4=list2[list3]

#collect level information 
level=rep(NA,length(list4))
for(i in 1:length(list4)){
    tmp=str_split(list4[i],"")[[1]]
    if(length(tmp)==3){
        level[i]=tmp[1]
        }else{
      
            level[i]=paste0(tmp[1:2],collapse="")
        }
   
}

isna=is.na(as.numeric(level))
level=level[!isna]
temp2=temp2[!isna,]


#combine bed/bath/parkinginformation 
roomfit2=apply(temp2[,3:5],1,function(x){paste(x,collapse="_")})

newwithlevel=data.frame(room=roomfit2,temp2,level=as.numeric(level))

newwithlevel[,"room"]=as.factor(newwithlevel[,"room"])

#fit GAM model, level do not have linear relationship with price, so we try to smoothing level 
model2 <- gam(price~ room+s(level),   data = newwithlevel, method = "REML")

#######################################################################################################################################
#prediction using the linear model with bed/bath/parking (factor) #
#######################################################################################################################################

mydata=allinfo

mydata[,"roominfo"]=as.character(mydata[,"roominfo"])
mydata[mydata[,"roominfo"]=="−","roominfo"]=0
mydata[,"roominfo"]= as.numeric(mydata[,"roominfo"])


mydata[,"bathinfo"]=as.character(mydata[,"bathinfo"])
mydata[mydata[,"bathinfo"]=="−","bathinfo"]=0
mydata[,"bathinfo"]= as.numeric(mydata[,"bathinfo"])

mydata[,"parkinginfo"]=as.character(mydata[,"parkinginfo"])
mydata[mydata[,"parkinginfo"]=="−","parkinginfo"]=0
mydata[,"parkinginfo"]= as.numeric(mydata[,"parkinginfo"])

room=apply(mydata[,3:5],1,function(x){paste(x,collapse="_")})

mydata=data.frame(room,mydata)
sel=room%in%roomfit
newmydata=mydata[sel,]

newmydata[,"room"]=as.factor(newmydata[,"room"])
predict1=predict(model1,newdata=newmydata)
model1matrix=cbind(newmydata,predict1)

#######################################################################################################################################
#prediction using the GAM model with bed/bath/parking (factor) and level  #
#######################################################################################################################################


list1=str_detect(newmydata[,"address"],"/")
#protenial have level
temp=newmydata[list1,]
#protentially do not have level 
temp2=newmydata[!list1,]

#grab level information from all data, not only property with numeric price information 
list2=apply(as.matrix(temp[,"address"]),1,function(x){str_split(x,"/")[[1]][1]})
needremove=list2%>%str_to_upper()%>%str_detect("[A-Z]")
temp2=rbind(temp2,temp[needremove,])
temp=temp[!needremove,]
list2=list2[!needremove]

list3=str_length(str_squish(list2))>=3
temp2=rbind(temp2,temp[!list3,])
temp=temp[list3,]
list4=list2[list3]

level=rep(NA,length(list4))
for(i in 1:length(list4)){
    tmp=str_split(list4[i],"")[[1]]
    if(length(tmp)==3){
        level[i]=tmp[1]
        }else{
      
            level[i]=paste0(tmp[1:2],collapse="")
        }
   
}
isna=is.na(as.numeric(level))
level=level[!isna]
temp=temp[!isna,]
withfloor=data.frame(temp,level=as.numeric(level))



withoutfloor=newmydata[!newmydata[,"address"]%in%withfloor[,"address"],]

sel=withfloor[,"room"]%in%roomfit2

withfloor=withfloor[sel,]
withfloor[,"room"]=as.factor(withfloor[,"room"])


sel2=withoutfloor[,"room"]%in%roomfit
withoutfloor=withoutfloor[sel2,]
withoutfloor[,"room"]=as.factor(withoutfloor[,"room"])


predictwithfloor=predict(model2,newdata=withfloor)
predictwithoutfloor=predict(model1,newdata=withoutfloor)
predictwithfloor=data.frame(predictedprice=predictwithfloor,withfloor)
predictwithfloor=predictwithfloor[,-which(colnames(predictwithfloor)%in%"level")]
predictwithoutfloor=data.frame(predictedprice=predictwithoutfloor,withoutfloor)

predictpriceWithlevel=rbind(predictwithfloor,predictwithoutfloor)


#######################################################################################################################################
#combine predicted price from two models with original property information   #
#######################################################################################################################################

m=match(predictpriceWithlevel[,"address"],model1matrix[,"address"])
allprice= data.frame(agentprice=model1matrix[m,"price"],nonlevelprice=round(model1matrix[m,"predict1"],digit=-3),levelprice= round(predictpriceWithlevel[,"predictedprice"],digit=-3))

priceranges=matrix(NA,nrow(allprice),1)

for(i in 1:nrow(allprice)){
    tempmin=min(allprice[i,2:3])
        tempmax=max(allprice[i,2:3])

    if(tempmin==tempmax){
        priceranges[i]=paste("$",format(tempmin,scientific=FALSE, big.mark=","),sep="")
    }else{
        priceranges[i]=paste(paste("$",format(tempmin,scientific=FALSE, big.mark=","),sep=""),paste("$",format(tempmax,scientific=FALSE, big.mark=","),sep=""),sep="-")
        
    }
}
allprice=data.frame(allprice,priceranges)
m=match(predictpriceWithlevel[,"address"],model1matrix[,"address"])

allinformation=data.frame(model1matrix[m,-which(colnames(model1matrix)%in%c("price","predict1"))],allprice)

predictedpriceMean=rowMeans(cbind(allinformation[,"nonlevelprice"],allinformation[,"levelprice"]))

m=match(allinformation[,"address"],actualprice[,"address"])
m=m[!is.na(m)]
actualprice= actualprice[m,]
pricemean=predictedpriceMean
pricemean[match(actualprice[,"address"],allinformation[,"address"])]=actualprice[,"price"]
allinformation=data.frame(allinformation,pricemean,predictedpriceMean)
#rename column names 
colnames(allinformation)[which(colnames(allinformation)=="room")]="Bed.Bath.Parking"
colnames(allinformation)[which(colnames(allinformation)=="agentprice")]="advertised.price"
colnames(allinformation)[which(colnames(allinformation)=="priceranges")]="predicted.price"
colnames(allinformation)[which(colnames(allinformation)=="pricemean")]="newpricemax"
colnames(allinformation)[which(colnames(allinformation)=="predictedpriceMean")]="predictprice"
colnames(allinformation)[which(colnames(allinformation)=="roominfo")]="bed"
colnames(allinformation)[which(colnames(allinformation)=="bathinfo")]="bath"
colnames(allinformation)[which(colnames(allinformation)=="parkinginfo")]="parking"
colnames(allinformation)[which(colnames(allinformation)=="levelprice")]="price.model.2"
colnames(allinformation)[which(colnames(allinformation)=="nonlevelprice")]="price.model.1"

allinformation=allinformation[!duplicated(allinformation[,"address"]),]
trimedaddress= str_squish(str_replace_all(allinformation[,"address"]," Adelaide SA 5000",""))

allinformation=data.frame(allinformation,trimedaddress)





#######################################################################################################################################
# this part is for preparing data to generate  map popup   #
#######################################################################################################################################
# test google API with a address, if return noapi, do nothing, else if google API is set up in R console,  it will collect Longitude and Latitude information for map pop-up
testing=tryCatch(geocode("1600 Amphitheatre Parkway, Mountain View, CA"),error=function(cond){return("noapi")})

if(testing[1]!="noapi"){

    location=geocode(allinformation[,"address"])
location=as.data.frame(location)
colnames(location)=c("Longitude","Latitude")
    allinformation=data.frame(allinformation,location)
}else{
    allinformation=allinformation
}

allinformation=drop_na(allinformation)


