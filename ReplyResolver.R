library(XML)

#
#Notice!
#Before run the script, has to turn
#<Fare_MasterPricerTravelBoardSearchReply xmlns="http://xml.amadeus.com/FMPTBR_14_3_1A">
#into
#<Fare_MasterPricerTravelBoardSearchReply>
#to avoid possible reading error
#

#
#read data into XML document
query1<-xmlTreeParse(file="2RS_Reply_TPEDUS.xml", encoding="UTF-8", useInternalNodes = TRUE)
# or use
#xmlParse(file="", encoding="UTF-8")
#

#
#processing based on Reco
#final attributes: flightcomb, fares, vc and RC item
#
Range<-xmlApply(getNodeSet(query1,"//Fare_MasterPricerTravelBoardSearchReply/recommendation/itemNumber/itemNumberId/number"),xmlValue, recursive=FALSE)
RCpart4<-data.frame()

for( i in Range ){

RCitem<-i

path1<-paste("//Fare_MasterPricerTravelBoardSearchReply/recommendation[",i,"]/segmentFlightRef",sep="")

RCFL<-xmlApply(getNodeSet(query1,path1),xmlValue,recursive=TRUE)

RCpart1<-data.frame(matrix(toString(unlist(RCFL)), ncol=1, byrow=TRUE))
RCpart1<-cbind(RCitem,RCpart1)

path2<-paste("//Fare_MasterPricerTravelBoardSearchReply/recommendation[",i,"]/paxFareProduct/paxFareDetail/
                         codeShareDetails[1]",sep="")

RCCS<-xmlApply(getNodeSet(query1,path2),xmlValue,recursive=TRUE)
RCpart2<-data.frame(matrix(unlist(RCCS),ncol=1,byrow = TRUE))
RCpart2<-cbind(RCpart1,RCpart2)

path3<-paste("//Fare_MasterPricerTravelBoardSearchReply/recommendation[",i,"]/paxFareProduct/paxFareDetail/
                         totalFareAmount",sep="")

RCFR<-xmlApply(getNodeSet(query1,path3),xmlValue,recursive=FALSE)

RCpart3<-data.frame(matrix(unlist(RCFR),ncol=1,byrow = TRUE))
RCpart3<-cbind(RCpart2,RCpart3)

RCpart4<-rbind(RCpart4,RCpart3)

}

colnames(RCpart4)<-c("RCitem","FlightComb","VCarrier","TotalFare")

#
### below part is still working on it
#

#
#RS1 flight proposal
#
RS1FP<-xmlApply(getNodeSet(query1,
                           "//Fare_MasterPricerTravelBoardSearchReply/
                           flightIndex[requestedSegmentRef=1]/
                           groupOfFlights/
                           propFlightGrDetail/flightProposal/ref"),xmlValue,recursive=TRUE)
RS1FP<-data.frame(matrix(unlist(RS1FP),ncol=3,byrow=TRUE))
colnames(RS1FP)<-c("FP1_item","FP1_EFT","FP2_MCX")
RS1FP$FP1_item<-paste("S",RS1FP$FP1_item,sep = "")

#
#RS2 flight proposal
#

RS2FP<-xmlApply(getNodeSet(query1,
                           "//Fare_MasterPricerTravelBoardSearchReply/
                           flightIndex[requestedSegmentRef=2]/
                           groupOfFlights/
                           propFlightGrDetail/flightProposal/ref"),xmlValue,recursive=TRUE)
RS2FP<-data.frame(matrix(unlist(RS2FP),ncol=3,byrow=TRUE))
colnames(RS2FP)<-c("FP2_item","FP2_EFT","FP2_MCX")
RS2FP$FP2_item<-paste("S",RS2FP$FP2_item,sep = "")

#
#RS1+RS2 flight proposal
#
FPcomb<-expand.grid(RS1FP$FP1_item,RS2FP$FP2_item)
FPcomb<-data.frame(paste(FPcomb$Var1,FPcomb$Var2,sep=""))
colnames(FPcomb)<-c("2RScomb")

FPcomb[,"CombValid"]<-ifelse(pmatch(FPcomb$`2RScomb`,RCpart4$FlightComb)>0,TRUE,FALSE)
FPcomb2<-subset(FPcomb,FPcomb$CombValid==TRUE)

# idea
# use grep to generate matching int
# use length >0 to get match pair
# matching = ture for sorting





FPcomb2<-subset(FPcomb,FPcomb$CombValid=="TRUE")

RS_all<-cbind(RS1FP,RS2FP)
RS_all[,"CompleteFP"]<-paste(RS1FP$FP1_item,RS2FP$FP2_item,sep="")


#----
RS1<-xmlApply(getNodeSet(query1,
                         "//Fare_MasterPricerTravelBoardSearchReply/
                         flightIndex[requestedSegmentRef=1]/
                         groupOfFlights/
                         propFlightGrDetail"),xmlValue,recursive=TRUE)

RS2<-xmlApply(getNodeSet(query1,
                         "//Fare_MasterPricerTravelBoardSearchReply/
                         flightIndex[requestedSegmentRef=2]/
                         groupOfFlights/
                         propFlightGrDetail"),xmlValue,recursive=TRUE)