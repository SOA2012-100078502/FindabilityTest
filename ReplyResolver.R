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


###working on it

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