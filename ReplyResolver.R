library(XML)
library(reshape)

##########################################Notice!##########################################
#Before run the script, has to turn
#<Fare_MasterPricerTravelBoardSearchReply xmlns="http://xml.amadeus.com/FMPTBR_14_3_1A">
#into
#<Fare_MasterPricerTravelBoardSearchReply>
#to avoid possible reading error
##########################################################################################

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
RCFL<-substr(RCFL,1,regexpr("B",RCFL)-1)

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
#RS1 flight proposal
#
RS1FP<-xmlApply(getNodeSet(query1,
                           "//Fare_MasterPricerTravelBoardSearchReply/
                           flightIndex[requestedSegmentRef=1]/
                           groupOfFlights/
                           propFlightGrDetail/flightProposal/ref"),xmlValue,recursive=TRUE)
RS1FP<-data.frame(matrix(unlist(RS1FP),ncol=3,byrow=TRUE))
colnames(RS1FP)<-c("FP1_item","FP1_EFT","FP1_MCX")
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

FPcomb<-data.frame()
FPcomb<-expand.grid(RS1FP$FP1_item,RS2FP$FP2_item)
FPcomb[,"CombCode"]<-data.frame(paste(FPcomb$Var1,FPcomb$Var2,sep=""))
colnames(FPcomb)<-c("FP1_item","FP2_item","CombCode")

FPcomb<-merge(FPcomb,RS2FP,by="FP2_item")
FPcomb<-merge(FPcomb,RS1FP,by="FP1_item")
FPcomb<-FPcomb[,c("FP1_item","FP2_item","CombCode","FP1_EFT","FP1_MCX","FP2_EFT","FP2_MCX")]

#
#Expand FPs in one RC into up to 250 RC*FP table
#

FP_key<-unlist(strsplit(as.character(RCpart4$FlightComb),split=", "))
RC_expand<-str_split_fixed(RCpart4$FlightComb,", ",n=length(unique(FP_key)))
RC_expand<-melt(cbind(RC_expand,RCpart4),id=c("RCitem","FlightComb","VCarrier","TotalFare"))
RC_expand<-subset(RC_expand,value!="")
RC_expand[,"CombCode"]<-RC_expand$value
#############

RC_expand_all<-merge(RC_expand,FPcomb,by="CombCode",all=FALSE)

#####################################endnote of the code##################################
#
#A piece of the old design: obsolete idea to FP-to-unlist-vector-to-RECO mapping
#Building FP to RC mapping vector
#
#FP_key<-unlist(strsplit(as.character(RCpart4$FlightComb),split=", "))
#MatchVector<-as.character()
#MatchVector2<-as.character()
#
#for(i in 1:length(FP_key)){
#  
#  FPmatch_N<-subset(FPcomb,nchar(as.character(FP_key[i]))==nchar(as.character(FPcomb$CombCode)))
#  
#  MatchVector<-match(FP_key[i],FPmatch_N$CombCode,nomatch = 0)
#  MatchVector2<-rbind(MatchVector2,MatchVector)
#  
#}
#
#FPRC_match<-data.frame()
#FPRC_match<-cbind(data.frame(matrix(FP_key,ncol = 1)),data.frame(matrix(MatchVector2,ncol=1)))
#colnames(FPRC_match)<-c("FP_key","MatchVector")
#
#####################################endnote of the code##################################
