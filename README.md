# PEAD
**Description**:Tested the existence of PEAD phenomenon in A-share market of China mainly based on the unexpected earnings index, and studied the factors affecting the phenomenon, including the calculation methods, companies size, the value of SUE(standardized unexpected earnings) etc. 

The code also tests the theory of risk pricing used to illustrate the PEAD pheonomenon based on the Carhart's four-factor model. The four factors are market factor(market return minus risk-free rate), size factor(SMB: small-size minus big-size),value factor(HML: high BV/MV minus low BV/MV), momentum factor(MOM).

**Procedure**:data cleansing and processing(stock selection, report selection)-->data merging-->index calculation(Cumulative abnormal return(CAR),Unexpected Earnings(UE), Standardized Unexpected Earnings(SUE)-->data grouping-->statistic summary-->quantile regression-->analysis and conclusion

**Data source** CSMAR, RESSET
