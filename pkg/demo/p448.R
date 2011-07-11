#line 4 "p448.Rnw"
library(fracdiff)
args(fdGPH)


#line 11 "p448.Rnw"
data(gnptbill,package = "RcompHam94")
print( fdGPH(log(gnptbill[,"GNP"]) ) )
print( fdGPH(gnptbill[,"TBILL"]) )


