meta = read.table("metastudy assessments.csv", header=T,sep="\t")

# set colnames
colnames(meta2) = c("assessor","journal","volume","issue","pages","evidence","corpusbased","quantitative","frequencies_are_just_mentioned","annotation","enriched","size_of_dataset_is_defined","additional_variables","hypothesis_mentioned","type_or_token_frequencies","data_availability","wordlists","Stat.tech","absolute_frequencies","percentages","basic_null_hypothesis_tests","linear_regresssion","philogenetic_trees","PCA","Comment")

# drop some rows that are not linguistic / historical

meta2 = meta[-c(6, 26 ),]
dim(meta2)

meta2 = droplevels(meta2)

meta2$neutral = ifelse(meta2$Comment == "Grammar writing, not really linguistic" |
meta2$Comment == "Mainly methodological" | meta2$Comment == "methodological paper" | meta2$Comment == "Theoretical overivew" |
meta2$Comment == "Theoretical paper", T, F)

summary(meta2$neutral)
#   Mode   FALSE    TRUE    NA's
#logical       6       5      56
#

meta3 = meta2[meta2$neutral == F | is.na(meta2$neutral),]
meta3 = droplevels(meta3)
dim(meta3)
# [1] 62 26

meta.qq = xtabs(~corpus_based + quantitative, meta3)
meta.qq
#            quantitative
#corpus_based FALSE TRUE
#       FALSE    33   11
#       TRUE      4   14
#
#

chisq.test(meta.qq)
#
#        Pearson's Chi-squared test with Yates' continuity correction
#
#data:  meta.qq
#X-squared = 12.675, df = 1, p-value = 0.0003706
#

source("cramer.r")
 cv.test(meta.qq)
#
# - - - - - - - - - - - - - - - - - - - - -
# Effect size for contingency tables
# Data: meta.qq
# df: 1
# Phi: 0.488363
# Pseudo R^2: 0.238498
# T-shirt effect size: medium
# - - - - - - - - - - - - - - - - - - - - -
# Warning: effect sizes are only guidelines!
#


# Addition 7/12-2015: See results without LVC
meta3.noLVC = meta3[meta3$journal != "LangVarChange",]
meta3.noLVC = droplevels(meta3.noLVC)
meta.q = xtabs(~corpus_based + quantitative, meta3.noLVC)
meta.q
#             quantitative
#corpus_based FALSE TRUE
#       FALSE    33    6
#       TRUE      4    7
#

sum(meta.q)
# [1] 50


prop.table(meta.q)
#            quantitative
#corpus_based FALSE TRUE
#       FALSE  0.66 0.12
#       TRUE   0.08 0.14
#       

chisq.test(meta.q)
#
#        Pearson's Chi-squared test with Yates' continuity correction
#
#data:  meta.q
#X-squared = 8.0262, df = 1, p-value = 0.004611
#

cv.test(meta.q)
#
# - - - - - - - - - - - - - - - - - - - - - 
# Effect size for contingency tables 
# Data: meta.q 
# df: 1 
# Phi: 0.455690 
# Pseudo R^2: 0.207653 
# T-shirt effect size: medium 
# - - - - - - - - - - - - - - - - - - - - - 
#

library(xtable)
xtable(meta.q, digits=0)

% latex table generated in R 3.1.2 by xtable 1.7-4 package
% Mon Dec 07 23:24:12 2015
\begin{table}[ht]
\centering
\begin{tabular}{rrr}
  \hline
 & FALSE & TRUE \\ 
  \hline
FALSE & 33 & 6 \\ 
  TRUE & 4 & 7 \\ 
   \hline
\end{tabular}
\end{table}

(6+7)/sum(meta.q)
# 0.26
(4+7)/sum(meta.q)
# 0.22



library(ca)

meth_journ.ca = mjca(meta3[,c(2, 7, 8) ])
summary(meth_journ.ca)
#Principal inertias (eigenvalues):
#
# dim    value      %   cum%   scree plot
# 1      0.244297  90.9  90.9  *************************
# 2      0.002162   0.8  91.7
# 3      00000000   0.0  91.7
# 4      00000000   0.0  91.7
#        -------- -----
# Total: 0.268796
#


plot(meth_journ.ca, map="symbiplot", mass=T, xlab="Dim 1: 90.9%", ylab="Dim 2: 0.8%", pch=16, col=c("gray","gray"), main="MCA plot, methods and journals")
box()

 meth_journ.ca$colnames
#[1] "journal"      "corpus_based" "quantitative"

  meth_journ.ca$colnames <- c("jrnl", "corp_bsd", "quant")
plot(meth_journ.ca, map="symbiplot", mass=T, xlab="Dim 1: 90.9%", ylab="Dim 2: 0.8%", pch=16, col=c("gray","gray"), main="MCA plot, methods and journals")
box()


plot(as.matrix(table(meta3$corpus_based, meta3$journal))[2,], as.matrix(table(meta3$quantitative, meta3$journal))[2,],
type="n", xlab="Corpus based articles", ylab="Quantitative articles")
text(as.matrix(table(meta3$corpus_based, meta3$journal))[2,], as.matrix(table(meta3$quantitative, meta3$journal))[2,],
names(as.matrix(table(meta3$corpus_based, meta3$journal))[2,]), cex=0.8)




for (i in 1:nrow(meta2)) {
  if (is.na(meta2[i,26])){
    tmp = F
  }
  else {
  if (meta2[i,26] == T) {
    tmp = T
   }
   else {
    tmp = F
   }
   }
   if (i == 1) {
    res = tmp
   }
   else {
    res = rbind(res, tmp)
   }
 }
 summary(res)
 
 meta2$neutral2 = res[,1]
 
 
 meth_journ.ca2 = mjca(meta2[,c(2, 7, 8, 27) ])
summary(meth_journ.ca2)

# numbers from Fig 1 in Sampson 2013 (the empirical trend 10 years on)
lyears = c(1960, 1962, 1965, 1967, 1970, 1972, 1975, 1977,
1980, 1982, 1985, 1987, 1990, 1992, 1995, 1998, 2001, 2003, 2006, 2008, 2011)

lemp = c(0.45, 0.37, 0.22, 0.35, 0.29,  0.31, 0.37, 0.47, 0.51,
0.5, 0.51, 0.53, 0.52, 0.55, 0.65, 0.67, 0.72, 0.68, 0.8, 0.8, 0.85)

# 1950 = 0.71

plot(lyears, lemp, type="l", col="darkgray", lwd=3, ylim=c(0,1), xlab="Year", ylab="Proportion of empirical articles",
main="Empirical articles published in Language")
abline(h=0.71, lty=2)

#lines(loess(lemp~lyears), col="red")





