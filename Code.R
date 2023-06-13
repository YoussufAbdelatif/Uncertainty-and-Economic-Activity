
library(ggcorrplot)
library(tidyverse)
library(xts)
library(quantmod)
library(ggplot2)
library(gridExtra)
library(tseries)
library(kableExtra)
library(knitr)
library(vars)
library(forecast)
library(xts)
library(stargazer)
library(tsDyn)


############################DATA################################################

getSymbols("WUIGLOBALWEIGHTAVG",src="FRED")
getSymbols("GEPUCURRENT",src="FRED") 
getSymbols("EMVOVERALLEMV",src="FRED") 

getSymbols("GDPC1",src="FRED") 
getSymbols("UNRATE",src="FRED")
getSymbols("GPDIC1",src="FRED")
getSymbols("MEDCPIM158SFRBCLE",src="FRED")

WUI = ts(WUIGLOBALWEIGHTAVG, start = c(1990, 1),end = c(2022,4), frequency = 4)
WUI=window(WUI,start=c(1997,1),end=c(2022,4))
WUI=as.ts(WUI)
GEPUI = ts(GEPUCURRENT, start = c(1997, 1),end = c(2022,12), frequency = 12)
GEPUI=as.ts(GEPUI)
EMV = ts(EMVOVERALLEMV, start = c(1985, 1),end = c(2023,2), frequency = 12)
EMV=window(EMV,start=c(1997,1),end=c(2022,12))
EMV=as.ts(EMV)
GDPC = ts(GDPC1, start = c(1947, 1),end = c(2022,4), frequency = 4)
GDPC=window(GDPC,start=c(1997,1),end=c(2022,4))
GDPC=as.ts(GDPC)
UNRATE = ts(UNRATE, start = c(1948, 1),end = c(2023,2), frequency = 12)
UNRATE=window(UNRATE,start=c(1997,1),end=c(2022,12))
UNRATE=as.ts(UNRATE)
INV = ts(GPDIC1, start = c(1947, 1),end = c(2022,4), frequency = 4)
INV=window(INV,start=c(1997,1),end=c(2022,4))
INV=as.ts(INV)
CPI = ts(MEDCPIM158SFRBCLE, start = c(1983, 1),end = c(2023,2), frequency = 12)
CPI=window(CPI,start=c(1997,1),end=c(2022,12))
CPI=as.ts(CPI)

GEPUI = aggregate.ts(GEPUI, nfrequency = 4,FUN = mean)
EMV = aggregate.ts(EMV, nfrequency = 4,FUN = mean)
UNRATE = aggregate.ts(UNRATE, nfrequency = 4,FUN = mean)
CPI= aggregate.ts(CPI, nfrequency = 4,FUN = mean)

Date=as.Date(seq(as.Date("1997-01-01"),as.Date("2022-12-01"),by="quarter"))


############################CORRELATION#########################################

data = data.frame(WUI, GEPUI, EMV, GDPC, UNRATE, INV, CPI)
colnames(data)=  c("World Uncertainty Index","Global Economic Policy Uncertainty","Equity Market Volatility","RGDP","UNRATE","INV","CPI")
corr_matrix = cor(data)
corplot = ggcorrplot::ggcorrplot(corr_matrix,lab=T,type = "lower")
ggsave("corplot.png", corplot, width = 18, height = 7, dpi = 300)

############################GRAPHICAL ANALYSIS##################################

plot_ts = function(data, variable) {
  ggplot(data, aes(x = Date, y = !!sym(variable))) +
    geom_line() +
    labs(title = variable, subtitle = "1997-2022") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    xlab("Time") +
    ylab("Index Value")+
    theme_classic()
}

# plot each time series
plot_ts(data, "World Uncertainty Index")
plot_ts(data, "Global Economic Policy Uncertainty")
plot_ts(data, "Equity Market Volatility")
plot_ts(data, "RGDP")
plot_ts(data, "UNRATE")
plot_ts(data, "INV")
plot_ts(data, "CPI")

plot1a = grid.arrange(plot_ts(data, "World Uncertainty Index"),plot_ts(data, "Global Economic Policy Uncertainty"),plot_ts(data, "Equity Market Volatility"),ncol=1)
plot1b = grid.arrange(plot_ts(data, "CPI"),plot_ts(data, "INV"),plot_ts(data, "UNRATE"),plot_ts(data, "RGDP"))
ggsave("plot1a.png", plot1a, width = 12, height = 10, dpi = 400)
ggsave("plot1b.png", plot1b, width = 10, height = 8, dpi = 300)


############################STATIONARITY########################################

kpss.test(WUI)
kpss.test(GEPUI)
kpss.test(EMV)
kpss.test(GDPC)
kpss.test(UNRATE)
kpss.test(INV)
kpss.test(CPI)

kpss.test(diff(log(WUI)))
kpss.test(diff(log(GEPUI)))
kpss.test(diff(log(EMV)))
kpss.test(diff(log(GDPC)))
kpss.test(diff(log(UNRATE)))
kpss.test(diff(log(INV)))
kpss.test(diff(log(CPI)))

kpss_results = data.frame(
  Time_Series = c("World Uncertainty Index","Global Economic Policy Uncertainty","Equity Market Volatility","RGDP","UNRATE","INV","CPI"),
  Test_Statistic = c(kpss.test(WUI)$statistic, kpss.test(GEPUI)$statistic, kpss.test(EMV)$statistic, kpss.test(GDPC)$statistic, kpss.test(UNRATE)$statistic, kpss.test(INV)$statistic, kpss.test(CPI)$statistic),
  P_Value = c(kpss.test(WUI)$p.value, kpss.test(GEPUI)$p.value, kpss.test(EMV)$p.value, kpss.test(GDPC)$p.value, kpss.test(UNRATE)$p.value, kpss.test(INV)$p.value, kpss.test(CPI)$p.value)
)
kable(kpss_results, digits = 4, caption = "KPSS test results for time series in levels")%>%
  kable_classic(full_width = F, html_font = "Cambria")


kpss_results = data.frame(
  Time_Series = c("World Uncertainty Index","Global Economic Policy Uncertainty","Equity Market Volatility","RGDP","UNRATE","INV","CPI"),
  Test_Statistic = c(kpss.test(diff(log(WUI)))$statistic, kpss.test(diff(log(GEPUI)))$statistic, 
                     kpss.test(diff(log(EMV)))$statistic, kpss.test(diff(log(GDPC)))$statistic, 
                     kpss.test(diff(log(UNRATE)))$statistic, kpss.test(diff(log(INV)))$statistic, 
                     kpss.test(diff(log(CPI)))$statistic),
  P_Value = c(kpss.test(diff(log(WUI)))$p.value, kpss.test(diff(log(GEPUI)))$p.value, 
              kpss.test(diff(log(EMV)))$p.value, kpss.test(diff(log(GDPC)))$p.value, 
              kpss.test(diff(log(UNRATE)))$p.value, kpss.test(diff(log(INV)))$p.value, 
              kpss.test(diff(log(CPI)))$p.value)
)

kable(kpss_results, digits = 4, caption = "KPSS test results for time series in growth rates")%>%
  kable_classic(full_width = F, html_font = "Cambria")

WUIdiff=(diff(log(WUI)))
GEPUIdiff=(diff(log(GEPUI)))
EMVdiff=(diff(log(EMV)))
GDPCdiff=(diff(log(GDPC)))
UNRATEdiff=(diff(log(UNRATE)))
INVdiff=(diff(log(INV)))
CPIdiff=(diff(log(CPI)))
datadiff = data.frame(WUIdiff, GEPUIdiff, EMVdiff, GDPCdiff, UNRATEdiff, INVdiff, CPIdiff)
colnames(datadiff)=  c("World Uncertainty Index","Global Economic Policy Uncertainty","Equity Market Volatility","RGDP","UNRATE","INV","CPI")
Datediff=as.Date(seq(as.Date("1997-04-01"),as.Date("2022-12-01"),by="quarter"))

plot_ts = function(data, variable) {
  ggplot(data, aes(x = Datediff, y = !!sym(variable))) +
    geom_line() +
    labs(title = variable, subtitle = "1997-2022") +
    scale_x_date(date_labels = "%Y", date_breaks = "5 years") +
    xlab("Time") +
    ylab("Index Value Growth Rate")+
    theme_classic()
}



plot2 = grid.arrange(plot_ts(datadiff, "World Uncertainty Index"),plot_ts(datadiff, "Global Economic Policy Uncertainty"),plot_ts(datadiff, "Equity Market Volatility"),plot_ts(datadiff, "CPI"),plot_ts(datadiff, "INV"),plot_ts(datadiff, "UNRATE"),plot_ts(datadiff, "RGDP"),ncol =2)
ggsave("plot2.png", plot2, width = 10, height = 8, dpi = 300)


############################DENSITY#############################################

den1 = data.frame(WUIdiff,GEPUIdiff,EMVdiff)
dichte1 = ggplot(data=den1) +
  stat_density(aes(x=WUIdiff,fill="black"),adjust=1.5, alpha=.2)+
  stat_density(aes(x=GEPUIdiff,fill="cyan3"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=EMVdiff,fill="red"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(black = "World Uncertainty Index", red ="Equity Market Volatility",
                                 cyan3 = "Global Economic Policy Uncertainty"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Density: Uncertainty Indexes", subtitle = "1997-2022",
       y     = "Density",
       x     = "x") 

dichte1
ggsave("density1.png", dichte1, width = 10, height = 8, dpi = 300)


den2 = data.frame(GDPCdiff,UNRATEdiff,INVdiff,CPIdiff)


dichte2 = ggplot(data=den1) +
  stat_density(aes(x=GDPCdiff,fill="black"),adjust=1.5, alpha=.2)+
  stat_density(aes(x=UNRATEdiff,fill="cyan3"),adjust=1.5, alpha=.4)+  
  stat_density(aes(x=INVdiff,fill="red"),adjust=1.5, alpha=.4) +
  stat_density(aes(x=CPIdiff,fill="gold"),adjust=1.5, alpha=.4) +
  scale_fill_identity(name = NULL, 
                      labels = c(black = "RGDP", red = "Inv",
                                 cyan3 = "Unrate",gold="CPI"
                      ),
                      guide = "legend")+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x     = element_text(angle = 90)) +
  labs(title = "Density: GDP, Unrate, Inv, CPI", subtitle = "1997-2022",
       y     = "Density",
       x     = "x") +
  scale_x_continuous(limits = c(-0.5, 0.5))

dichte2
ggsave("density2.png", dichte2, width = 10, height = 8, dpi = 300)


############################COINTEGRATION#######################################

vardata1 = (cbind(GDPC,EMV,GEPUI,WUI))
colnames(vardata1) = c("RGDP","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata1,lag.max =8,type = "trend")
stargazer(VARselect(vardata1,lag.max =8,type = "trend"),title = "Information criteria VAR1 GDP", out="LagsVar1.html") 

vardata2 = (cbind(UNRATE,EMV,GEPUI,WUI))
colnames(vardata2) = c("UNRATE","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata2,lag.max =8,type = "trend")
stargazer(VARselect(vardata2,lag.max =8,type = "trend"),title = "Information criteria VAR2 UNRATE", out="LagsVar2.html") 

vardata3 = (cbind(INV,EMV,GEPUI,WUI))
colnames(vardata3) = c("INV","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata3,lag.max =8,type = "trend")
stargazer(VARselect(vardata3,lag.max =8,type = "trend"),title = "Information criteria VAR3 INV", out="LagsVar3.html") 

vardata4 = (cbind(CPI,EMV,GEPUI,WUI))
colnames(vardata4) = c("CPI","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata4,lag.max =8,type = "trend")
stargazer(VARselect(vardata4,lag.max =8,type = "trend"),title = "Information criteria VAR4 CPI", out="LagsVar4.html") 


VECM1 = VECM(vardata1,lag=0, r=3, estim = c("ML"),include = "trend") 
VECM2 = VECM(vardata2,lag=0, r=3, estim = c("ML"),include = "trend") 
VECM3 = VECM(vardata3,lag=0, r=3, estim = c("ML"),include = "trend") 
VECM4 = VECM(vardata4,lag=0, r=3, estim = c("ML"),include = "trend") 

summ = summary(rank.test(VECM1, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration VAR1: RGDP",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 

summ = summary(rank.test(VECM2, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration VAR2: UNRATE",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 

summ = summary(rank.test(VECM3, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration VAR3: INV",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 

summ = summary(rank.test(VECM4, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration VAR4: CPI",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 

############################VAR ESTIMATION######################################

vardata1 = ts(cbind(GDPCdiff,EMVdiff,GEPUIdiff,WUIdiff))
colnames(vardata1) = c("RGDP","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")

Var1 = VAR(vardata1,p = 1,type = "const")
Var2 = VAR(vardata2,p = 1,type = "const")
Var3 = VAR(vardata3,p = 1,type = "const")
Var4 = VAR(vardata4,p = 1,type = "const")

coef1 = coef(Var1)
vcov1 = vcov(Var1)
depvar1 = names(coef1)
coef2 = coef(Var2)
vcov2 = vcov(Var2)
depvar2 = names(coef2)
coef3 = coef(Var3)
vcov3 = vcov(Var3)
depvar3 = names(coef3)
coef4 = coef(Var4)
vcov4 = vcov(Var4)
depvar4 = names(coef4)
stargazer(coef1, type = "html", out = "VAR1.html",align = T,title=depvar1)
stargazer(coef2, align = T,title=depvar2, type = "html", out = "VAR2.html")
stargazer(coef3, align = T,title=depvar3, type = "html", out = "VAR3.html")
stargazer(coef4, align = T,title=depvar4, type = "html", out = "VAR4.html")

############################Residual Analysis###################################

JB1=normality.test(Var1)
Serial1=serial.test(Var1)
Homosked1= arch.test(Var1,lags.multi = 10)
Homosked1

JB2=normality.test(Var2)
Serial2=serial.test(Var2)
Homosked2 = arch.test(Var2,lags.multi = 10)
Homosked2

JB3=normality.test(Var3)
Serial3=serial.test(Var3)
Homosked3 = arch.test(Var3,lags.multi = 10)
Homosked3

JB4=normality.test(Var4)
Serial4=serial.test(Var4)
Homosked4 = arch.test(Var4,lags.multi = 10)
Homosked4

dt = data.frame(rbind(JB_Test = c("<0.01","<0.01", "<0.01","<0.01"),
                 Portmanteau_Test = c(round(Serial1$serial$p.value,4), round(Serial2$serial$p.value,4), round(Serial3$serial$p.value,4), round(Serial4$serial$p.value,4)),
                 ARCH_Test = c(round(Homosked1$arch.mul$p.value,4),round(Homosked2$arch.mul$p.value,4),round(Homosked3$arch.mul$p.value,4),round(Homosked4$arch.mul$p.value,4))))

colnames(dt) = c("VAR1","VAR2","VAR3","VAR4")


residual_table = dt %>%
  kbl(caption = "Residual Analysis", row.names = TRUE,digits = 2) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c") %>%
  kable_styling(row_label_position = "c") %>%
  row_spec(0:3, align = "c") %>%
  column_spec(1:4, width = "3cm")

############################Residual Covariances###################################

corres1=summary(Var1)$corres
corres2=summary(Var2)$corres
corres3=summary(Var3)$corres
corres4=summary(Var4)$corres

table1 = kbl(corres1, caption="Residual Covariance Matrix VAR1", booktabs = TRUE) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c")
table2 = kbl(corres2, caption="Residual Covariance Matrix VAR2", booktabs = TRUE) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c")
table3 = kbl(corres3, caption="Residual Covariance Matrix VAR3", booktabs = TRUE) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c")
table4 = kbl(corres4, caption="Residual Covariance Matrix VAR4", booktabs = TRUE) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c")

corr1 = (summary(Var1)$corres)
lmtest = (corr1[upper.tri(corr1)])^2
corr1p = 1-pchisq(sum(lmtest)*104 , df=4)
corr2 = (summary(Var2)$corres) 
lmtest = (corr2[upper.tri(corr2)])^2
corr2p = 1-pchisq(sum(lmtest)*104 , df=4)
corr3 = (summary(Var3)$corres) 
lmtest = (corr3[upper.tri(corr3)])^2
corr3p = 1-pchisq(sum(lmtest)*104 , df=4)
corr4 = (summary(Var4)$corres)
lmtest = (corr4[upper.tri(corr4)])^2
corr4p = 1-pchisq(sum(lmtest)*104 , df=4)

pval = rbind("<0.01","<0.01", "<0.01","<0.01")
rownames(pval)=c("P-Value VAR1","P-Value VAR2","P-Value VAR3","P-Value VAR4")
colnames(pval)="LM-Test"

kbl(pval, caption="LM-Test with H0=correlation is insignificant", booktabs = TRUE) %>%
  kable_classic_2(full_width = T, html_font = "Cambria", row_label_position = "c")

############################GRANGER CAUSALITY###################################

library(bruceR)
granger_causality(Var1,var.x = c("EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex"),var.y = c("RGDP"))
granger_causality(Var2,var.x = c("EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex"),var.y = c("UNRATE"))
granger_causality(Var3,var.x = c("EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex"),var.y = c("INV"))
granger_causality(Var4,var.x = c("EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex"),var.y = c("CPI"))


############################IMPULSE RESPONSE####################################

plot_irf = function(var_model, cause_var, effect_var) {
  irf = irf(var_model, response = effect_var, impulse = cause_var, boot = T,n.ahead=30,ortho=T)
  data=data.frame(cbind(data.frame(irf$irf)[effect_var],data.frame(irf$Lower)[effect_var],data.frame(irf$Upper)[effect_var]))
  irf_plot=ggplot(data = data, aes(x = 0:30, y = data[,1])) +
    geom_line() +
    geom_line(aes(y = data[,2], colour = 'red')) +
    geom_line(aes(y = data[,3]), colour = 'red')+
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste("Reaction of",
                        effect_var, "to a shock in",cause_var), x = "Time", y = "Impulse Response") +
  theme_classic()+
    guides(color = "none")
  return(irf_plot)
}

set.seed(50005)
plot3a=grid.arrange(plot_irf(Var4,"WorldUncertaintyIndex","CPI"),plot_irf(Var3,"WorldUncertaintyIndex","INV"),plot_irf(Var2,"WorldUncertaintyIndex","UNRATE"),plot_irf(Var1,"WorldUncertaintyIndex","RGDP"),ncol=2)
plot3b=grid.arrange(plot_irf(Var4,"GlobalEconomicPolicyUncertainty","CPI"),plot_irf(Var3,"GlobalEconomicPolicyUncertainty","INV"),plot_irf(Var2,"GlobalEconomicPolicyUncertainty","UNRATE"),plot_irf(Var1,"GlobalEconomicPolicyUncertainty","RGDP"),ncol=2)
plot3c=grid.arrange(plot_irf(Var4,"EquityMarketVolatility","CPI"),plot_irf(Var3,"EquityMarketVolatility","INV"),plot_irf(Var2,"EquityMarketVolatility","UNRATE"),plot_irf(Var1,"EquityMarketVolatility","RGDP"),ncol=2)

ggsave("plot3a.png", plot3a, width = 14, height = 10, dpi = 300)
ggsave("plot3b.png", plot3b, width = 14, height = 10, dpi = 300)
ggsave("plot3c.png", plot3c, width = 14, height = 10, dpi = 300)


####################VARIANCE DECOMPOSITION######################################

df_rgdp = data.frame(fevd(x=Var1,n.ahead = 20)$RGDP) %>%
  tibble::rownames_to_column(var = "Observation") %>%
  gather(key = "Variable", value = "Value", -Observation) %>%
  mutate(Variable = factor(Variable, levels = rev(unique(Variable)))) 
df_rgdp$Observation = factor(df_rgdp$Observation, levels = unique(df_rgdp$Observation))

plot4a = ggplot(df_rgdp) +
  geom_bar(aes(x=Observation,y=Value,fill=Variable),
           stat='identity')+theme_classic()+
  labs(title = "FEVD for RGDP", x = "Time", y = "Percentage")+
  scale_fill_manual(values=c("red",
                             "blue",
                             "green",
                             "orange",
                             "purple",
                             "yellow",
                             "black"))
ggsave("plot4a.png", plot4a, width = 14, height = 10, dpi = 300)

df_unrate = data.frame(fevd(x=Var2,n.ahead = 20)$UNRATE) %>%
  tibble::rownames_to_column(var = "Observation") %>%
  gather(key = "Variable", value = "Value", -Observation) %>%
  mutate(Variable = factor(Variable, levels = rev(unique(Variable)))) 
df_unrate$Observation = factor(df_unrate$Observation, levels = unique(df_unrate$Observation))

plot4b = ggplot(df_unrate) +
  geom_bar(aes(x=Observation,y=Value,fill=Variable),
           stat='identity')+theme_classic()+
  labs(title = "FEVD for UNRATE", x = "Time", y = "Percentage")+
  scale_fill_manual(values=c("red",
                             "blue",
                             "green",
                             "orange",
                             "purple",
                             "yellow",
                             "black"))
ggsave("plot4b.png", plot4b, width = 14, height = 10, dpi = 300)

df_inv = data.frame(fevd(x=Var3,n.ahead = 20)$INV) %>%
  tibble::rownames_to_column(var = "Observation") %>%
  gather(key = "Variable", value = "Value", -Observation) %>%
  mutate(Variable = factor(Variable, levels = rev(unique(Variable)))) 
df_inv$Observation = factor(df_inv$Observation, levels = unique(df_inv$Observation))

plot4c=ggplot(df_inv) +
  geom_bar(aes(x=Observation,y=Value,fill=Variable),
           stat='identity')+theme_classic()+
  labs(title = "FEVD for Investment", x = "Time", y = "Percentage")+
  scale_fill_manual(values=c("red",
                             "blue",
                             "green",
                             "orange",
                             "purple",
                             "yellow",
                             "black"))
ggsave("plot4c.png", plot4c, width = 14, height = 10, dpi = 300)

df_cpi = data.frame(fevd(x=Var4,n.ahead = 20)$CPI) %>%
  tibble::rownames_to_column(var = "Observation") %>%
  gather(key = "Variable", value = "Value", -Observation) %>%
  mutate(Variable = factor(Variable, levels = rev(unique(Variable)))) 
df_cpi$Observation = factor(df_cpi$Observation, levels = unique(df_cpi$Observation))

plot4d=ggplot(df_cpi) +
  geom_bar(aes(x=Observation,y=Value,fill=Variable),
           stat='identity')+theme_classic()+
  labs(title = "FEVD for Median CPI", x = "Time", y = "Percentage")+
  scale_fill_manual(values=c("red",
                             "blue",
                             "green",
                             "orange",
                             "purple",
                             "yellow",
                             "black"))
ggsave("plot4d.png", plot4d, width = 14, height = 10, dpi = 300)


####################FORECASTING#################################################

##################################AR Forecast##################################

#training (80%) and testing data (20%):


cv_data_ar = function(rec, names) {
  fold_size = floor(length(rec)/5)
  train_start = 1
  train_end = length(rec)
  for (i in 1:5) {
    test_start = (i-1)*fold_size + 1
    test_end = i*fold_size
    train_data = rec[-c(test_start:test_end)]
    test_data = rec[test_start:test_end]
    print(test_start)
    print(test_end) 
    name_train=paste0("train_data_",names,"_",as.character(i))
    name_test=paste0("test_data_",names,"_",as.character(i))
    assign(name_train,train_data,envir = parent.frame())
    assign(name_test,test_data,envir = parent.frame())
  }
}

cv_data_ar(INVdiff,"INV")
cv_data_ar(CPIdiff,"CPI")
cv_data_ar(UNRATEdiff,"UNRATE")
cv_data_ar(GDPCdiff,"GDP")
cv_data_ar(EMVdiff,"EMV")
cv_data_ar(GEPUIdiff,"GEPUI")
cv_data_ar(WUIdiff,"WUI")

arforecast = function(datatrain,datadifftest,mde) {
  n_steps = length(datadifftest)
  forecasts = numeric(n_steps)
  data=c(datatrain,datadifftest)
  for (i in 1:n_steps) {
    train_set = data[1:84+i-1]
    test_set = datadifftest[i]
    arima_model = arima(train_set,c(1,0,0))
    forecast = predict(arima_model, n.ahead=1)$pred
    forecasts[i] = forecast
    
  }
  name1=paste0("f_", mde, "_forecasts")
  name2=paste0("fe_", mde, "_forecast_errors")
  forecast_errors = datadifftest - forecasts
  assign(name1, forecasts,envir = parent.frame())
  assign(name2, forecast_errors,envir = parent.frame())
  
}


arforecast(train_data_INV_1,test_data_INV_1,"INV_1")
arforecast(train_data_INV_2,test_data_INV_2,"INV_2")
arforecast(train_data_INV_3,test_data_INV_3,"INV_3")
arforecast(train_data_INV_4,test_data_INV_4,"INV_4")
arforecast(train_data_INV_5,test_data_INV_5,"INV_5")

ARRMSEREC1INV = sqrt(mean(fe_INV_1_forecast_errors^2))
ARRMSEREC2INV = sqrt(mean(fe_INV_2_forecast_errors^2))
ARRMSEREC3INV = sqrt(mean(fe_INV_3_forecast_errors^2))
ARRMSEREC4INV = sqrt(mean(fe_INV_4_forecast_errors^2))
ARRMSEREC5INV = sqrt(mean(fe_INV_5_forecast_errors^2))

ARRMSEINV=mean(ARRMSEREC1INV,ARRMSEREC2INV,ARRMSEREC3INV,ARRMSEREC4INV,ARRMSEREC5INV)

arforecast(train_data_CPI_1,test_data_CPI_1,"CPI_1")
arforecast(train_data_CPI_2,test_data_CPI_2,"CPI_2")
arforecast(train_data_CPI_3,test_data_CPI_3,"CPI_3")
arforecast(train_data_CPI_4,test_data_CPI_4,"CPI_4")
arforecast(train_data_CPI_5,test_data_CPI_5,"CPI_5")

ARRMSEREC1CPI = sqrt(mean(fe_CPI_1_forecast_errors^2))
ARRMSEREC2CPI = sqrt(mean(fe_CPI_2_forecast_errors^2))
ARRMSEREC3CPI = sqrt(mean(fe_CPI_3_forecast_errors^2))
ARRMSEREC4CPI = sqrt(mean(fe_CPI_4_forecast_errors^2))
ARRMSEREC5CPI = sqrt(mean(fe_CPI_5_forecast_errors^2))

ARRMSECPI=mean(ARRMSEREC1CPI,ARRMSEREC2CPI,ARRMSEREC3CPI,ARRMSEREC4CPI,ARRMSEREC5CPI)

arforecast(train_data_UNRATE_1,test_data_UNRATE_1,"UNRATE_1")
arforecast(train_data_UNRATE_2,test_data_UNRATE_2,"UNRATE_2")
arforecast(train_data_UNRATE_3,test_data_UNRATE_3,"UNRATE_3")
arforecast(train_data_UNRATE_4,test_data_UNRATE_4,"UNRATE_4")
arforecast(train_data_UNRATE_5,test_data_UNRATE_5,"UNRATE_5")

ARRMSEREC1UNRATE = sqrt(mean(fe_UNRATE_1_forecast_errors^2))
ARRMSEREC2UNRATE = sqrt(mean(fe_UNRATE_2_forecast_errors^2))
ARRMSEREC3UNRATE = sqrt(mean(fe_UNRATE_3_forecast_errors^2))
ARRMSEREC4UNRATE = sqrt(mean(fe_UNRATE_4_forecast_errors^2))
ARRMSEREC5UNRATE = sqrt(mean(fe_UNRATE_5_forecast_errors^2))

ARRMSEUNRATE=mean(ARRMSEREC1UNRATE,ARRMSEREC2UNRATE,ARRMSEREC3UNRATE,ARRMSEREC4UNRATE,ARRMSEREC5UNRATE)

arforecast(train_data_GDP_1,test_data_GDP_1,"GDP_1")
arforecast(train_data_GDP_2,test_data_GDP_2,"GDP_2")
arforecast(train_data_GDP_3,test_data_GDP_3,"GDP_3")
arforecast(train_data_GDP_4,test_data_GDP_4,"GDP_4")
arforecast(train_data_GDP_5,test_data_GDP_5,"GDP_5")

ARRMSEREC1GDP = sqrt(mean(fe_GDP_1_forecast_errors^2))
ARRMSEREC2GDP = sqrt(mean(fe_GDP_2_forecast_errors^2))
ARRMSEREC3GDP = sqrt(mean(fe_GDP_3_forecast_errors^2))
ARRMSEREC4GDP = sqrt(mean(fe_GDP_4_forecast_errors^2))
ARRMSEREC5GDP = sqrt(mean(fe_GDP_5_forecast_errors^2))

ARRMSEGDP=mean(ARRMSEREC1GDP,ARRMSEREC2GDP,ARRMSEREC3GDP,ARRMSEREC4GDP,ARRMSEREC5GDP)


plot_ar = function(data,datadifftrain,fore,name) {
  forecast_df = data.frame(cbind(data, c(datadifftrain ,fore)))
  colnames(forecast_df)=c("actual","forecast")
  ggplot(forecast_df, aes(x = Date[1:length(data)])) +
    geom_line(aes(y = forecast, color = "Forecast")) +
    geom_line(aes(y = actual, color = "Real")) +
    labs(title = "ARIMA Forecasts vs Real Values",
         x = "Time",
         y = "INVdiff") +
    scale_color_manual(values = c("Forecast" = "blue", "Real" = "red")) +
    xlim((Datediff)[78], (Datediff)[103])+
    labs(title = paste("Forecast vs Real values of",
                       name), x = "Time", y = "Index") +
    theme_classic()
}

##################################VAR Forecast##################################

vardata1 = (cbind(GDPCdiff,EMVdiff,GEPUIdiff,WUIdiff))
colnames(vardata1) = c("RGDP","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata1,lag.max =8,type = "trend")
stargazer(VARselect(vardata1,lag.max =8,type = "trend"),title = "Information criteria GDP", out="Lags.html") 

vardata2 = (cbind(UNRATEdiff,EMVdiff,GEPUIdiff,WUIdiff))
colnames(vardata2) = c("UNRATE","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata2,lag.max =8,type = "trend")
stargazer(VARselect(vardata2,lag.max =8,type = "trend"),title = "Information criteria UNRATE", out="Lags.html") 

vardata3 = (cbind(INVdiff,EMVdiff,GEPUIdiff,WUIdiff))
colnames(vardata3) = c("INV","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata3,lag.max =8,type = "trend")
stargazer(VARselect(vardata3,lag.max =8,type = "trend"),title = "Information criteria INV", out="Lags.html") 

vardata4 = (cbind(CPIdiff,EMVdiff,GEPUIdiff,WUIdiff))
colnames(vardata4) = c("CPI","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
VARselect(vardata4,lag.max =8,type = "trend")
stargazer(VARselect(vardata4,lag.max =8,type = "trend"),title = "Information criteria CPI", out="Lags.html") 

cv_data_var = function(vardata1, names) {
  fold_size = floor(length(vardata1[,1])/5)
  train_start = 1
  train_end = length(vardata1[,1])
  for (i in 1:5) {
    test_start = (i-1)*fold_size + 1
    test_end = i*fold_size
    train_data = vardata1[-c(test_start:test_end),]
    test_data = vardata1[test_start:test_end,]
    print(test_start)
    print(test_end) 
    name_train=paste0("train_data_v_",names,"_",as.character(i))
    name_test=paste0("test_data_v_",names,"_",as.character(i))
    assign(name_train,train_data,envir = parent.frame())
    assign(name_test,test_data,envir = parent.frame())
    
  }
}

cv_data_var(vardata1,"RGDP")
cv_data_var(vardata2,"UNRATE")
cv_data_var(vardata3,"INV")
cv_data_var(vardata4,"CPI")


varforecast = function(datatrain,datadifftest,mde,namefe) {
  n_steps = length(datadifftest[,1])
  forecasts_v = numeric(n_steps)
  data=data.frame(rbind(datatrain,datadifftest))
  for (i in 1:n_steps) {
    train_set = data[1:84+i-1,]
    test_set = datadifftest[i,]
    var_model = VAR(train_set,ic="AIC",type="const")
    forecast_v = predict(var_model,n.ahead=1,se.fit=FALSE)
    forecasts_v[i] = forecast_v$fcst[[mde]][1]
    
  }
  name1=paste0("f_", mde, "_forecasts_v",namefe)
  name2=paste0("fe_", mde, "_forecast_errors_v",namefe)
  forecast_errors_v = datadifftest[,1] - forecasts_v
  assign(name1, forecasts_v,envir = parent.frame())
  assign(name2, forecast_errors_v,envir = parent.frame())
}

varforecast_gdp = function(datatrain,datadifftest,mde,namefe) {
  n_steps = length(datadifftest[,1])
  forecasts_v = numeric(n_steps)
  data=data.frame(rbind(datatrain,datadifftest))
  for (i in 1:n_steps) {
    train_set = data[1:84+i-1,]
    test_set = datadifftest[i,]
    var_model = VAR(train_set,ic="AIC",type="trend")
    forecast_v = predict(var_model,n.ahead=1,se.fit=FALSE)
    forecasts_v[i] = forecast_v$fcst[[mde]][1]
    
  }
  name1=paste0("f_", mde, "_forecasts_v",namefe)
  name2=paste0("fe_", mde, "_forecast_errors_v",namefe)
  forecast_errors_v = datadifftest[,1] - forecasts_v
  assign(name1, forecasts_v,envir = parent.frame())
  assign(name2, forecast_errors_v,envir = parent.frame())
}
varforecast_gdp(train_data_v_RGDP_1,test_data_v_RGDP_1,"RGDP","K1")
varforecast_gdp(train_data_v_RGDP_2,test_data_v_RGDP_2,"RGDP","K2")
varforecast_gdp(train_data_v_RGDP_3,test_data_v_RGDP_3,"RGDP","K3")
varforecast_gdp(train_data_v_RGDP_4,test_data_v_RGDP_4,"RGDP","K4")
varforecast_gdp(train_data_v_RGDP_5,test_data_v_RGDP_5,"RGDP","K5")

VARRMSEREC1GDP = sqrt(mean(fe_RGDP_forecast_errors_vK1^2))
VARRMSEREC2GDP = sqrt(mean(fe_RGDP_forecast_errors_vK2^2))
VARRMSEREC3GDP = sqrt(mean(fe_RGDP_forecast_errors_vK3^2))
VARRMSEREC4GDP = sqrt(mean(fe_RGDP_forecast_errors_vK4^2))
VARRMSEREC5GDP = sqrt(mean(fe_RGDP_forecast_errors_vK5^2))

VARRMSEGDP=mean(VARRMSEREC1GDP,VARRMSEREC2GDP,VARRMSEREC3GDP,VARRMSEREC4GDP,VARRMSEREC5GDP)

TheilsUGDP = VARRMSEGDP/ARRMSEGDP

varforecast(train_data_v_UNRATE_1,test_data_v_UNRATE_1,"UNRATE","K1")
varforecast(train_data_v_UNRATE_2,test_data_v_UNRATE_2,"UNRATE","K2")
varforecast(train_data_v_UNRATE_3,test_data_v_UNRATE_3,"UNRATE","K3")
varforecast(train_data_v_UNRATE_4,test_data_v_UNRATE_4,"UNRATE","K4")
varforecast(train_data_v_UNRATE_5,test_data_v_UNRATE_5,"UNRATE","K5")

VARRMSEREC1UNRATE = sqrt(mean(fe_UNRATE_forecast_errors_vK1^2))
VARRMSEREC2UNRATE = sqrt(mean(fe_UNRATE_forecast_errors_vK2^2))
VARRMSEREC3UNRATE = sqrt(mean(fe_UNRATE_forecast_errors_vK3^2))
VARRMSEREC4UNRATE = sqrt(mean(fe_UNRATE_forecast_errors_vK4^2))
VARRMSEREC5UNRATE = sqrt(mean(fe_UNRATE_forecast_errors_vK5^2))

VARRMSEUNRATE=mean(VARRMSEREC1UNRATE,VARRMSEREC2UNRATE,VARRMSEREC3UNRATE,VARRMSEREC4UNRATE,VARRMSEREC5UNRATE)

TheilsUUNRATE = VARRMSEUNRATE/ARRMSEUNRATE

varforecast(train_data_v_INV_1,test_data_v_INV_1,"INV","K1")
varforecast(train_data_v_INV_2,test_data_v_INV_2,"INV","K2")
varforecast(train_data_v_INV_3,test_data_v_INV_3,"INV","K3")
varforecast(train_data_v_INV_4,test_data_v_INV_4,"INV","K4")
varforecast(train_data_v_INV_5,test_data_v_INV_5,"INV","K5")

VARRMSEREC1INV = sqrt(mean(fe_INV_forecast_errors_vK1^2))
VARRMSEREC2INV = sqrt(mean(fe_INV_forecast_errors_vK2^2))
VARRMSEREC3INV = sqrt(mean(fe_INV_forecast_errors_vK3^2))
VARRMSEREC4INV = sqrt(mean(fe_INV_forecast_errors_vK4^2))
VARRMSEREC5INV = sqrt(mean(fe_INV_forecast_errors_vK5^2))

VARRMSEINV=mean(VARRMSEREC1INV,VARRMSEREC2INV,VARRMSEREC3INV,VARRMSEREC4INV,VARRMSEREC5INV)

TheilsUINV = VARRMSEINV/ARRMSEINV

varforecast(train_data_v_CPI_1,test_data_v_CPI_1,"CPI","K1")
varforecast(train_data_v_CPI_2,test_data_v_CPI_2,"CPI","K2")
varforecast(train_data_v_CPI_3,test_data_v_CPI_3,"CPI","K3")
varforecast(train_data_v_CPI_4,test_data_v_CPI_4,"CPI","K4")
varforecast(train_data_v_CPI_5,test_data_v_CPI_5,"CPI","K5")

VARRMSEREC1CPI = sqrt(mean(fe_CPI_forecast_errors_vK1^2))
VARRMSEREC2CPI = sqrt(mean(fe_CPI_forecast_errors_vK2^2))
VARRMSEREC3CPI = sqrt(mean(fe_CPI_forecast_errors_vK3^2))
VARRMSEREC4CPI = sqrt(mean(fe_CPI_forecast_errors_vK4^2))
VARRMSEREC5CPI = sqrt(mean(fe_CPI_forecast_errors_vK5^2))

VARRMSECPI=mean(VARRMSEREC1CPI,VARRMSEREC2CPI,VARRMSEREC3CPI,VARRMSEREC4CPI,VARRMSEREC5CPI)

TheilsUCPI = VARRMSECPI/ARRMSECPI


TheilsU_table = data.frame(
  Variable = c("INV", "CPI", "UNRATE", "RGDP"),
  Theils_U = c(TheilsUINV, TheilsUCPI, TheilsUUNRATE, TheilsUGDP)
)

kable(TheilsU_table, align = c("l", "r"), col.names = c("Variable", "Theil's U")) %>% 
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 


##################################Forecast Evaluation############################

library(readxl)
fore = read_excel("Median_RGDP_Growth.xlsx",col_names = T)
fore=data.frame(fore[115:214,c(1,2,3)])

fore1 = read_excel("meanlevel.xlsx",col_names = T,sheet = "UNEMP")
fore1=fore1[114:214,]

RGDPactual=(datadiff$RGDP[1:100])*100
RGDPactual=RGDPactual*4
RGDPforecasts=c(f_RGDP_forecasts_vK1,f_RGDP_forecasts_vK2,f_RGDP_forecasts_vK3,f_RGDP_forecasts_vK4,f_RGDP_forecasts_vK5)*100
RGDPforecasts=RGDPforecasts*4
INVactual=datadiff$INV[1:100]
INVforecasts=c(f_INV_forecasts_vK1,f_INV_forecasts_vK2,f_INV_forecasts_vK3,f_INV_forecasts_vK4,f_INV_forecasts_vK5)
CPIactual=datadiff$CPI[1:100]
CPIforecasts=c(f_CPI_forecasts_vK1,f_CPI_forecasts_vK2,f_CPI_forecasts_vK3,f_CPI_forecasts_vK4,f_CPI_forecasts_vK5)
UNRATEactual=(datadiff$UNRATE[1:100])*100
UNRATEforecasts=c(f_UNRATE_forecasts_vK1,f_UNRATE_forecasts_vK2,f_UNRATE_forecasts_vK3,f_UNRATE_forecasts_vK4,f_UNRATE_forecasts_vK5)*100

summary(lm(RGDPactual~RGDPforecasts)) #quality of forecasts
summary(lm(INVactual~INVforecasts)) #quality of forecasts
summary(lm(CPIactual~CPIforecasts)) #quality of forecasts
summary(lm(UNRATEactual~UNRATEforecasts)) #quality of forecasts

model1 = lm(RGDPactual ~ RGDPforecasts)
model2 = lm(INVactual ~ INVforecasts)
model3 = lm(CPIactual ~ CPIforecasts)
model4 = lm(UNRATEactual ~ UNRATEforecasts)

stargazer(model1, model2, model3, model4, 
          title = "Regression Results for Forecast Quality",
          align = TRUE, type = "text",out = "Forecastquality.html")




plot_var=function(dataactual,dataforecast,name){
  forgraph=data.frame(cbind(as.Date(Datediff[1:100]),dataactual,dataforecast))
  forgraph[,1]=Datediff[1:100]
  ggplot(forgraph,aes(x = forgraph[,1])) +
    geom_line(aes(y = forgraph[,3], color = "Forecast")) +
    geom_line(aes(y = forgraph[,2], color = "Real")) +
    labs(title = "VAR Forecasts vs Real Values",
         x = "Time",
         y = "INVdiff") +
    scale_color_manual(values = c("Forecast" = "blue", "Real" = "red")) +
    labs(title = paste("Forecast vs Real values of",name), x = "Time", y = "Index") +
    theme_classic()
}


plot6=grid.arrange(plot_var(RGDPactual,RGDPforecasts,"RGDP"),plot_var(INVactual,INVforecasts,"Investments"),plot_var(UNRATEactual,UNRATEforecasts,"Unemployment Rate"),plot_var(CPIactual,CPIforecasts,"Median CPI"),ncol=2,nrow=2)
ggsave("plot6.png", plot6, width = 14, height = 12, dpi = 400)

##################################Claric West Test##############################

Var_RGDPfe = c(fe_RGDP_forecast_errors_vK1,fe_RGDP_forecast_errors_vK2,fe_RGDP_forecast_errors_vK3,fe_RGDP_forecast_errors_vK4,fe_RGDP_forecast_errors_vK5)
Ar_RGDPfe = c(fe_GDP_1_forecast_errors,fe_GDP_2_forecast_errors,fe_GDP_3_forecast_errors,fe_GDP_4_forecast_errors,fe_GDP_5_forecast_errors)

Var_INVfe=c(fe_INV_forecast_errors_vK1,fe_INV_forecast_errors_vK2,fe_INV_forecast_errors_vK3,fe_INV_forecast_errors_vK4,fe_INV_forecast_errors_vK5)
Ar_INVfe=c(fe_INV_1_forecast_errors,fe_INV_2_forecast_errors,fe_INV_3_forecast_errors,fe_INV_4_forecast_errors,fe_INV_5_forecast_errors)

Var_CPIfe=c(fe_CPI_forecast_errors_vK1,fe_CPI_forecast_errors_vK2,fe_CPI_forecast_errors_vK3,fe_CPI_forecast_errors_vK4,fe_CPI_forecast_errors_vK5)
Ar_CPIfe=c(fe_CPI_1_forecast_errors,fe_CPI_2_forecast_errors,fe_CPI_3_forecast_errors,fe_CPI_4_forecast_errors,fe_CPI_5_forecast_errors)

Var_UNRATEfe=c(fe_UNRATE_forecast_errors_vK1,fe_UNRATE_forecast_errors_vK2,fe_UNRATE_forecast_errors_vK3,fe_UNRATE_forecast_errors_vK4,fe_UNRATE_forecast_errors_vK5)
Ar_UNRATEfe=c(fe_UNRATE_1_forecast_errors,fe_UNRATE_2_forecast_errors,fe_UNRATE_3_forecast_errors,fe_UNRATE_4_forecast_errors,fe_UNRATE_5_forecast_errors)

CW_RGDP=Ar_RGDPfe^2-Var_RGDPfe^2+(Ar_RGDPfe-Var_RGDPfe)^2
CW_INV=Ar_INVfe^2-Var_INVfe^2+(Ar_INVfe-Var_INVfe)^2
CW_CPI=Ar_CPIfe^2-Var_CPIfe^2+(Ar_CPIfe-Var_CPIfe)^2
CW_UNRATE=Ar_UNRATEfe^2-Var_UNRATEfe^2+(Ar_UNRATEfe-Var_UNRATEfe)^2

reg1 = lm(CW_RGDP ~ 1)
reg2 = lm(CW_INV ~ 1)
reg3 = lm(CW_CPI ~ 1)
reg4 = lm(CW_UNRATE ~ 1)

stargazer(reg1, reg2, reg3, reg4, type = "text",title = "Results of the Clark-West test", column.labels = c("CW_RGDP", "CW_INV", "CW_CPI", "CW_UNRATE"), covariate.labels = NULL,header = FALSE, notes = ("One sided t-test with 99 degrees of freedom, critical value at 10% significance = 1.290"), align = TRUE, digits = 3,out="CW.html", add.lines = 
            list(c("Test Statistic", 
              round(summary(reg1)$coefficients[3],3), 
              round(summary(reg2)$coefficients[3],3),
              round(summary(reg3)$coefficients[3],3),
              round(summary(reg4)$coefficients[3],3)
            ) ))

###############################Forecast Encompassing############################

foreUNRATE=(diff(log(fore1$UNEMP2)))*100
reg1=(lm(RGDPactual~fore$DRGDP2+RGDPforecasts-1)) 
names(reg1$coefficients)=c("SPF-Forecasts","VAR-Forecasts")
reg2=(lm(UNRATEactual~foreUNRATE+UNRATEforecasts-1)) 
names(reg2$coefficients)=c("SPF-Forecasts","VAR-Forecasts")

stargazer(reg1, reg2, 
          type = "text", out="Encompassing.html", column.sep.width = "50pt",
          title = "Forecast-encompassing test SPF-Forecasts vs. VAR-Forecasts")

fore_new=reg1$coefficients[1]*fore$DRGDP2+reg1$coefficients[2]*RGDPforecasts
sqrt(mean((fore_new-RGDPactual)^2))
error_new=fore_new-RGDPactual
DMW1=Var_RGDPfe^2-error_new^2
regDMW1=(lm(DMW1~1))
error_SPF=RGDPforecasts-RGDPactual
DMW2= error_SPF^2-error_new^2
regDMW2=(lm(DMW2~1))

stargazer(regDMW1, regDMW2, type = "text",title = "Results of the Diebold-Mariano-West test", column.labels = c("CW_RGDP", "CW_INV", "CW_CPI", "CW_UNRATE"), covariate.labels = NULL,header = FALSE, notes = ("One sided t-test with 99 degrees of freedom, critical value at 10% significance = 1.290"), align = TRUE, digits = 3,out="DMW.html", add.lines = 
            list(c("Test Statistic", 
                   round(summary(regDMW1)$coefficients[3],3), 
                   round(summary(regDMW2)$coefficients[3],3)) ))
##################################Recession Forecast############################

getSymbols("JHGDPBRINDX",src="FRED")
rec = ts(JHGDPBRINDX, start = c(1967, 4),end = c(2022,4), frequency = 4)
rec=window(rec,start=c(1997,1),end=c(2022,4))
kpss.test(rec)
vardata1 = (cbind(rec,EMV,GEPUI,WUI))
colnames(vardata1) = c("Recession","EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex")
vardata1 = na.omit(vardata1)
VECM1 = VECM(vardata1,lag=1, r=3, estim = c("ML"),include = "trend") 
summ = summary(rank.test(VECM1, type = c("eigen", "trace"), cval = 0.05))
rang= c(summ$r)
trace = c(summ$trace_pval)
eigen = c(summ$eigen_pval)
dt = data.frame(cbind(rang,trace,eigen))
dt %>%
  kbl(caption = "Test for Cointegration RGDP",col.names = c("Rank","P-Value Trace","P-Value Eigenvalue")) %>%
  kable_classic_2(full_width =T , html_font = "Cambria",row_label_position = "c") %>% kable_styling(row_label_position = "c")%>% 
  row_spec(0:4, align = "c") 
var_model = VAR(vardata1,ic="SC",type="const",lag.max=6)
plot_irf(var_model,"EquityMarketVolatility","Recession")
plot_irf(var_model,"GlobalEconomicPolicyUncertainty","Recession")
plot_irf(var_model,"WorldUncertaintyIndex","Recession")
granger_causality(var_model,var.y = "Recession",var.x =c("EquityMarketVolatility","GlobalEconomicPolicyUncertainty","WorldUncertaintyIndex") )


fold_size = floor(length(rec)/5)
train_start = 1
train_end = length(rec)

for (i in 1:5) {
  test_start = (i-1)*fold_size + 1
  test_end = i*fold_size
  train_data = rec[-c(test_start:test_end)]
  test_data = rec[test_start:test_end]
  print(test_start)
  print(test_end) 
  name_train=paste0("train_data_",as.character(i))
  name_test=paste0("test_data_",as.character(i))
  assign(name_train,train_data,envir = parent.frame())
  assign(name_test,test_data,envir = parent.frame())
  
}

arforecast = function(datatrain,datadifftest,mde) {
  n_steps = length(datadifftest)
  forecasts = numeric(n_steps)
  data=c(datatrain,datadifftest)
  for (i in 1:n_steps) {
    train_set = data[1:84+i-1]
    test_set = datadifftest[i]
    arima_model = arima(train_set,c(1,0,0))
    forecast = predict(arima_model, n.ahead=1)$pred
    forecasts[i] = forecast
    
  }
  name1=paste0("f_", mde, "_forecasts")
  name2=paste0("fe_", mde, "_forecast_errors")
  forecast_errors = datadifftest - forecasts
  assign(name1, forecasts,envir = parent.frame())
  assign(name2, forecast_errors,envir = parent.frame())
  
}


fold_size = floor(length(vardata1[,1])/5)
train_start = 1
train_end = length(vardata1[,1])

for (i in 1:5) {
  test_start = (i-1)*fold_size + 1
  test_end = i*fold_size
  train_data = vardata1[-c(test_start:test_end),]
  test_data = vardata1[test_start:test_end,]
  print(test_start)
  print(test_end) 
  name_train=paste0("train_data_v_",as.character(i))
  name_test=paste0("test_data_v_",as.character(i))
  assign(name_train,train_data,envir = parent.frame())
  assign(name_test,test_data,envir = parent.frame())
  
}

varforecast = function(datatrain,datadifftest,mde,namefe) {
  n_steps = length(datadifftest[,1])
  forecasts_v = numeric(n_steps)
  data=data.frame(rbind(datatrain,datadifftest))
  for (i in 1:n_steps) {
    train_set = data[1:84+i-1,]
    test_set = datadifftest[i,]
    var_model = VAR(train_set,ic="AIC",type="const",lag.max=6)
    forecast_v = predict(var_model,n.ahead=1,se.fit=FALSE)
    forecasts_v[i] = forecast_v$fcst[[mde]][1]
    
  }
  name1=paste0("f_", mde, "_forecasts_v",namefe)
  name2=paste0("fe_", mde, "_forecast_errors_v",namefe)
  forecast_errors_v = datadifftest[,1] - forecasts_v
  assign(name1, forecasts_v,envir = parent.frame())
  assign(name2, forecast_errors_v,envir = parent.frame())
}

arforecast(train_data_1,test_data_1,"rec_1")
arforecast(train_data_2,test_data_2,"rec_2")
arforecast(train_data_3,test_data_3,"rec_3")
arforecast(train_data_4,test_data_4,"rec_4")
arforecast(train_data_5,test_data_5,"rec_5")

ARRMSEREC1 = sqrt(mean(fe_rec_1_forecast_errors^2))
ARRMSEREC2 = sqrt(mean(fe_rec_2_forecast_errors^2))
ARRMSEREC3 = sqrt(mean(fe_rec_3_forecast_errors^2))
ARRMSEREC4 = sqrt(mean(fe_rec_4_forecast_errors^2))
ARRMSEREC5 = sqrt(mean(fe_rec_5_forecast_errors^2))

ARRMSEREC=mean(ARRMSEREC1,ARRMSEREC2,ARRMSEREC3,ARRMSEREC4,ARRMSEREC5)

varforecast(train_data_v_1,test_data_v_1,"Recession","K1")
varforecast(train_data_v_2,test_data_v_2,"Recession","K2")
varforecast(train_data_v_3,test_data_v_3,"Recession","K3")
varforecast(train_data_v_4,test_data_v_4,"Recession","K4")
varforecast(train_data_v_5,test_data_v_5,"Recession","K5")


VARRMSEREC1 = sqrt(mean(fe_Recession_forecast_errors_vK1^2))
VARRMSEREC2 = sqrt(mean(fe_Recession_forecast_errors_vK2^2))
VARRMSEREC3 = sqrt(mean(fe_Recession_forecast_errors_vK3^2))
VARRMSEREC4 = sqrt(mean(fe_Recession_forecast_errors_vK4^2))
VARRMSEREC5 = sqrt(mean(fe_Recession_forecast_errors_vK5^2))


VARRMSEREC=mean(VARRMSEREC1,VARRMSEREC2,VARRMSEREC3,VARRMSEREC4,VARRMSEREC5)

TheilsUREC = VARRMSEREC/ARRMSEREC

#Evaluation:
summary(lm(rec[1:100]~c(f_Recession_forecasts_vK1,f_Recession_forecasts_vK2,f_Recession_forecasts_vK3,f_Recession_forecasts_vK4,f_Recession_forecasts_vK5)))
  