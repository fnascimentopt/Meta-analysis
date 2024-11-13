library(metafor)

#Organização da coluna de parametros(grupos) e cálculo

dados <- Dados_filtrados[order(Dados_filtrados$Parameter), ]

dados1<-escalc(measure="SMD",m1i=caso,m2i=controle,n1i=n_caso,n2i=n_control,sd1i=dp...5,sd2i=dp...9,data=dados,vtype="UB",append=TRUE)
?escalc

efe.ale<-rma(yi,vi,method="DL",data=dados1)
summary(efe.ale)
efe.ale

####

#Gráfico  
forest(efe.ale, mlab="", slab=paste(dados1$Artigos), showweights = TRUE,
       cex=0.85,xlim=c(-25,45))


par(mar=c(4,4,2,5))
forest(efe.ale, mlab="", slab=paste(dados1$Artigos), showweights = TRUE,
       rows=c(3:16,20:22,26:38,42:55), cex=0.65, ylim=c(-3,56), xlim=c(-25,45)) 
op2<-par(cex=0.75, font=4)
text(35,c(56), "Weight and Effect [95% CI]")
text(-20,c(56), "Author, Date")
par(op2)


#calculo dos resultados separados
res_di <- rma(yi,vi, method="DL", data = dados1, subset = (Parameter=="DI"))
res_fdna <- rma(yi,vi, method="DL", data = dados1, subset = (Parameter=="fDNA"))
res_otm <- rma(yi,vi, method="DL", data = dados1, subset = (Parameter=="OTM"))
res_tl <- rma(yi,vi, method="DL", data = dados1, subset = (Parameter=="TL"))

op1<-par(cex=0.75, font=4)

# adicionar poligono no forest
par()
addpoly(res_di,cex=0.75, row=2,  mlab = "")
addpoly(res_fdna,cex=0.75, row=19, mlab ="")
addpoly(res_otm,cex=0.75, row=25, mlab = "")
addpoly (res_tl,cex=0.75, row=41, mlab = "")




#inserir texto e resultados dos subgrupos
text(-25, -1, pos=4, cex=0.70, bquote(paste("RE Model for all Studies (Q = ",
                                            .(formatC(efe.ale$QE, digits=2, format="f")), ", df = ", .(efe.ale$k - efe.ale$p),
                                            ", p = ", .(formatC(efe.ale$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(efe.ale$I2, digits=1, format="f")), "%)")))

#subgrupos
text(-25, 2, pos=4, cex=0.70, bquote(paste("Damage Index (Q = ",
                                           .(formatC(res_di$QE, digits=2, format="f")), ", df = ", .(res_di$k - res_di$p),
                                           ", p = ", .(formatC(res_di$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                           .(formatC(res_di$I2, digits=1, format="f")), "%)")))
text(-25, 19, pos=4, cex=0.70, bquote(paste("DNA Frequency (Q = ",
                                            .(formatC(res_fdna$QE, digits=2, format="f")), ", df = ", .(res_fdna$k - res_fdna$p),
                                            ", p = ", .(formatC(res_fdna$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_fdna$I2, digits=1, format="f")), "%)")))
text(-25, 25, pos=4, cex=0.70, bquote(paste("Olive Tail Moment (Q = ",
                                            .(formatC(res_otm$QE, digits=2, format="f")), ", df = ", .(res_otm$k - res_otm$p),
                                            ", p = ", .(formatC(res_otm$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_otm$I2, digits=1, format="f")), "%)")))
text(-25, 41, pos=4, cex=0.70, bquote(paste("Tail Lenght (Q = ",
                                            .(formatC(res_tl$QE, digits=2, format="f")), ", df = ", .(res_tl$k - res_tl$p),
                                            ", p = ", .(formatC(res_tl$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res_tl$I2, digits=1, format="f")), "%)")))



#teste de assimetria do funnel
regtest(efe.ale)
ranktest(efe.ale)



#meta-regressão

res<-rma(yi, vi, mods = ~ factor(Parameter) - 1,method = "DL", data = dados1)
res

##### funil com a linha de regressão
funnel(efe.ale)
reg <- regtest(efe.ale)
se <- seq(0,1.5,length=100)
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se, se)
reg <- regtest(efe.ale, predictor="vi")
lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se^2, se)
reg

#teste de sensibilidade
leave1out(efe.ale)

######### com ordem 
dados.tes <- dados1[order(dados1$yi), ]
efe.tes<-rma(yi,vi,method="DL",data=dados.tes)
forest(efe.tes)
par(mar=c(4,4,2,5))
forest(efe.tes,  mlab="", slab=paste(dados.tes$Artigos), showweights = TRUE,
       cex=0.85,xlim=c(-25,45))

op2<-par(cex=0.75, font=4)
text(35,c(45), "Weight and Effect [95% CI]")
text(-20,c(45), "Author, Date")
par(op2)

text(-25, -2, pos=4, cex=0.75, bquote(paste("RE Model for all Studies (Q = ",
                                            .(formatC(efe.tes$QE, digits=2, format="f")), ", df = ", .(efe.tes$k - efe.tes$p),
                                            ", p = ", .(formatC(efe.tes$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(efe.tes$I2, digits=1, format="f")), "%)")))


# Análise entre caso controle de fumantes
dadossmok <- escalc(measure="SMD",m1i=Dados_filtrados$`M smok...13`,m2i=Dados_filtrados$`M smok...21`,n1i=Dados_filtrados$`N smokers...11`
                    ,n2i=Dados_filtrados$`N smokers...19`,sd1i=Dados_filtrados$`SD smok...15`,sd2i=Dados_filtrados$`SD smok...23`,data=Dados_filtrados,vtype="UB",append=TRUE)


efe.smok<- rma(yi,vi,method="DL",data=dadossmok)
efe.smok
par(mar=c(4,4,2,5))
forest(efe.smok, slab=paste(dadossmok$Artigos), showweights = TRUE, mlab="", cex=1)
op2<-par(cex=1, font=4)
text(30,c(15), "Weight and Effect [95% CI]")
text(-25,c(15), "Author, Date")
par(op2)
text(-32, -1, pos=4, cex=0.75, bquote(paste("RE Model for all Studies (Q = ",
                                            .(formatC(efe.smok$QE, digits=2, format="f")), ", df = ", .(efe.smok$k - efe.smok$p),
                                            ", p = ", .(formatC(efe.smok$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(efe.smok$I2, digits=1, format="f")), "%)")))

funnel(efe.smok)

#Análise caso controle entre nao fumantes
dadosnonsmok<-escalc(measure="SMD",m1i=Dados_filtrados$`M non sm...14`,m2i=Dados_filtrados$`M non sm...22`,n1i=Dados_filtrados$`N non-smokers...12`
                     ,n2i=Dados_filtrados$`N non-smokers...20`,sd1i=Dados_filtrados$`SD non sm...16`,sd2i=Dados_filtrados$`SD non sm...24`,data=Dados_filtrados,vtype="UB",append=TRUE)

efe.non.smok<-rma(yi,vi,method="DL",data=dadosnonsmok)
efe.non.smok
forest(efe.non.smok, slab=paste(dadosnonsmok$Artigos), showweights = TRUE, mlab="", cex=1)
op2<-par(cex=1, font=4)
text(50,c(19), "Weight and Effect [95% CI]")
text(-45,c(19), "Author, Date")
par(op2)
text(-53, -1, pos=4, cex=0.75, bquote(paste("RE Model for all Studies (Q = ",
                                            .(formatC(efe.non.smok$QE, digits=2, format="f")), ", df = ", .(efe.non.smok$k - efe.non.smok$p),
                                            ", p = ", .(formatC(efe.non.smok$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(efe.non.smok$I2, digits=1, format="f")), "%)")))

#Análise de caso entre fumantes e não fumantes
dadoscomp<- escalc(measure="SMD",m1i=Dados_filtrados$`M smok...13`,m2i=Dados_filtrados$`M non sm...14`,n1i=Dados_filtrados$`N smokers...11`
,n2i=Dados_filtrados$`N non-smokers...12`,sd1i=Dados_filtrados$`SD smok...15`,sd2i=Dados_filtrados$`SD non sm...16`,data=Dados_filtrados,vtype="UB",append=TRUE)
efe.com.exp<-rma(yi,vi,method="DL",data=dadoscomp)
efe.com.exp
forest(efe.com.exp, slab=paste(dadoscomp$Artigos), showweights = TRUE, mlab="", cex=1)
op2<-par(cex=1, font=4)
text(6,c(14), "Weight and Effect [95% CI]")
text(-6,c(14), "Author, Date")
par(op2)
text(-7, -1, pos=4, cex=0.75, bquote(paste("RE Model for all Studies (Q = ",
                                            .(formatC(efe.com.exp$QE, digits=2, format="f")), ", df = ", .(efe.com.exp$k - efe.com.exp$p),
                                            ", p = ", .(formatC(efe.com.exp$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(efe.com.exp$I2, digits=1, format="f")), "%)")))

funnel(efe.com.exp)
