#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%% Codigo da analise dos dados da Seazone sobre ocupação e   %%
#%% preço de anúncios no Airbnb.                              %%
#%% Autora e candidata: Sheyla Maria Tavares e Tavares        %%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                                           %%
#%%     DEFINICOES INICIAIS E SINTAXES INTERMEDIARIAS         %%
#%%                                                           %%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
rm(list=ls(all=T))  # remover objetos anteriores
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Carregar_Pacotes=function(){
  if(!require(formattable)){install.packages("formattable"); library(formattable)}
  if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
  if(!require(tidyverse)){install.packages("tidyverse"); library(tidyverse)}
  if(!require(lubridate)){install.packages("lubridate"); library(lubridate)}
}
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%                                                           %%
#%%               PARTE PRINCIPAL DA SINTAXE                  %%
#%%                                                           %%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Carregar_Pacotes()

#*********************  DADOS PARA ANALISE *********************

price=read.csv2(file = "desafio_priceav.csv",header = T,sep = ",",encoding = "UTF-8"); price=price[,-2]
details=read.csv2(file = "desafio_details.csv",header = T,sep = ",",encoding = "UTF-8")

dados=merge(price,details,by.x = "airbnb_listing_id",by.y = "airbnb_listing_id",all = T)# mesclar data frames
dados[,4]=as.Date(dados[,4]);dados[,5]=as.numeric(dados[,5])

# ##############################################################
#                   CONHECENDO AS VARIÁVEIS                   ##
# ##############################################################

# Ocupação
dados = within(dados, {
  occupied <- factor(occupied, labels=c("Livre","Ocupado"))
})

tab1 = table(dados$occupied)
rotulos = paste(row.names(tab1),round(prop.table(tab1)*100,digits = 1),c("%","%"))
pie(tab1,labels = rotulos, col = c("tomato","darkblue"),main = )

# Bairros
tab2 = as.data.frame(table(dados$suburb))
ggplot(tab2, aes(x =fct_reorder(Var1,Freq), y = Freq), fill = Freq) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Freq),colour="blue") + 
  labs(title = "Bairros do listing",
       x = "Bairro",
       y = "Quantidade de Listings")+
  coord_flip()

# Superhost
tab3 = table(dados$is_superhost)
rot3 = paste(c("Não","Sim"),round(prop.table(tab3)*100,digits = 1),c("%","%"))
pie(tab3,labels = rot3, col = c("tomato","darkblue"),main = )

# Nota do anuncio
tab4 = as.data.frame(table(dados$star_rating))
ggplot(tab4, aes(x =Var1, y = Freq), fill = Freq) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Freq),colour="blue") + 
  labs(title = "Notas do listing",
       x = "Nota",
       y = "Quantidade de Listings")

# Nota do Banheiros
tab5 = as.data.frame(table(dados$number_of_bathrooms))
ggplot(tab5, aes(x =Var1, y = Freq), fill = Freq) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Freq),colour="blue") + 
  labs(title = "Número de banheiros do listing",
       x = "Quantidade de banheiros",
       y = "Quantidade de Listings")

# Nota do Quartos
tab6 = as.data.frame(table(as.factor(as.numeric(dados$number_of_bedrooms))))
ggplot(tab6, aes(x =Var1, y = Freq), fill = Freq) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Freq),colour="blue") + 
  labs(title = "Número de quartos do listing",
       x = "Quantidade de quartos",
       y = "Quantidade de Listings")

# Preços dos anuncios
tab7 = summary(dados$price_string)
est_price = data.frame(x=matrix(tab7),row.names=names(tab7))
print(est_price)
formattable(est_price)

# 1. Ordene os bairros em ordem crescente de número de listings (anuncios) - details

# ##############################
# Faturamento por bairro      ##
# ##############################

tb1=as.data.frame(sort(table(dados$suburb),decreasing = T));colnames(tb1)=c("Bairros","Listings")

ggplot(tb1, aes(x =fct_reorder(Bairros,Listings), y = Listings),) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Listings),colour="blue") + 
  theme(axis.text = element_text(size = 12))+
  labs(title = "Listings por bairro",
       x = "Bairro",
       y = "Quantidade de Listings")+
  coord_flip()

# 2. Ordene os bairros em ordem crescente de faturamento médio dos listings (anuncios) - details

# ##################################
# FATURAMENTO MÉDIO POR BAIRRO    ##
# ##################################

sel2=dados[,c(1,4,5,8)];
sel21 = aggregate(price_string ~ airbnb_listing_id, dados, sum) # faturamento
tab21 = aggregate(price_string ~ suburb, sel2, mean);tab21[,2]=round(tab21[,2],2) 

ggplot(tab21, aes(x =fct_reorder(suburb,price_string), y = price_string),) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = price_string),colour="blue") + 
  theme(axis.text = element_text(size = 12))+
  labs(title = "Faturamento médio por bairro",
       x = "Bairro",
       y = "Faturamento médio")+
  coord_flip()

# ########################################
# FATURAMENTO MÉDIO ANUAL POR BAIRRO    ##
# ########################################

tab22 = aggregate(price_string ~ suburb + year(date), sel2, mean);tab22[,3]=round(tab22[,3],digits = 2); colnames(tab22)[2]="ano" 
tab22 = within(tab22, {ano <- factor(ano, labels=c(2020,2021))})

ggplot(tab22, aes(x = suburb, y = price_string, fill = ano)) + 
  geom_col()+
  scale_fill_brewer(palette = "Reds")+
  labs(title = "Faturamento médio por bairro",
       x = "Bairro",
       y = "Faturamento médio")+
  geom_text(aes(label = price_string), position = position_stack(vjust = 0.5),colour = "blue")

# ###############################################
# FATURAMENTO MÉDIO MENSAL/ANUAL POR BAIRRO    ##
# ###############################################

tab23 = aggregate(price_string ~ suburb + year(date) + month(date,label = T), sel2, mean) 

formattable(tab23, list(area(col = c(price_string)) ~ normalize_bar("tomato", 0.2)))

# 3. Existem correlações entre as características de um anúncio e seu faturamento?

sel3=merge(details,sel21,by = "airbnb_listing_id")

#   TABELAS
# ################
# Bairro
# ################

tab31 = aggregate(price_string ~ suburb, sel3, mean)
ggplot(sel3, aes(y = price_string, x = suburb)) +
  labs(title = "Faturamento por bairro",
       x = "Bairro",
       y = "Faturamento")+
  geom_boxplot()

# ##################
# Número de quartos
# ##################
sel3[,5] = as.factor(as.numeric(sel3[,5]))

ggplot(sel3, aes(y = price_string, x = number_of_bedrooms)) +
  labs(title = "Faturamento por Número de quartos",
       x = "Número de quartos",
       y = "Faturamento")+
  geom_boxplot()

# ####################
# Número de banheiros
# ####################

sel3[,6] = as.factor(as.numeric(sel3[,6]))

ggplot(sel3, aes(y = price_string, x = number_of_bathrooms)) +
  labs(title = "Faturamento por Número de banheiros",
       x = "Número de banheiros",
       y = "Faturamento")+
  geom_boxplot()

# ############
# Superhost
# ###########

ggplot(sel3, aes(y = price_string, x = is_superhost)) +
  labs(title = "Faturamento por Superhost",
       x = "Superhost",
       y = "Faturamento")+
  geom_boxplot()

# ##################
# Nota do Anúncio
# ##################
sel3[,7] = as.factor(as.numeric(sel3[,7]))

ggplot(sel3, aes(y = price_string, x = star_rating)) +
  labs(title = "Faturamento por Nota do Anúncio",
       x = "Nota do Anúncio",
       y = "Faturamento")+
  geom_boxplot()

# ##################
# Número de Reviews
# ##################
cor(sel3$price_string,as.numeric(sel3$number_of_reviews),use = "pairwise")

#   a. Quais? Explique

# 4. Qual a antecedência média das reservas?

di=as.Date.character(dados$booked_on)
df=dados$date

antc = na.omit(df-di)
mean(antc)


#   a. Esse número é maior ou menor para finais de semana?
# ###############
# Dias da semana
# ###############
ds <- wday(di,label = T)

tab8 = as.data.frame(table(ds));
ggplot(tab8, aes(x =ds, y = Freq), fill = Freq) +
  geom_col(position = "dodge",fill = "tomato") +
  geom_label(aes(label = Freq),colour="blue") + 
  labs(title = "Número de listing por dia da semana",
       x = "Dia da semana",
       y = "Quantidade de Listings")
