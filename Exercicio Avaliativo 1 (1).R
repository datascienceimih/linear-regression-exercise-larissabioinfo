### Machine Learning ### Regressão Linear
## Exercício 1
# Banco de Dados Boston
#####################################
#Abra o banco de dados Boston e estime 3 regressões:

# Se os pacotes necessários não estiverem instalados, faça a instalação
if (! "MASS" %in% installed.packages()) install.packages("MASS")
if (! "dplyr" %in% installed.packages()) install.packages("dplyr")
if (! "ggplot2" %in% installed.packages()) install.packages("ggplot2")
if (! "readr" %in% installed.packages()) install.packages("readr")
if (! "texreg" %in% installed.packages()) install.packages("texreg")

# Carregando o pacote do livro ISLR
library(readr)
library(dplyr) #manipulação 
library(texreg) #tabelas de regressão formatadas 
library(ggplot2)


# Carrega os dados Advertisement
Boston = read_csv("https://raw.githubusercontent.com/selva86/datasets/master/BostonHousing.csv")
Boston

# 1) Abra o banco de dados Boston e ESTIME 3 REGRESSOES:

#A primeira prevendo medv usando como preditor #lstat;
#A segunda prevendo medv usando como preditor #age;
#A terceira usando ambos os preditores.

reg1 = lm(medv ~ lstat, data = Boston) 
reg1

reg2 = lm(medv ~ age, data = Boston) 
reg2

reg_completa = lm(medv ~ lstat + age, data = Boston)
reg_completa

#O que podemos dizer sobre o comportamento dos coeficientes estimados? 
#E sobre o ajuste dos modelos?
summary(reg1)
summary(reg2)
summary(reg_completa)
### No moelo Regi e Reg2 os modelos tiverem alta significancia=.
### No modelo Reg com as duas vairaveis, apesar de ainda ser sgnificante, 
##perde-se um pouco sua significancia
################################################################################

# 2 - Estime uma regressão usando todas as variáveis como preditoras. 
#Todas são uteis para prever medv? Quais você deixaria e quais tiraria? Por quê?
#Como foi o ajuste desse modelo?
reg_completa2 = lm(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio +
                     b + lstat, data = Boston)

reg_completa2  
summary(reg_completa2)
#Resposta
#Não.Deixaria todas, exceto "indus" e "age". Indus e Age nao sao significativas.

                
#Agora tire as variaveis que voce considerou que nao ajudam a estimacao de medv. 
#O que aconteceu com o ajuste do modelo?
reg_completa3 = lm(medv ~ crim + zn + chas + nox + rm  + dis + rad + tax + ptratio +
                     b + lstat, data = Boston)
reg_completa3
summary(reg_completa3)

# resposta : Tirando as variaveis que nao tinham significancia , obtivemos resultado parecido.
#------------------------------------------------------------------------------------------
# Exercício 2
#Vamos fazer algumas investigações no banco credit. Aí estão dados de cartão de 
#crédito com as seguintes variáveis:

#Income - renda em milhares de dólares;
#Limit - limite de crédito;
#Rating - o score de crédito;
#Card - número de cartões de crédito que a pessoa possui;
#Age - idade;
#Education - anos de escolaridade;
#Gender - sexo;
#Student - se é estudante;
#Married - se é casado;
#Ethnicity - etnia e
#Balance - dívida média de cartão de crédito.

bd_credit = read_csv("http://www-bcf.usc.edu/~gareth/ISL/Credit.csv")
bd_credit

bd_credit$Gender = factor(bd_credit$Gender,levels = c("Male","Female"),
                          labels = c("Male","Female"))
str(bd_credit$Gender)
head(bd_credit)
# 1 - Estude a diferença entre o balance entre homens e mulheres usando 
# um modelo de regressão. Interprete os resultados.

reg_gender = lm(Balance ~Gender, data = bd_credit) 
reg_gender
summary(reg_gender)
# Resposta : A variável preditora Gender prevê o Balance, 
#não existe correlação nenhuma entre o Balance e Gender. 
#Então podemos concluir que a dívida de crédito não tem relação entre ser Homem e Mulher.

#2
#Estude a diferença entre o balance entre pessoas de diferentes etnias 
#usando um modelo de regressão. Interprete os resultados.
reg_Ethnicity2 = lm(Balance ~ Ethnicity , data = bd_credit) 
reg_Ethnicity2
summary(reg_Ethnicity2)

# Resposta
#O modelo_credit_v2 mostra que não existe  relação nenhuma 
#entre etnia com o valor da dívida de crédito.
# esta maior do que 0,05 ( não há relação)


#3
#Estime agora um modelo de regressão utilizando todas as variáveis presentes. 
#O que podemos dizer sobre os resultados? Todas as variáveis ajudam a 
#explicar o balance? 
#Quais variáveis você tiraria? Como ficaria seu novo modelo?


reg3 = lm(Balance ~ Income + Limit + Rating + Cards + Age + Education
               + Gender + Student + Married + Ethnicity, bd_credit)
reg3 
summary(reg3)

#Resposta :
# Nem todas as variaveis ajudam a explicar
#Tiraria as que tem menos significania : Education / GenderMale / MarriedYes
# EthnicityAsian / EthnicityCaucasian
#O novo modelo ficaria da seguinte forma : 
#4
#Estime o seu novo modelo. Interprete os resultados. 
#O que podemos dizer sobre o ajuste do novo modelo em relação 
#ao primeiro?
reg4 = lm(Balance ~ Income + Limit + Rating + Cards + Age 
           + Student , bd_credit)
reg4 
summary(reg4)
