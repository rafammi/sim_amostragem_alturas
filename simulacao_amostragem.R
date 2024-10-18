#### PACOTES ####
library(ggplot2)
library(dplyr)

#### SEED ####
set.seed(123)

## carregar dados
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
dados<-read.csv('teste.csv')

#### PLOTAR DADOS ####

ggplot(dados,aes(x=dap,y=altura)) +
  geom_point(shape = 21, fill = NA, color = "black") + 
  geom_smooth(method='lm',col='lightblue') + facet_wrap(~id)
  theme_bw()

# histogramas
ggplot(dados, aes(x = dap)) +
  geom_histogram(binwidth = 2 * IQR(dados$dap) / length(dados$dap)^(1/3)) +
  labs(title = "Hist dap",
       x = "DAP (cm)",
       y = "Freq.") +
  theme_bw()

ggplot(dados, aes(x = altura)) +
  geom_histogram(binwidth=2 * IQR(dados$altura) / length(dados$altura)^(1/3)) +
  labs(title = "Hist alturas",
       x = "Altura (m)",
       y = "Freq.") +
  theme_bw() + theme(axis.text=element_text(size=6))

ggplot(dados, aes(x = dap)) +
  geom_histogram(binwidth = 2 * IQR(dados$dap) / length(dados$dap)^(1/3)) +
  labs(title = "Hist p/ talhao",
       x = "DAP (cm)",
       y = "Freq.") + facet_wrap(~id) +
  theme_bw()

ggplot(dados, aes(x = altura)) +
  geom_histogram(binwidth=2 * IQR(dados$altura) / length(dados$altura)^(1/3)) +
  labs(title = "Hist p/ talhao",
       x = "Altura (m)",
       y = "Freq.") + facet_wrap(~id) +
  theme_bw() + theme(axis.text=element_text(size=6))


#### 1A FUNCAO: SIMULAR DADOS COM BASE EMPIRICA ####
# 'amostras' serao coletadas com base no valor estipulado de amostragem
# ex: sampled_dados ira receber desde o primeiro valor ate o numero especificado
#  pela amostragem essa funcao vai ser a base de nossa simulacao
sample_dados <- function(sample_size, dados) {
  sampled_dados <- dados[sample(1:nrow(dados), 
                                   sample_size, replace = FALSE), ]
  return(sampled_dados)
}

#### 2A FUNCAO: OBTER METRICAS DE DADOS ####
# finalmente, essa fun vai utilizar da ultima 'sample_dados' para puxar diferentes 
# combinacoes de amostras com base na quantidade de simulacoesrecebidas pela 
# variavel de controle e extrair o valor do erro em uma regressao linear
simulacao_amostra <- function(sample_size, n_simulations, dados) {
 error <- numeric(n_simulations)
  for (i in 1:n_simulations) {
    dados <- sample_dados(sample_size, dados)
    model <- lm(altura ~ dap, data = dados)
    error[i] <- ((summary(model)$sigma) / mean(dados$altura)) * 100
  }
  return(error)
}


sample_sizes <- seq(5 , 70, 1)  # nossas amostras vao de 5 ate 50 com passo 1
n_simulations <- 10 #simularemos 20 vezes


results_id <-data.frame() #cria um df vazio que ira receber os resultados

# para cada quantidade de amostras simuladas (5, 6, ..., 50)
for (sample_size in sample_sizes) {
  # para cada id diferente (ex: cada talhao)
  for (id in unique(dados$id)) {  
    current_dados <- dados[dados$id == id, ]
    error <- simulacao_amostra(sample_size, n_simulations, current_dados)
    results_id <- rbind(results_id, 
                     data.frame(talhao = id, 
                                SampleSize = sample_size, 
                                Error = error))
  }
}

## sumarizar resultados obtidos em um df
summary_results_id <- results_id %>%
  group_by(talhao,SampleSize) %>%
  summarize(MeanError = mean(Error), SD = sd(Error))


#### RESULTADOS ####

# verificar valores de estabilizacao do erro, permitindo alocar um numero
# minimo de arvores amostradas por talhao, garantindo uma maior base
# na hora de mensuracao
  p<-ggplot(summary_results_id, aes(x = SampleSize, y = MeanError)) +
  geom_line() +
  geom_point() + geom_hline(yintercept = 0) +
  geom_errorbar(aes(ymin = MeanError - SD, ymax = MeanError + SD), width = 0.1) +
  labs(title = "Simulacao - Alturas p/ talhao",
       x = "Arvores Amostradas",
       y = "Syx%") +
  theme_bw() + theme(axis.text=element_text(size=7))

  p + facet_wrap(~talhao)  
  
