# Carregar pacotes
library(readxl)
library(ggplot2)
library(dplyr)  # Pacote necessário para o operador %>%
library(ggpubr)  # Para usar ggqqplot
library(gridExtra)  # Para salvar tabelas como imagens
library(grid)  # Para usar as funções do grid na criação da tabela

# Função para ler apenas os dados de erros e definir os identificadores manualmente
ler_dados <- function(caminho, equipe, rodada, metodologia) {
  # Ler apenas a segunda linha, que contém o número de erros
  erros <- read_excel(caminho, range = "A2:H2", col_names = FALSE)
  
  # Criar data frame com identificadores de participantes e erros
  data.frame(
    Participante = paste0("P", 1:8),  # Definir identificadores de P1 a P8
    Erros = as.numeric(unlist(erros)),  # Converter a linha de erros para numérico
    Rodada = rodada,
    Metodologia = metodologia,
    Equipe = equipe
  )
}

# Carregar dados das quatro planilhas usando a função
rodada1_e1_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 1 (E1) - ADHOC.xlsx", "E1", "Rodada 1", "Adhoc")
rodada2_e2_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 2 (E2) - ADHOC.xlsx", "E2", "Rodada 2", "Adhoc")
rodada1_e2_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 1 (E2) - SonarQube.xlsx", "E2", "Rodada 1", "SonarQube")
rodada2_e1_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 2 (E1) - SonarQube.xlsx", "E1", "Rodada 2", "SonarQube")

# Combinar todos os dados em um único data frame
dados <- rbind(rodada1_e1_adhoc, rodada2_e2_adhoc, rodada1_e2_sonar, rodada2_e1_sonar)

# Gerar QQ plots para cada metodologia
metodologias <- unique(dados$Metodologia)

for(metodologia in metodologias) {
  # Filtrar dados para a metodologia atual
  dados_metodologia <- subset(dados, Metodologia == metodologia)
  
  # Remover valores não finitos (NA, Inf, -Inf)
  dados_metodologia <- dados_metodologia %>%
    filter(!is.na(Erros) & is.finite(Erros))  # Remover NA e Inf
  
  # Criar o QQ plot usando ggqqplot
  qqplot <- ggqqplot(dados_metodologia$Erros, 
                     color = "blue", 
                     xlab = "Distribuição Normal", 
                     ylab = metodologia, 
                     title = paste("QQ Plot para", metodologia))
  
  # Definir o caminho para salvar a imagem
  caminho_imagem <- paste0("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/graficos/qqplot_", metodologia, ".png")
  
  # Salvar o gráfico em formato PNG
  ggsave(filename = caminho_imagem, plot = qqplot, width = 4, height = 4, units = "in", dpi = 300)
  
  # Mensagem de confirmação
  cat("O QQ Plot foi salvo em: ", caminho_imagem, "\n")
}
