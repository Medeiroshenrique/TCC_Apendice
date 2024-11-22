# Carregar pacotes
library(readxl)
library(ggplot2)

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
rodada1_e1_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/planilhas/Rodada 1 (E1) - ADHOC.xlsx", "E1", "Rodada 1", "Adhoc")
rodada2_e2_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/planilhas/Rodada 2 (E2) - ADHOC.xlsx", "E2", "Rodada 2", "Adhoc")
rodada1_e2_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/planilhas/Rodada 1 (E2) - SonarQube.xlsx", "E2", "Rodada 1", "SonarQube")
rodada2_e1_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/planilhas/Rodada 2 (E1) - SonarQube.xlsx", "E1", "Rodada 2", "SonarQube")

# Combinar todos os dados em um único data frame
dados <- rbind(rodada1_e1_adhoc, rodada2_e2_adhoc, rodada1_e2_sonar, rodada2_e1_sonar)

# Gerar o histograma apenas dividido por Metodologia
ggplot(dados, aes(x = Erros, fill = Metodologia)) +
  geom_histogram(binwidth = 5, position = "dodge", color = "black") +
  labs(title = "Distribuição dos Erros Encontrados por Metodologia",
       x = "Número de Erros Encontrados por Estudante",
       y = "Frequência") +
  theme_minimal() +
  scale_fill_manual(values = c("Adhoc" = "coral", "SonarQube" = "skyblue"))