# Carregar pacotes
library(readxl)
library(ggplot2)
library(dplyr)  # Pacote necessário para o operador %>%
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

# Aplicar o teste de Shapiro-Wilk para cada metodologia
resultado_shapiro <- dados %>%
  group_by(Metodologia) %>%
  summarise(
    shapiro_test = list(shapiro.test(Erros)), # Aplica o teste para os erros de cada metodologia
    .groups = 'drop'
  )

# Extrair o valor p do teste de Shapiro-Wilk para cada metodologia e arredondar para 2 casas decimais
resultado_shapiro <- resultado_shapiro %>%
  mutate(p_value = round(sapply(shapiro_test, function(x) x$p.value), 2)) %>%
  select(Metodologia, p_value)

# Converter a tabela de resultados em uma imagem usando gridExtra
tabela <- tableGrob(resultado_shapiro, rows = NULL)

# Salvar a tabela como imagem .png
caminho_imagem <- "/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/graficos/resultado_shapiro.png"
ggsave(filename = caminho_imagem, plot = tabela, width = 5, height = 2, dpi = 300)

# Mensagem de confirmação
cat("O gráfico foi salvo em: ", caminho_imagem, "\n")
