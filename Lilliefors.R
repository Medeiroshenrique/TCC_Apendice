# Carregar pacotes
library(readxl)
library(dplyr)  # Pacote necessário para o operador %>%
library(nortest)  # Pacote necessário para o teste de Lilliefors
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

# Aplicar o teste de Lilliefors para cada metodologia
resultado_lillie <- dados %>%
  group_by(Metodologia) %>%
  summarise(
    lillie_test = list(lillie.test(Erros)), # Aplica o teste Lilliefors para os erros de cada metodologia
    .groups = 'drop'
  )

# Extrair o valor p do teste de Lilliefors para cada metodologia
resultado_lillie <- resultado_lillie %>%
  mutate(p_value = sapply(lillie_test, function(x) x$p.value)) %>%
  select(Metodologia, p_value)

# Converter a tabela de resultados em uma imagem usando gridExtra
tabela_lillie <- tableGrob(resultado_lillie, rows = NULL)

# Salvar a tabela como imagem .png
caminho_imagem_lillie <- "/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/graficos/resultado_lillie.png"
ggsave(filename = caminho_imagem_lillie, plot = tabela_lillie, width = 5, height = 2, dpi = 300)

# Mensagem de confirmação
cat("O gráfico do teste de Lilliefors foi salvo em: ", caminho_imagem_lillie, "\n")
