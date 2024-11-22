# Carregar pacotes
library(readxl)
library(ggplot2)
library(dplyr)  # Pacote necessário para o operador %>%
library(gridExtra)  # Para salvar tabelas como imagens
library(grid)  # Para usar as funções do grid na criação da tabela
library(nortest)  # Pacote para o teste de Anderson-Darling

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

# Função para adicionar jitter (ruído pequeno) aos dados para evitar empates
adicionar_jitter <- function(dados) {
  jitter(dados, amount = 0.1)  # A quantidade pode ser ajustada conforme necessário
}

# Visualização preliminar dos dados de erro para a metodologia Adhoc
ggplot(dados %>% filter(Metodologia == "Adhoc"), aes(x = Erros)) +
  geom_histogram(bins = 10, fill = "skyblue", color = "black") +
  labs(title = "Distribuição dos Erros - Adhoc")

# Aplicar o teste de Anderson-Darling para cada metodologia
resultado_ad <- dados %>%
  group_by(Metodologia) %>%
  summarise(
    ad_test = list(ad.test(adicionar_jitter(Erros))), # Aplica o teste Anderson-Darling com jitter
    .groups = 'drop'
  )

# Verificar o resultado do teste Anderson-Darling para Adhoc
resultado_ad_adhoc <- resultado_ad %>% filter(Metodologia == "Adhoc")
print(resultado_ad_adhoc)

# Extrair o valor p do teste de Anderson-Darling e arredondar para 2 casas decimais
resultado_ad <- resultado_ad %>%
  mutate(p_value = round(sapply(ad_test, function(x) x$p.value), 2)) %>%
  select(Metodologia, p_value)

# Converter a tabela de resultados em uma imagem usando gridExtra
tabela <- tableGrob(resultado_ad, rows = NULL)

# Salvar a tabela como imagem .png
caminho_imagem <- "/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/graficos/resultado_ad.png"
ggsave(filename = caminho_imagem, plot = tabela, width = 5, height = 2, dpi = 300)

# Mensagem de confirmação
cat("O gráfico foi salvo em: ", caminho_imagem, "\n")
