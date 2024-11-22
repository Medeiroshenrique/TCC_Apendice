# Carregar pacotes
library(readxl)
library(ggplot2)

# Definir o diretório do script como diretório de trabalho
script_dir <- dirname(sys.frame(1)$ofile)
setwd(script_dir)

# Função para ler os dados
ler_dados <- function(caminho, equipe, rodada, metodologia) {
  # Ler os dados da planilha (ajustar o range de leitura para uma linha completa de erros)
  erros <- read_excel(caminho, range = "A2:H2", col_names = FALSE)  # Ajuste o range se necessário
  
  # Exibir o conteúdo lido para diagnóstico
  print(erros)  # Verifique os dados lidos
  
  # Ajustar número de participantes com base na equipe e rodada
  if (equipe == "E1") {
    n_participantes <- 8  # Equipe 1 tem 8 participantes
    participantes <- paste0("P", 1:n_participantes)  # Identificadores de P1 a P8
  } else {
    if (rodada == "Rodada 1") {
      n_participantes <- 8  # Equipe 2 tem 8 participantes incluindo P15 na rodada 1
      participantes <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P15")
    } else {
      n_participantes <- 7  # Equipe 2 tem 7 participantes (excluindo P15 na rodada 2)
      participantes <- c("P1", "P2", "P3", "P4", "P5", "P6", "P7")
    }
  }
  
  # Garantir que o número de erros seja o mesmo que o número de participantes
  erros <- as.numeric(unlist(erros))  # Converter para numérico
  
  # Remover valores NA dos erros (se houver)
  erros <- erros[!is.na(erros)]
  
  # Ajustar o número de participantes com base no número de erros lidos
  participantes <- participantes[1:length(erros)]
  
  # Retornar o dataframe com os dados
  data.frame(
    Participante = participantes,  # Ajuste o número de participantes
    Erros = erros,
    Rodada = rodada,
    Metodologia = metodologia,
    Equipe = equipe
  )
}

# Atualizar os caminhos para o novo diretório
rodada1_e1_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 1 (E1) - ADHOC.xlsx", "E1", "Rodada 1", "Adhoc")
rodada2_e2_adhoc <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 2 (E2) - ADHOC.xlsx", "E2", "Rodada 2", "Adhoc")
rodada1_e2_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 1 (E2) - SonarQube.xlsx", "E2", "Rodada 1", "SonarQube")
rodada2_e1_sonar <- ler_dados("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/Planilhas_SEM_OUTLIER/Rodada 2 (E1) - SonarQube.xlsx", "E1", "Rodada 2", "SonarQube")

# Combinar todos os dados em um único data frame
dados <- rbind(rodada1_e1_adhoc, rodada2_e2_adhoc, rodada1_e2_sonar, rodada2_e1_sonar)

# Criar a pasta para salvar as plotagens, se ela não existir
dir.create("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/plotagens", showWarnings = FALSE)

# Gerar o boxplot
p <- ggplot(dados, aes(x = Metodologia, y = Erros, fill = Metodologia)) +
  geom_boxplot() +
  labs(title = "Desempenho dos Estudantes por Metodologia",
       x = "Metodologia",
       y = "Número de Erros Encontrados") +
  theme_minimal() +
  scale_fill_manual(values = c("Adhoc" = "coral", "SonarQube" = "skyblue")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))  # Formatar o eixo y para 2 casas decimais

# Salvar o boxplot no diretório especificado
ggsave("/home/henriquemedeiros/Documents/TCC/2024.3 TCC/R_Plotagens/plotagens/desempenho_estudantes_por_metodologia.png", plot = p, width = 8, height = 6)
