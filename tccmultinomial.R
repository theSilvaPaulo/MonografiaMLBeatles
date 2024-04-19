library(tidyverse)
library(MASS)
library(readxl)
library(nnet)
library(xlsx)
library(cvms)
library(dplyr)
library(psych)
library(lmtest)
library(pROC)
library(caret)
library(mlogit)
library(corrplot)
library(DescTools)
library(broom)
library(pacman)
pacman::p_load(dplyr, car, psych, nnet, AER, lmtest,
               gtsummary, reshape2, ggplot2, DescTools)

#LEITURA E TRATAMENTO DOS DADOS

  # Lê os dados do arquivo Excel

  dadosParaRegressao <- read.xlsx("featuresmultinomial.xlsx", sheetName = "Sheet1",stringsAsFactors = TRUE)
  
  #glimpse(dadosParaRegressao)
  
# TRATANDO DADOS PARA REGRESSÃO
  
  # Remove colunas roubadas
  
  colunas_para_remover <- c("Song", "AlbumPosition", "Album", "Lyrics")
  dadosParaRegressao <- dadosParaRegressao[, !names(dadosParaRegressao) %in% colunas_para_remover]
  
  rm(colunas_para_remover)

# PRÉ-ANALISANDO OS DADOS
  
  # Analisa as frequências

  #summary(dadosParaRegressao)                #HÁ COLUNAS MUITO DESBALANCEADAS. É UM PROBLEMA?

  # Analisa se há alguma célula vazia
  
  #table(dadosParaRegressao$Era,dadosParaRegressao$Sings,dadosParaRegressao$ComposingConceptBy)
  
  # HÁ. E AGORA? Mas vou continuar.
  
  # Foi removido manualmente os ComposingConceptBy = Starr e Others.
  
  # Analisa e define categorias de referência
  
  #levels(dadosParaRegressao$Era)
  dadosParaRegressao$Sings <- relevel(dadosParaRegressao$Sings,ref="McCartney")
  dadosParaRegressao$ComposingConceptBy <- relevel(dadosParaRegressao$ComposingConceptBy,ref="McCartney")
  #levels(dadosParaRegressao$ComposingConceptBy)
  #levels(dadosParaRegressao$Sings)
  dadosParaRegressao$ChorusPresence <- relevel(dadosParaRegressao$ChorusPresence, ref = "No")
  dadosParaRegressao$TimeSignatureIn <- relevel(dadosParaRegressao$TimeSignatureIn, ref = "Four")
  
  # Transformar ComposingConceptBy em One-Hot me jogará na Dummy Variable Trap.
  # Observarei quais variáveis possuem correlação alta e eliminei a que possui menor correlação com "Era".
  # É Lennon.
  
  one_hot <- model.matrix(~ ComposingConceptBy - 1, data = dadosParaRegressao)
  one_hot_df <- as.data.frame(one_hot)
  one_hot_df <- one_hot_df[, -which(names(one_hot_df) == "ComposingConceptByLennon")]
  dadosParaRegressao$ComposingConceptBy <- NULL
  dadosParaRegressao <- cbind(dadosParaRegressao, one_hot_df)
  
# PRESSUPOSTOS PARA REGRESSÃO LOGÍSTICA MULTINOMIAL
  
  # 1. Variável dependente nominal (categorias mutuamente exclusivas)
  # 2. Independência das observações (sem repetições)
  # 3. Ausência de multicolinearidade. >10 = colinearidade
  
  modeloBasico <- lm(as.numeric(Era) ~ . ,dadosParaRegressao)
  
  #car::vif(modeloBasico)
  
  # Há colinearidade altíssima entre Sings e ComposingConceptBy, também entre as Lyrical.

  # Conferi correlação delas com Era. De Sings é maior.

  #Significância estatística de ComposingConceptBy é disparadamente maior. Eliminarei Sings.

  coluna_para_remover <- c("Sings")
  dadosParaRegressao <- dadosParaRegressao[, -which(names(dadosParaRegressao) %in% coluna_para_remover)]
  rm(coluna_para_remover)

  # Agora, resolver caso dos Lyrical. Conferindo P-valores, para ver se compensa manter:

  #modeloTodos <- MASS::polr(Era~.,data=dadosParaRegressao,Hess=T)
  #car::Anova(modeloTodos, type = "II", test = "Wald")

  #summary(dadosParaRegressao)
  
  # REVER!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  #Para resolver a multicolinearidade, utilizarei PCA.

  ############ PCA

  pca_valores <- data.frame(dadosParaRegressao$LyricalPositiveness,dadosParaRegressao$LyricalNeutralness,dadosParaRegressao$LyricalNegativeness)
  resultado_pca <- prcomp(pca_valores, center = TRUE, scale. = TRUE)

  #print(resultado_pca$sdev)
  #print(sum(resultado_pca$sdev))

  # Obtendo as proporções da variância explicada por cada componente
  prop_var_explicada <- (resultado_pca$sdev^2) / sum(resultado_pca$sdev^2)

  # Exibindo as proporções da variância explicada
  #print("Proporção da Variância Explicada por cada Componente Principal:")
  #print(prop_var_explicada)

  # Calculando a porcentagem da variância explicada por cada componente (multiplicando por 100)
  #porcentagem_var_explicada <- prop_var_explicada * 100

  # Exibindo a porcentagem da variância explicada por cada componente principal
  #print("Porcentagem da Variância Explicada por cada Componente Principal:")
  #print(porcentagem_var_explicada)

  componentes_principais <- resultado_pca$x
  #print("Componentes Principais:")
  #print(componentes_principais)

  # Os dois primeiros componentes explicam 99.9% da variância. Vou usar apenas os dois.

  dadosParaRegressao$PCALyricalAnalysis1 <- componentes_principais[, 1]
  dadosParaRegressao$PCALyricalAnalysis2 <- componentes_principais[, 2]
  
  # Encontrar colunas que começam com "Lyrical"
  colunas_para_remover <- names(dadosParaRegressao)[grepl("^Lyrical", names(dadosParaRegressao))]

  # Use o operador de subconjunto para remover as colunas
  dadosParaRegressao <- dadosParaRegressao[, -which(names(dadosParaRegressao) %in% colunas_para_remover)]

  rm(colunas_para_remover)

  ############ FIM PCA

  # Foi realizado o PCA aplicado às 3, o que resolveu a multicolinearidade.
  
  # Outra forma (psych):
  
  #colunas_para_plot <- setdiff(1:ncol(dadosParaRegressao), 19)
  #psych::pairs.panels(dadosParaRegressao[colunas_para_plot])
  
  # Outra forma (corrplot):
  
  #corrplot(cor(subset(dadosParaRegressao, select = -Era)))
  
  # Outra forma (VIF):
  
  #modeloVif = lm(as.numeric(Era) ~ ., data = dadosParaRegressao)
  
  #car::vif(modeloVif)
  
  # Parece estar passando.
  
  # 4. Independência das alternativas irreleantes (Hausman-McFadden)
  # criticas: https://statisticalhorizons.com/iia/
  # em modelos diferentes, remove-se cada uma das alternativas não-referência.
  # os coeficientes precisam ser muito parecidos com os originais.
  
  # removi DurationMs pois com ele, está acusando multicolinearidade.
  
  dadosParaHMTest <- dadosParaRegressao[, -which(names(dadosParaRegressao) == "DurationMs")]
  
  variaveis_regressao <- setdiff(names(dadosParaHMTest), "Era")

  formula_regressao <- as.formula(paste("Era ~ 1 |", paste(variaveis_regressao, collapse = "+")))
  
  modeloGeral <- mlogit::mlogit(formula_regressao, data = dadosParaHMTest, shape = "wide", reflevel = "Beatlemania")
  
  modeloTransition <- mlogit::mlogit(formula_regressao, data = dadosParaHMTest, shape = "wide", reflevel = "Beatlemania",alt.subset=c("Beatlemania","Psychedelic","Dissolution"))
  
  modeloPsychedelic <- mlogit::mlogit(formula_regressao, data = dadosParaHMTest, shape = "wide", reflevel = "Beatlemania",alt.subset=c("Beatlemania","Transition","Dissolution"))
  
  modeloDissolution <- mlogit::mlogit(formula_regressao, data = dadosParaHMTest, shape = "wide", reflevel = "Beatlemania",alt.subset=c("Beatlemania","Transition","Psychedelic"))
  
  #summary(modeloGeral)
  #summary(modeloTransition)
  #summary(modeloPsychedelic)
  #summary(modeloDissolution)
  
  mlogit::hmftest(modeloGeral,modeloTransition)
  mlogit::hmftest(modeloGeral,modeloPsychedelic)
  mlogit::hmftest(modeloGeral,modeloDissolution)
  
# CONSTRUÇÃO DO MODELO E DO MODELO NULO
  
  modeloOficial <- multinom(Era ~  ., data = dadosParaRegressao, model = TRUE,maxit=10000)
  
  modeloNulo <- multinom(Era ~ 1, data = dadosParaRegressao, model = TRUE,maxit=10000)
  
  # Conferindo ajuste do modelo
  
  anova(modeloOficial,modeloNulo)
  
  # p deu menor que 0.05. A previsibilidade é melhor que sem previsores.d
  
  DescTools::PseudoR2(modeloOficial,which="Nagelkerke")
  
  # NÃO se pode interpretar como porcentagem explicada dos dados.
  
  # Efeitos gerais:
  
  #car::Anova(modeloOficial, type = "II", test = "Wald") ##### AS VEZES PODE DESCOMENTAR!!!!!!!!!
  
  # Efeitos específicos:
  
  #summary(modeloOficial)
  
  # Obtendo p-valores
  
  print(tidy(modeloOficial),n=1000)
  #coeftest(modeloOficial)
  
  # Aplicando exponencial pois, por ser logístico, é difícil interpretar a magnitude do coeficiente.
  # Assim, é possível obter a razão de chances.
  # 1.33 = aumento de 33%, 1.33x
  
  #exp(coef(modeloOficial))
  
  # Intervalos de confiança para as razões de chance:
  # Razões de chance estatisticamente significativas são diferentes de 1, pois =1 = não tem efeito.
  
  #exp(confint(modeloOficial))

  # Tabela completa (precisa indicar em algum lugar na tabela que a categoria de referência de todas é a Beatlemania.)
  
  tbl_regression(modeloOficial,exponentiate=FALSE)
  tbl_regression(modeloOficial,exponentiate=TRUE)
  
# COMPARAÇÃO DE MODELOS
  
  # declaração do modelo secundário
  # comparação se é melhor q o nulo: anova(mod0,mod2)
  # Desctools mod2
  # Pseudo R2
  # Efeitos globais
  # Summary
  # coeftest
  # exp(coef(mod2)) e exp(coefint(mod2))
  # comparação AIC(mod1,mod2) haike - quanto menor, melhor
  # comparação BIC(mod1,mod2) criterio de informacao heyseano bayseano
  # anova(mod2,mod1,test="Chisq"). se PrChi for maior q 0,05, principio da parcimonia. Menos variaveis independentes.
  
# TABELA DE CLASSIFICAÇÃO
  
  tabela_classificacao <- table(Observado = dadosParaRegressao$Era, Previsto = predict(modeloOficial))
  
  prop.table(tabela_classificacao)
  
  acuracia <- sum(diag(tabela_classificacao)) / sum(tabela_classificacao)
  
  caret::confusionMatrix(predict(modeloOficial),dadosParaRegressao$Era)
  
  
# VISUALIZAÇÃO GRÁFICA
    
  dados_prev <- cbind(   (dadosParaRegressao[setdiff(1:ncol(dadosParaRegressao), 19)])    ,(predict(modeloOficial,type="probs",se=TRUE)))
  
  nomes_colunas <- names(dadosParaRegressao)
  coluna_a_excluir <- 19 # ERA
  nomes_colunas <- nomes_colunas[-coluna_a_excluir]
  
  dados_prev$ChorusPresence <- ifelse(dados_prev$ChorusPresence == "Yes", 1, 0)
  dados_prev$Mode <- ifelse(dados_prev$Mode == "Major", 1, 0)
  dados_prev$ChangesKey <- ifelse(dados_prev$ChangesKey == "Yes", 1, 0)
  dados_prev$TimeSignatureIn <- ifelse(dados_prev$TimeSignatureIn == "Four", 1, 0)
  dados_prev$ComposingConceptByMcCartney <- ifelse(dados_prev$ComposingConceptByMcCartney == "Yes", 1, 0)
  dados_prev$ComposingConceptByHarrison <- ifelse(dados_prev$ComposingConceptByHarrison == "Yes", 1, 0)
  dados_prev$ComposingConceptByLennonMcCartney <- ifelse(dados_prev$ComposingConceptByLennonMcCartney == "Yes", 1, 0)

  dados_prev <- reshape2::melt(dados_prev,
                               id.vars = nomes_colunas,
                               value.name = "Probabilidade",
                               variable.name = "Era")
  
  ggplot(dados_prev, aes(x = DurationMs, y = Probabilidade)) +
    geom_smooth(method = "loess", span = 1) +
    labs(y = "Probabilidade de Pertencimento", x = "Duração da Canção em Milissegundos") +
    scale_y_continuous(label = scales::percent_format(decimal.mark = ",")) +
    scale_x_continuous(labels = scales::number_format()) +  # Adicionando esta linha
    facet_grid(Era ~ .) +
    theme_bw() +
    guides(color = guide_legend(override.aes = list(fill = NA)))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  

  
  # AUC K-FOLD 
  
  num_folds <- 7

  auc_total <- numeric(num_folds)
  
  fold_size <- nrow(dadosParaRegressao) %/% num_folds
  
  for (fold in 1:num_folds) {
    
    start_index <- (fold - 1) * fold_size + 1
    end_index <- fold * fold_size
    
    test_indices <- start_index:end_index
    
    dadosTreino <- dadosParaRegressao[-test_indices, ]
    dadosTeste <- dadosParaRegressao[test_indices, ]
    
    modeloTeste <- multinom(Era ~ ., data = dadosTreino, model = TRUE, maxit = 10000)
    
    predictauc <- predict(modeloTeste, newdata = dadosTeste, type = "prob")
    
    respostas_reais <- dadosTeste$Era
    
    roc_obj <- multiclass.roc(respostas_reais, predictauc)
    
    
    predictions <- predict(modeloTeste, newdata = dadosTeste, type = "class")
    conf_matrix <- table(respostas_reais, predictions)
    
    # Calcule a acurácia
    accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
    cat("Acurácia:", accuracy, "\n")
    
    auc_fold <- auc(roc_obj, average = "macro")
    
    auc_total[fold] <- auc_fold
    
    print(paste("AUC Média para Fold", fold, ":", auc_fold))
  }

  auc_media <- mean(auc_total)
  
  print(paste("AUC Média Global:", auc_media))
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # DESCOBRIR THRESHOLD DE MAIOR ACURÁCIA
  
  thresholdPerfeito <- 0
  acertou_total <- 0
  maior = 0
  
  for (threshold in seq(0, 1, 0.05)) {
    
    acertou_total <- 0
    
    print(threshold)
    
    for (linha in 1:nrow(dadosParaRegressao)) {
      
      novaBase <- dadosParaRegressao[-linha, ]
      
      modeloTeste <- multinom(Era ~  ., data = novaBase, model = TRUE,maxit=10000,trace="false")
      
      dadosPrever <- as.data.frame(dadosParaRegressao[linha, ])
      
      predicoes <- predict(modeloTeste, newdata = dadosPrever, type = "probs")
      
      predicoes_binarias <- ifelse(predicoes["Dissolution"] > threshold, "Dissolution",
                                   ifelse(predicoes["Psychedelic"] > threshold, "Psychedelic",
                                          ifelse(predicoes["Transition"] > threshold, "Transition", "Beatlemania")))
      
      matriz_confusao <- table(real = dadosPrever$Era,previsto = predicoes_binarias)
      
      acertou <- ifelse(matriz_confusao[predicoes_binarias, ] == 1, 1, 0)
      
      acertou_total <- acertou + acertou_total
      
    }
    
    acuracia = acertou_total/nrow(dadosParaRegressao)
    
    if (acuracia > maior){
      thresholdPerfeito <- threshold
      maior = acuracia
    }
    
  }
  
  print(maior)
  print(thresholdPerfeito)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # AUC com mesmo conjunto
  
  respostasTeste <- dadosParaRegressao$Era
  
  probabilidadesTeste <- predict(modeloOficial, newdata = dadosParaRegressao, type = "probs")
  
  roc_object = multiclass.roc(respostasTeste, probabilidadesTeste)
  
  print(auc(roc_object))
  
  # Extrair as classes reais (Era) como um vetor numérico
  classes_reais <- as.numeric(respostasTeste)
  
  # Calcular a AUC para cada classe
  
  auc_values <- numeric()
  class_names <- levels(respostasTeste)
  for (i in 1:(nlevels(respostasTeste))) {
    rocccccc <- roc(as.numeric(classes_reais == i), probabilidadesTeste[, i])
    auc_values[i] <- auc(rocccccc)
    cat("AUC para a classe", class_names[i], ":", auc_values[i], "\n")
    roc_df <- data.frame(
      TPR=rev(rocccccc$sensitivities), 
      FPR=rev(1 - rocccccc$specificities),
      thresholds=rev(rocccccc$thresholds)
    )
    print(roc_df)
    
    grafico <- ggplot(data = roc_df, aes(x = FPR, y = TPR)) +
      geom_point() +
      ggtitle(class_names[i]) +
      xlab("FPR") +
      ylab("TPR")
    
    print(grafico)
    
  }

  mean_auc <- mean(auc_values)
  print("AUC média:")
  print(mean_auc)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # IGNORAR
  
  total_rows <- nrow(dadosParaRegressao)
  
  indices_treino <- sample(1:total_rows, 0.7 * total_rows)
  
  dadosTreino <- dadosParaRegressao[indices_treino, ]
  
  modeloTreinado <- multinom(Era ~  ., data = dadosTreino,maxit=10000,trace="false")
  
  dadosTeste <- dadosParaRegressao[-indices_treino, ]
  
  probabilidadesTeste <- predict(modeloTreinado, newdata = dadosTeste, type = "probs")
  
  respostasTeste <- dadosTeste$Era
  
  roc_object = multiclass.roc(respostasTeste, probabilidadesTeste)
  
  print(auc(roc_object))
  
  
  # Extrair as classes reais (Era) como um vetor numérico
  classes_reais <- as.numeric(dadosTeste$Era)
  
  # Calcular a AUC para cada classe
  
  auc_values <- numeric()
  class_names <- levels(dadosTeste$Era)
  for (i in 1:(nlevels(dadosTeste$Era))) {
    rocccccc <- roc(as.numeric(classes_reais == i), probabilidadesTeste[, i])
    auc_values[i] <- auc(rocccccc)
    cat("AUC para a classe", class_names[i], ":", auc_values[i], "\n")
    roc_df <- data.frame(
      TPR=rev(rocccccc$sensitivities), 
      FPR=rev(1 - rocccccc$specificities),
      thresholds=rev(rocccccc$thresholds)
    )
    print(roc_df)
    
    grafico <- ggplot(data = roc_df, aes(x = FPR, y = TPR)) +
      geom_point() +
      ggtitle(i) +
      xlab("FPR") +
      ylab("TPR")
    
    print(grafico)
    
    
  }
  
  mean_auc <- mean(auc_values)
  print("AUC média:")
  print(mean_auc)
  
  print(indices_treino)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Inicializando variáveis
  thresholdPerfeito <- 0
  acertou_total <- 0
  maior <- 0
  
  # Inicializando lista para armazenar as médias de acurácia de cada classe
  media_acuracia_classe <- c(Dissolution = 0, Psychedelic = 0, Transition = 0, Beatlemania = 0)
  
  # Loop sobre diferentes thresholds
  for (threshold in seq(0, 1, 0.05)) {
    
    # Zerando acertos totais para cada threshold
    acertou_total <- 0
    
    # Loop sobre cada linha dos dados
    for (linha in 1:nrow(dadosParaRegressao)) {
      
      # Criando nova base excluindo a linha atual
      novaBase <- dadosParaRegressao[-linha, ]
      
      # Treinando modelo com a nova base
      modeloTeste <- multinom(Era ~ ., data = novaBase, model = TRUE, maxit = 10000, trace = "false")
      
      # Obtendo dados a serem previstos
      dadosPrever <- as.data.frame(dadosParaRegressao[linha, ])
      
      # Prevendo probabilidades
      predicoes <- predict(modeloTeste, newdata = dadosPrever, type = "probs")
      
      # Convertendo probabilidades em classes binárias com base no threshold
      predicoes_binarias <- ifelse(predicoes["Dissolution"] > threshold, "Dissolution",
                                   ifelse(predicoes["Psychedelic"] > threshold, "Psychedelic",
                                          ifelse(predicoes["Transition"] > threshold, "Transition", "Beatlemania")))
      
      # Criando matriz de confusão
      matriz_confusao <- table(real = dadosPrever$Era, previsto = predicoes_binarias)
      
      # Calculando acertos para cada classe
      acertos_classe <- sapply(names(media_acuracia_classe), function(classe) {
        ifelse(matriz_confusao[classe, ] == 1, 1, 0)
      })
      
      # Atualizando acertos totais
      acertou_total <- sum(acertos_classe) + acertou_total
      
      # Atualizando médias de acurácia para cada classe
      media_acuracia_classe <- media_acuracia_classe + acertos_classe / nrow(dadosParaRegressao)
      
    }
    
    # Calculando acurácia total
    acuracia <- acertou_total / nrow(dadosParaRegressao)
    
    # Atualizando thresholdPerfeito e maior se a acurácia atual for maior
    if (acuracia > maior) {
      thresholdPerfeito <- threshold
      maior <- acuracia
    }
    
  }
  
  # Imprimindo a maior acurácia e o threshold correspondente
  print("Maior Acurácia:")
  print(maior)
  print("Threshold Correspondente:")
  print(thresholdPerfeito)
  
  # Imprimindo a média de acurácia para cada classe
  print("Média de Acurácia por Classe:")
  print(media_acuracia_classe)
  