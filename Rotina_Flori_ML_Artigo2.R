## Análise do Segundo Artigo 
## Florianopolis 10 anos 

## Pacotes necessários
library(dplyr)
library(ranger)
library(readxl)
library(tidyverse)  
library(timetk)  
library(tsibble)  
library(tsibbledata)
library(fastDummies)
library(tidymodels)
library(skimr)
library(xgboost)
library(parsnip)
library(kernlab)
library(lightgbm)
library(kableExtra)
library(modeltime)
library("bonsai")


## Entrada dos dados
setwd("G:\\Meu Drive\\Mestrado\\Dissertação\\Análises\\Temp de Flori - 2º Artigo\\Florianopolis 10 anos")
dados <- read_xlsx("Atualizada.xlsx") 
colnames(dados)<-c("Data.Medicao", "PRECIPITACAO", "PRESSAO","TEMP_ORV",
                   "TEMP_MEDIA","UMIDADE","VENT_.MED")
glimpse(dados)


# Convertendo objeto para o tipo tibble e
# Transforma data de medição para o tipo date
dados1 <- dados %>%
  tk_tbl() %>%
  mutate(Data.Medicao = as.Date(Data.Medicao))
dados1

# Plot da série temporal
dados1 %>%
  plot_time_series(Data.Medicao, TEMP_MEDIA,
                   .title = NULL,
                   .smooth  = FALSE, 
                   .interactive = TRUE)

# Criando os conjuntos de Treino e teste
particao <- dados1 %>%
  time_series_split(Data.Medicao, 
                    assess = 1095, 
                    cumulative = TRUE)

# Quantidade de elementos por conjuntos Treino/Teste/Total
particao

particao %>%
  tk_time_series_cv_plan() %>% glimpse()

# Plot da serie nos conjuntos de Treinio e Teste
particao %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(.date_var = Data.Medicao, 
                           .value = TEMP_MEDIA, 
                           .title = NULL)

# Preparando dados para análise
recipe <- recipe(TEMP_MEDIA ~ ., data = training(particao)) %>%
  step_rm(matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>% 
  step_dummy(all_nominal(), one_hot = TRUE) 

recipe

# Ajuste dos métodos de ML aos dados de treino (sem seleção dos hiperparametros) 
# Random Forest
fit_rf <- workflow() %>%
  add_model(
    spec = rand_forest(
      mode = "regression"
    ) %>% 
      set_engine("ranger")
  ) %>%
  add_recipe(recipe %>% 
               update_role(Data.Medicao, new_role = "indicator")) %>%
  fit(training(particao))

fit_rf
fit_rf$pre


# XGBoost
fit_xgboost <- workflow() %>%
  add_model(
    spec = boost_tree(
      mode = "regression"
    ) %>%
      set_engine("xgboost")
  ) %>%
  add_recipe(recipe %>% 
               update_role(Data.Medicao, new_role = "indicator")) %>%
  fit(training(particao))

fit_xgboost
fit_xgboost$pre


# Prophet
fit_prophet <- workflow() %>%
  add_model(
    spec = prophet_reg(
      seasonality_daily  = FALSE, 
      seasonality_weekly = FALSE, 
      seasonality_yearly = TRUE
    ) %>% 
      set_engine("prophet")
  ) %>%
  add_recipe(recipe) %>%
  fit(training(particao))

fit_prophet
fit_prophet$pre


# Prophet Boost
fit_prophet_boost <- workflow() %>%
  add_model(
    spec = prophet_boost(
      seasonality_daily  = FALSE, 
      seasonality_weekly = FALSE, 
      seasonality_yearly = TRUE
    ) %>% 
      set_engine("prophet_xgboost")
  ) %>%
  add_recipe(recipe) %>%
  fit(training(particao))

fit_prophet_boost
fit_prophet_boost$pre


# SVR
fit_svr <- workflow() %>%
  add_model(
    spec = svm_linear(
      mode = "regression"
    ) %>% 
      set_engine("kernlab")
  ) %>%
  add_recipe(recipe %>% 
               update_role(Data.Medicao, new_role = "indicator")) %>%
  fit(training(particao))

fit_svr
fit_svr$pre


# Avaliacao Modelos
Avaliacao_Modelos <- modeltime_table(
  fit_rf,
  fit_xgboost,
  fit_prophet,
  fit_prophet_boost,
  fit_svr
)

Avaliacao_Modelos

# Modelos Ajustados _ Conjunto Treinamento
modelos_ajustados_treinamento <- Avaliacao_Modelos %>%
  modeltime_calibrate(new_data = training(particao))

modelos_ajustados_treinamento

# Avaliacao Modelos_Treinamento
modelos_ajustados_treinamento %>%
  modeltime_accuracy(training(particao)) %>%
  arrange(rmse)


# Modelos Ajustados _ Conjunto Teste
modelos_ajustados <- Avaliacao_Modelos %>%
  modeltime_calibrate(new_data = testing(particao))

modelos_ajustados


# Avaliacao Modelos
modelos_ajustados %>%
  modeltime_accuracy(testing(particao)) %>%
  arrange(rmse)


# Plot dos valores preditos
modelos_ajustados %>%
  modeltime_forecast(
    new_data    = testing(particao),
    actual_data = dados1,
    keep_data   = TRUE 
  ) %>%
  plot_modeltime_forecast(
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )


# Ajuste dos hiperparametros - Usando 
# Rolling Origin Forecast Resampling


# Número maximo de Slices k = 5
set.seed(123)
r_origin<- time_series_cv(data = training(particao),
                          date_var = Data.Medicao,
                          initial     = "4 years",  # Treino
                          assess      = "1 years",  # Teste
                          skip        = "6 months", # Translado da serie
                          cumulative  = FALSE,
                          slice_limit = 5)

r_origin %>%
  tk_time_series_cv_plan()%>%
  plot_time_series_cv_plan(Data.Medicao, TEMP_MEDIA, .interactive = FALSE)


# Random Forest
rf_tune <- rand_forest(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
) %>%
  set_engine("ranger")

wflw_rf_tune <- workflow() %>%
  add_model(rf_tune) %>%
  add_recipe(recipe)
wflw_rf_tune


# Random Forest
recipe %>% 
  update_role(Data.Medicao, new_role = "indicator") %>%
  prep() %>%
  summary() %>% 
  group_by(role) %>% 
  summarise(n=n())

# Busca aleatória de Hiperparametros
set.seed(123)
grid_hiper<- grid_latin_hypercube(
  extract_parameter_set_dials(rf_tune) %>% 
    update(mtry = mtry(range = c(1, 6)),
           trees = trees(range = c(630, 1000)),
           min_n = min_n(range=c(12, 25))),
  size = 35     # Numero máximo de combinações dos hiperparametros
)

resultados_rf<- wflw_rf_tune %>%
  tune_grid(
    resamples  = r_origin,
    grid = grid_hiper,
    control = control_grid(verbose = TRUE, 
                           allow_par = TRUE)
  )


resultados_rf%>% 
  show_best("rmse", n = 2)

# Ajuste do melhor modelo pelo RMSE
set.seed(123)
fit_rf_tuned <- wflw_rf_tune %>%
  finalize_workflow(
    select_best(resultados_rf, "rmse", n=1)) %>%
  fit(training(particao))

modeltime_table(fit_rf_tuned) %>%
  modeltime_calibrate(testing(particao)) %>%
  modeltime_accuracy()


# XGBoost
xgboost_tune <- boost_tree(
  mode = "regression",
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  #  tree_depth = tune(),
  learn_rate = tune(),
  #  loss_reduction = tune(),
  #  sample_size = tune(),
) %>%
  set_engine("xgboost")

wflw_xgboost_tune <- workflow() %>%
  add_model(xgboost_tune) %>%
  add_recipe(recipe %>% 
               update_role(Data.Medicao, new_role = "indicator"))
wflw_xgboost_tune


# Busca aleatória de Hiperparametros
set.seed(123)
grid_hiper<- grid_latin_hypercube(
  extract_parameter_set_dials(xgboost_tune) %>% 
    update(mtry = mtry(range = c(1, 6)),
           learn_rate = learn_rate(range = c(-1.7, -0.58)),
           trees = trees(range = c(800, 1050))),
  size = 35
)


resultados_xgboost<- wflw_xgboost_tune %>%
  tune_grid(
    resamples  = r_origin,
    grid = grid_hiper,
    control = control_grid(verbose = TRUE, 
                           allow_par = TRUE)
  )


resultados_xgboost%>% 
  show_best("rmse", n = 2)


# Ajuste do melhor modelo pelo RMSE
set.seed(123)
fit_xgboost_tuned <- wflw_xgboost_tune %>%
  finalize_workflow(
    select_best(resultados_xgboost, "rmse", n=1)) %>%
  fit(training(particao))

modeltime_table(fit_xgboost_tuned) %>%
  modeltime_calibrate(testing(particao)) %>%
  modeltime_accuracy()


# Prophet
prophet_tune <- prophet_reg(
  mode = "regression",
  seasonality_yearly = TRUE,
  seasonality_weekly = TRUE,
  seasonality_daily = TRUE,
) %>%
  set_engine("prophet")

wflw_prophet_tune <- workflow() %>%
  add_model(prophet_tune) %>%
  add_recipe(recipe)
wflw_prophet_tune

recipe %>% 
  update_role(Data.Medicao, new_role = "indicator") %>%
  prep() %>%
  summary() %>% 
  group_by(role) %>% 
  summarise(n=n())


# Ajuste do melhor modelo pelo RMSE
set.seed(123)
fit_prophet_tuned <- wflw_prophet_tune %>%
  fit(training(particao))

modeltime_table(fit_prophet_tuned) %>%
  modeltime_calibrate(testing(particao)) %>%
  modeltime_accuracy()


# Prophet Boost
prophet_boost_tune <- prophet_boost(
  mode = "regression",
  changepoint_num = tune(),
  seasonality_yearly = TRUE,
  seasonality_weekly = TRUE,
  seasonality_daily = TRUE,
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
) %>%
  set_engine("prophet_xgboost")

wflw_prophet_boost_tune <- workflow() %>%
  add_model(prophet_boost_tune) %>%
  add_recipe(recipe)
wflw_prophet_boost_tune

recipe %>% 
  update_role(Data.Medicao, new_role = "indicator") %>%
  prep() %>%
  summary() %>% 
  group_by(role) %>% 
  summarise(n=n())


# Busca aleatória de Hiperparametros
set.seed(123)
grid_hiper<- grid_latin_hypercube(
  extract_parameter_set_dials(prophet_boost_tune) %>% 
    update(mtry = mtry(range = c(1, 5)),
           learn_rate = learn_rate(range = c(-1.7, -0.58)),
           trees = trees(range = c(600, 1500))),
  size = 5
)


resultados_prophet_boost <- wflw_prophet_boost_tune %>%
  tune_grid(
    resamples  = r_origin,
    grid = grid_hiper,
    control = control_grid(verbose = TRUE, 
                           allow_par = TRUE)
  )

resultados_prophet_boost %>% 
  show_best("rmse", n = 2)



# Ajuste do melhor modelo pelo RMSE
set.seed(123)
fit_prophet_boost_tuned <- wflw_prophet_boost_tune %>%
  finalize_workflow(
    select_best(resultados_prophet_boost, "rmse", n=1)) %>%
  fit(training(particao))

modeltime_table(fit_prophet_boost_tuned) %>%
  modeltime_calibrate(testing(particao)) %>%
  modeltime_accuracy()



# SVR
svr_tune <- svm_linear(
  mode = "regression",
  cost = tune(),
  #  margin = tune(),
) %>%
  set_engine("kernlab")

model_svr_tune <- workflow() %>%
  add_model(svr_tune) %>%
  add_recipe(recipe)
model_svr_tune


recipe %>% 
  update_role(Data.Medicao, new_role = "indicator") %>%
  prep() %>%
  summary() %>% 
  group_by(role) %>% 
  summarise(n=n())

# Busca aleatória de Hiperparametros
set.seed(123)
grid_hiper <- grid_latin_hypercube(
  extract_parameter_set_dials(svr_tune) %>% 
    update(cost = cost(range = c(0.1, 1))),
  size = 5
)

grid_hiper


# SVR - Tune Grid
resultados_svr <- model_svr_tune %>%
  tune_grid(
    resamples  = r_origin,
    grid = grid_hiper,
    control = control_grid(verbose = TRUE, 
                           allow_par = TRUE)
  )


resultados_svr %>% 
  show_best("rmse", n = Inf)



# Ajuste do melhor modelo pelo RMSE
set.seed(123)
fit_svr_tuned <- model_svr_tune %>%
  finalize_workflow(
    select_best(resultados_svr, "rmse", n=1)) %>%
  fit(training(particao))

modeltime_table(fit_svr_tuned) %>%
  modeltime_calibrate(testing(particao)) %>%
  modeltime_accuracy()

recipe %>% 
  update_role(Data.Medicao, new_role = "indicator") %>%
  prep() %>%
  summary() %>% 
  group_by(role) %>% 
  summarise(n=n())

# Resutados finais

# Modelos ajustados sem escolha dos hiperparametros
Avaliacao_Modelos <- modeltime_table(
  fit_rf,
  fit_xgboost,
  fit_prophet,
  fit_prophet_boost,
  fit_svr
)

# Modelos ajustados com escolha dos hiperparametros
modelos_c_selecao <- modeltime_table(
  fit_rf_tuned,
  fit_xgboost_tuned,
  fit_prophet_tuned,
  fit_prophet_boost_tuned,
  fit_svr_tuned,
  fit_ligthgbm_tuned
  
) %>%
  update_model_description(1, "RANGER - Tuned") %>%
  update_model_description(2, "XGBOOST - Tuned") %>%
  update_model_description(3, "PROPHET W/ REGRESSORS - Tuned") %>%
  update_model_description(4, "PROPHET W/ XGBOOST ERRORS - Tuned") %>%
  update_model_description(5, "SVR - Tuned") %>%
  combine_modeltime_tables(Avaliacao_Modelos)

modelos_c_selecao


ajuste_todos_treinamento <- modelos_c_selecao %>%
  modeltime_calibrate(training(particao))

ajuste_todos_treinamento %>% 
  modeltime_accuracy() %>%
  arrange(rmse)

ajuste_todos <- modelos_c_selecao %>%
  modeltime_calibrate(testing(particao))

ajuste_todos %>% 
  modeltime_accuracy() %>%
  arrange(rmse)

#Plot dos valores preditos
ajuste_todos %>%
  modeltime_forecast(
    new_data    = testing(particao),
    actual_data = artifacts$data$dados1,
    keep_data   = TRUE 
  ) %>%
  plot_modeltime_forecast(
    #.facet_ncol         = 4, 
    .conf_interval_show = FALSE,
    .interactive        = TRUE
  )


