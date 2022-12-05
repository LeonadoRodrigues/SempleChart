#' Função para criar gráficos simples e elegantes
#'
#' Essa função serve para criar gráficos simples e elegantes.
#'
#' @param base   Base de dados
#' @param eixo_x Eixo 'X' do gráfico
#' @param eixo_y Eixo 'Y' do gráfico
#' @param fill   Variável que receberá o preenchimento
#' @param paleta Paleta de cores - o pacote usa a escala de cor 'viridis'
#'
#' @examples
#'
#' @export

library(tidyverse)

SempleBar <- function(base, eixo_x, eixo_y, fill, paleta){
  grafico = ggplot(base, aes(x=eixo_x, y=eixo_y, fill = fill)) +
    geom_bar(stat = "identity", position="dodge") +
    scale_fill_viridis(discrete = TRUE, # Se a escalar for discreta então, TRUE.
                       alpha= 0.6, # Transparência
                       option= paleta, # Opção de paletas de cores
                       begin = 0,  # Onde começar as cores
                       end = 1,) + # Onde terminar
    labs(x = "Legislatura",        # Rótulo eixo X.
         y = "",                   # Rótulo eixo Y.
         fill = "Tipo de Projeto") + # Rórulo em fill.
    scale_y_continuous("", labels = function(x) paste0(x, "%"),
                       limits = c(0, 50)) +
    ggthemes::theme_tufte() +                 # Tema do gráfico.
    theme(legend.position="top") + # Posição da legenda fill - "top", "down", ou "none".
    geom_text(aes(label = ""), # Rótulo que o gráfico irá receber.
              position = position_dodge(0.9), # Posição onde ficará a legenda no gráfico.
              vjust = 1,                      # Ajuste da legenda, se em cima do gráfico, fora...
              size = 3.0,                     # Tamanho da fonte da legenda no gráfico.
              check_overlap = T,              # Permite que a as legendas não se sobreponham, use TRUE.
              color = "White")                # Cor da legenda.
  return(grafico)
}
