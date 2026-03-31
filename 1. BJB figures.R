
#-------------------------------------------------------------------------------
#PRELIMINARES
ls()            #requests all objects in R's brain
rm(list=ls())    #we use two functions at once, rm an ls. rm stands for 

getwd()			    #"get working directory", where R in "currently" loonking...
setwd("C:/Users/Fadini/Documents/0. publicacoes/1. em preparação/1. Dalila_Ds_1_BJB")
getwd()			    #use getwd to confirm that R is now looking here

detach(dados)
rm(dados)


#-------------------------------------------------------------------------------
#Carregando pacotes

if(!require(pacman)){install.packages("pacman")}
pacman::p_load(ggplot2, dplyr, scales, ggpubr, grDevices, ggimage,stringr,
               nlstools,ggsignif,car,table1,ggsave)



################################################################################
#Figure 1

# Data figure 1 ---------------------------------------------------------------
dados <- data.frame(
  
  painel = rep(c("A)", "B)"), each = 3),
  
  linha = rep(3:1, 2),
  
  esquerda = c(10, 9, 9, 10, 10, 11),
  direita  = c(10, 11, 11, 11, 10, 9),
  
  pvalor = c("X2= 0.00, df= 1, p=1.00",
             "X2= 0.20, df= 1, p=0.65",
             "X2= 0.20, df= 1, p=0.65",
             "X2= 0.20, df= 1, p=0.83",
             "X2= 0.00, df= 1, p=1.00",
             "X2= 0.20, df= 1, p=0.65"),
  
  NR = c("NR = 2",
         "NR = 3",
         "NR = 6",
         "NR = 2",
         "NR = 3",
         "NR = 3")
)


# proporções (negativo à esquerda) ---------------------------------------------
dados <- dados %>%
  mutate(
    esq = -esquerda / (esquerda + direita),
    dir =  direita / (esquerda + direita)
  )


# gráfico ----------------------------------------------------------------------
ggplot(dados) +
  
  # barras esquerda ------------------------------------------------------------
  geom_rect(aes(xmin = esq, xmax = 0,
                ymin = linha - 0.3, ymax = linha + 0.3),
            
            fill = "white", color = "black") +
  
  # barras direita -------------------------------------------------------------
  geom_rect(aes(xmin = 0, xmax = dir,
                ymin = linha - 0.3, ymax = linha + 0.3,
                fill = painel),
            color = "black") +
  
  scale_fill_manual(values = c("A)" = "white", "B)" = "green")) +
  
  # linha central --------------------------------------------------------------
  geom_vline(xintercept = 0) +
  
  # linhas pontilhadas ---------------------------------------------------------
  geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed") +
  
  # valores dentro das barras --------------------------------------------------
  #geom_text(aes(x = esq/2, y = linha, label = esquerda), size = 4) +
  #geom_text(aes(x = dir/2, y = linha, label = direita),  size = 4) +

  geom_text(aes(x = -0.1, y = linha, label = esquerda), size = 4) +
  geom_text(aes(x =  0.1, y = linha, label = direita),  size = 4) +
  
  # p-valor --------------------------------------------------------------------
  geom_text(aes(x = -1.0, y = linha, label = pvalor), hjust = 0, size = 3) +
  
  # NR -------------------------------------------------------------------------
  geom_text(aes(x = 0.9, y = linha, label = NR), hjust = 0, size = 3) +
  
  # painéis --------------------------------------------------------------------
  facet_wrap(~painel, ncol = 1, scales = "free_y") +
  
  # eixo x com valores positivos nos dois lados --------------------------------
  scale_x_continuous(
    limits = c(-1, 1),
    breaks = c(-1, -0.5, 0, 0.5, 1),
    labels = function(x) abs(x)
  ) +
  
  # tema -----------------------------------------------------------------------
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    
    # linha do eixo x ----------------------------------------------------------
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    
    # remover eixo y -----------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    # melhorar visual do eixo x ------------------------------------------------
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black")
  ) +
  
  labs(x = "Predatory mite choice proportion")


#-------------------------------------------------------------------------------

ggsave("3. Fig_1.tiff", height = 9, width = 7, units = "in", dpi = 300)
ggsave("3. Fig_1.jpg",  height = 9, width = 7, units = "in", dpi = 300)

################################################################################


################################################################################
#Figure 2 - 30.mar.2026

# data figure 2 ---------------------------------------------------------------
dados <- data.frame(
  
  painel = rep(c("A)", "B)"), each = 3),
  
  linha = rep(3:1, 2),
  
  esquerda = c(14, 16, 17, 16, 16, 15),
  direita  = c( 6,  4,  3,  4,  4,  5),
  
  pvalor = c("X2= 3.20, df= 1, p=0.07",
             "X2= 7.20, df= 1, p=0.01",
             "X2= 9.80, df= 1, p>0.01",
             "X2= 7.20, df= 1, p=0.01",
             "X2= 7.20, df= 1, p=0.01",
             "X2= 5.00, df= 1, p=0.02"),
  
  NR = c("NR = 1",
         "NR = 4",
         "NR = 6",
         "NR = 3",
         "NR = 5",
         "NR = 5")
)


# proporções (negativo à esquerda) ---------------------------------------------
dados <- dados %>%
  mutate(
    esq = -esquerda / (esquerda + direita),
    dir =  direita / (esquerda + direita),
    grupo_esq = ifelse(painel == "A)", "grupo1", "grupo2")
  )


# gráfico ----------------------------------------------------------------------
ggplot(dados) +
  
  # barras esquerda (duas cores) -----------------------------------------------
  geom_rect(aes(xmin = esq, xmax = 0,
                ymin = linha - 0.3, ymax = linha + 0.3,
                fill = grupo_esq),
            color = "black") +
  
  # barras direita (mesma cor)--------------------------------------------------
  geom_rect(aes(xmin = 0, xmax = dir,
                ymin = linha - 0.3, ymax = linha + 0.3),
            fill = "grey50", color = "black") +
  
  # cores das barras esquerda --------------------------------------------------
  scale_fill_manual(values = c("grupo1" = "#FFD39B",
                               "grupo2" = "#98F5FF")) +
  
  # linha central --------------------------------------------------------------
geom_vline(xintercept = 0) +
  
  # linhas pontilhadas ---------------------------------------------------------
geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed") +
  
  # valores dentro das barras --------------------------------------------------
#geom_text(aes(x = esq/2, y = linha, label = esquerda), size = 4) +
#geom_text(aes(x = dir/2, y = linha, label = direita),  size = 4) +
  
geom_text(aes(x = -0.1, y = linha, label = esquerda), size = 4) +
geom_text(aes(x =  0.1, y = linha, label = direita),  size = 4) +


  # p-valor --------------------------------------------------------------------
geom_text(aes(x = -1.3, y = linha, label = pvalor), hjust = 0, size = 3) +
  
  # NR -------------------------------------------------------------------------
geom_text(aes(x = 0.9, y = linha, label = NR), hjust = 0, size = 3) +
  
  # painéis --------------------------------------------------------------------
facet_wrap(~painel, ncol = 1, scales = "free_y") +
  
  # eixo x com valores positivos nos dois lados --------------------------------
scale_x_continuous(
  limits = c(-1.3, 1),
  breaks = c(-1, -0.5, 0, 0.5, 1),
  labels = function(x) abs(x)
) +
  
  # tema -----------------------------------------------------------------------
theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    
    # linha do eixo x ----------------------------------------------------------
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    
    # remover eixo y -----------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    # melhorar visual do eixo x ------------------------------------------------
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black")
  ) +
  
  labs(x = "Predatory mite choice proportion")


#-------------------------------------------------------------------------------

ggsave("3. Fig_2.tiff", height = 9, width = 7.5, units = "in", dpi = 300)
ggsave("3. Fig_2.jpg",  height = 9, width = 7.5, units = "in", dpi = 300)

################################################################################



################################################################################
#Figure 3 - 30.mar.2026

# data figure 3 ---------------------------------------------------------------
dados <- data.frame(
  
  painel = rep(c("A)", "B)"), each = 3),
  
  linha = rep(3:1, 2),
  
  esquerda = c(16, 18, 17, 17, 14, 14),
  direita  = c( 4,  2,  3,  3,  6,  6),
  
  pvalor = c("X2=  7.20, df= 1, p=0.01",
             "X2= 12.20, df= 1, p>0.01",
             "X2=  9.80, df= 1, p>0.01",
             "X2=  9.80, df= 1, p>0.01",
             "X2=  3.20, df= 1, p=0.07",
             "X2=  3.20, df= 1, p=0.07"),
  
  NR = c("NR = 5",
         "NR = 3",
         "NR = 2",
         "NR = 5",
         "NR = 6",
         "NR = 5")
)


# proporções (negativo à esquerda) ---------------------------------------------
dados <- dados %>%
  mutate(
    esq = -esquerda / (esquerda + direita),
    dir =  direita / (esquerda + direita),
    grupo_esq = ifelse(painel == "A)", "grupo1", "grupo2")
  )


# gráfico ----------------------------------------------------------------------
ggplot(dados) +
  
  # barras esquerda (duas cores) -----------------------------------------------
geom_rect(aes(xmin = esq, xmax = 0,
              ymin = linha - 0.3, ymax = linha + 0.3,
              fill = grupo_esq),
          color = "black") +
  
  # barras direita (mesma cor)--------------------------------------------------
geom_rect(aes(xmin = 0, xmax = dir,
              ymin = linha - 0.3, ymax = linha + 0.3),
          fill = "grey50", color = "black") +
  
  # cores das barras esquerda --------------------------------------------------
scale_fill_manual(values = c("grupo1" = "#A2CD5A",
                             "grupo2" = "#CD6600")) +
  
  # linha central --------------------------------------------------------------
geom_vline(xintercept = 0) +
  
  # linhas pontilhadas ---------------------------------------------------------
geom_vline(xintercept = c(-0.5, 0.5), linetype = "dashed") +
  
  # valores dentro das barras --------------------------------------------------
#geom_text(aes(x = esq/2, y = linha, label = esquerda), size = 4) +
#geom_text(aes(x = dir/2, y = linha, label = direita),  size = 4) +

  geom_text(aes(x =  -0.1, y = linha, label = esquerda), size = 4) +
  geom_text(aes(x =  0.05, y = linha, label = direita),  size = 4) +
  
  
  # p-valor --------------------------------------------------------------------
geom_text(aes(x = -1.3, y = linha, label = pvalor), hjust = 0, size = 3) +
  
  # NR -------------------------------------------------------------------------
geom_text(aes(x = 0.9, y = linha, label = NR), hjust = 0, size = 3) +
  
  # painéis --------------------------------------------------------------------
facet_wrap(~painel, ncol = 1, scales = "free_y") +
  
  # eixo x com valores positivos nos dois lados --------------------------------
scale_x_continuous(
  limits = c(-1.3, 1),
  breaks = c(-1, -0.5, 0, 0.5, 1),
  labels = function(x) abs(x)
) +
  
  # tema -----------------------------------------------------------------------
theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    
    # linha do eixo x ----------------------------------------------------------
    axis.line.x = element_line(color = "black", linewidth = 0.8),
    
    # remover eixo y -----------------------------------------------------------
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    
    # melhorar visual do eixo x ------------------------------------------------
    axis.text.x = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black")
  ) +
  
  labs(x = "Predatory mite choice proportion")


#-------------------------------------------------------------------------------

ggsave("3. Fig_3.tiff", height = 9, width = 7.5, units = "in", dpi = 300)
ggsave("3. Fig_3.jpg",  height = 9, width = 7.5, units = "in", dpi = 300)

################################################################################



