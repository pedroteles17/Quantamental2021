# Códigos

Os códigos utilizados no desafio são importados. O número no início do nome do arquivo indica a sequência na qual o código deve ser rodado. Um código possui o número 99 porque ele não é um script e sim, apenas um código com funções que serão importadas para o script 3.

## 1 - Markov

Esse arquivo recebe uma base de dados com o preço de diversos índices e gera os pesos de cada mercado dentro do nosso portfólio.

## 2 - Otim HRP

Realiza a otimização do período de estimação da matriz de covariância. 

Utiliza uma df com o retorno diário de mais de 500 ativos e contrói 1000 portfólios diferentes com 100 ativos cada. Ao final, comparamos o Índice Sharpe de cada combinação período de estimação da volatilidade e período de estimação das correlações. 

Resultado é um gráfico heatmap.

Código bem pesado e pode levar até um dia para rodar.

## 3 - Backtest AR1

Código que realiza o backtest da nossa estratégia para cada um dos países em nossa amostra. 

## 4 Resultado Final

Com base nos outputs do arquivo 1 e 3, bem como outras bases de dados, construímos o portfólio final (que incorpora variação cambial) e calculamos algumas estatísticas dos retornos.
