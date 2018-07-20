#!/usr/bin/env Rscript

#função recebe como entrada matriz de atributos, vetor de labels e número de épocas
#os pesos são atualizados a cada iteração
perceptron <- function(x, classes, n_epocas) {

	#inicializa vetor de pesos e erros
	pesos <- rep(0, dim(x)[2] + 1)
	erros <- rep(0, n_epocas)

	#inicializa vetor com labels das classes, todas iguais a 1
	y <- rep(1, dim(x)[1])

	#inicializa vetor das previsões para as labels
	prev_y <- rep(0, dim(x)[1])

	#se classe for homem então valor = -1
	#se classe for mulher então valor = 1
	y[classes[] == 0] <- -1

	for (i in 1:n_epocas) {

		for (j in 1:dim(x)[1]) {

			#usa função de ativação Heaviside para fazer previsão da label
			z <- sum(pesos[2:length(pesos)] * as.numeric(x[j, ])) + pesos[1]
			if(z < 0) {
				prev_y[j] <- -1
			} else {
				prev_y[j] <- 1
			}

			#atualiza os pesos
			dif_pesos <- (y[j] - prev_y[j]) * c(1, as.numeric(x[j, ]))
			pesos <- pesos + dif_pesos

			#atualiza os erros
			if ((y[j] - prev_y[j]) != 0.0) {
				erros[i] <- erros[i] + 1
			}
		}

	}

	resultado <- list(y, prev_y, erros, pesos)
	return(resultado)
}


#função que gera o tamanho de samples para validação cruzada k-fold num dataset de tamanho n
kfoldTams <- function(n, k){

	#inicializa vetor que guardará os tamanhos de cada sample
	tams <- c()

	for(i in 1:k){
		primeiro <- 1 + (((i - 1)*n) %/% k)
		ultimo <- (i*n) %/% k
		tams <- append(tams, ultimo - primeiro + 1)
	}
	return(tams)
}


#função que gera os índices dos samples para validação cruzada k-fold num dataset de tamanho n
kfoldTeste <- function(n, k){

	#inicializa lista que guardará índices de samples aleatórios de observações
	indices <- list()

	tams <- kfoldTams(n, k)

	#incializa variável que guardará range de valores que poderão ser escolhidos para as observações
	valores <- 1:n

	for(i in 1:k){

		#pega um sample aleatório de tams[i] valores
		s <- sample(valores, tams[i])

		#adiciona esses valores, que correspondem aos índices das observações, à lista de índices
		indices[[i]] <- s

		#retira os índices aleatóriamente pegos do range de valores possíveis
		valores <- setdiff(valores, s)
	}
	return(indices)
}

classificadorPerceptron <- function(treino, teste, classe, n_epocas){
	resultado <- perceptron(treino[, -classe], treino[, classe], n_epocas)
	print(resultado)
	return(resultado)
}


#dados é o data frame, onde cada coluna é um atributo e cada linha é uma observação, classe é índice da coluna do atributo a ser previsto
#k é a quantidade de folds que serão criados para validação cruzada k-fold
kfoldClassificador <- function(dados, classe, k, n_epocas){

	#inicializa lista que guardará resultados da classificação em cada uma das k vezes que é rodado
	resultado <- list()

	todos_indices_teste <- kfoldTeste(dim(dados)[1], k)
	for(i in 1:k){
		indices_teste <- todos_indices_teste[[i]]
		treino <- dados[-indices_teste, ]
		teste <- dados[indices_teste, ]
		resultado[[i]] <- classificadorPerceptron(treino, teste, classe, n_epocas)
	}
	return(resultado)
}


#define salary como dataset a ser analisado
salary <- read.table("salary.csv", header=TRUE, sep=",")

#define matriz de atributos, pega as observações de todas as variáveis menos da classe
x <- salary[, 1:5]

#define vetor de classes
classes <- salary[, 6]

#define coluna referente à classe
classe <- 6

resultado <- kfoldClassificador(salary, classe, 5, 50)
