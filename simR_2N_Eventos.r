
#todos os dados foram gerados com seed(1) para validar os resultados.
set.seed(1)

#####################
# Variaveis globais #
#####################
#Variavel do MaxLoop
MaxLoop_G <- 10000
#Variavel Top_V
Top_G <- 0
#Variavel Bot_V
Bot_G <- 0
#Variavel K
K_G <- 4
#Variavel N
N_G <- 5
#Variavel isRandom, = 1 se TimeLine random, isRandom = 2 se 1 user com 1 fakeNews no TOP , = 0 caso contrario
isRandom_G <- 2
#Variavel FIFO = 1, RDN = 0
isFIFO_G <- 0





#calcula o proximo evento
calculaProxEvento <- function(tl, colname, n, k, tt) {
  
	#inicializo uma matriz para inserir os eventos que vao ser gerados para poder pegar o proximo evento, 
	#sendo que esse evento eh o com menor tempo (por falta de memoria podemos fazer assim)
	prox <- matrix(rep(0,), ncol = length(colname), nrow = n*2)
	colnames(prox) <- colname
	
	#Faco um for para calcular o proximo evento de cada usuario
	#Cada usuario propaga uma FakeNews e uma GoodNews. 
	#Se a timeline dele for soh FakeNews(ou soh goodNews) 
	#ele propaga a FakeNews(ou a GoodNews) com probabilidade 1.
	
	for(i in 1:n){
	
	#calcula prox FakeNews propagado do usuario i
	#numero aleatorio entre 1 e K-1, 
	#daqui fazemos a conta para saber qual o vizinho que vai receber o post.
	#A probabilidade para cair em cada vizinho eh 1/(N-1), porque ele nao tem "auto referencia",
	#entao se cair nele, vai ser o N-esimo elemento.
	#Um pouco de gambiarra, mas funciona bem
	  vizinho <- sample(1:(n-1), 1) 
	  if(vizinho == i) {vizinho <- n} 
		
	  #popula as colunas da matriz pra linha do usuario i
		prox[i,1] <- i #TlSource
		prox[i,2] <- vizinho #TlDestination
		
		#todos os posts do propagador sao FakeNews, entao ele propaga FN
		if(rowSums(tl)[i] == k){
		  prox[i,3] <- 1
		  #todos os posts do propagador sao GoodNews, entao ele propaga GN
		} else if(rowSums(tl)[i] == 0){
		  prox[i,3] <- 0
		} else {
		  prox[i,3] <- 1 #Estamos propagando uma FakeNews
		}
		
		#tempo exponencial usando funcao do proprio R.
		prox[i,6] <- rexp(1)
		#Tempo total calculado somando o tempo total com o tempo exponecial calculado agora
		prox[i,5] <- prox[i,6] + tt
		
		#calcula prox GoodNews propagado do usuario i
		#mesma logica usada acima
		vizinho <- sample(1:(n-1), 1)
		if(vizinho == i) {vizinho <- n} 
		
		prox[n+i,1] <- i #TlSource
		prox[n+i,2] <- vizinho #TlDestination
		
		#todos os posts do propagador sao FakeNews, entao ele propaga FN
		if(rowSums(tl)[i] == k){
		  prox[n+i,3] <- 1
		#todos os posts do propagador sao GoodNews, entao ele propaga GN
		} else if(rowSums(tl)[i] == 0){
		  prox[n+i,3] <- 0
		} else {
		  prox[n+i,3] <- 0 #Estou propagando oa GoodNews
		}
		
		prox[n+i,6] <- rexp(1)
		prox[n+i,5] <- prox[n+i,6] + tt
	
	}
	
	#orderna evento pela coluna "Time" em ordem crescente e adiciona esse evento no proximo evento.
	#Esse eh o primeiro evento que vai acontecer pois eh o com menor tempo.
	prox[order(prox[,ncol(prox)-1]),][1,]
	
}


#################################
roda_sim <- function(maxLoop_v, top_v, botton_v, K_v, N_v, round_v, isRandom, isFIFO) {
	if(missing(isRandom)) {
        isRandom <- 0
	}
  
	#Cria timeLine com os valores "F" = 1,"G" = 0, podemos modificar do jeito que quisermos
	#GoodNews = 0
	#FakeNews = 1

  #numero maximo de loop do meu loop principal
	maxLoop <- maxLoop_v
	#qual valor vai ser inicializado no top das timeLines
	top <- top_v
	#qual valor vai ser inicializado no botton das timeLines
	botton <- botton_v
	#Numero posts na timeline
	K <- K_v
	#Numero de usuarios
	N <- N_v

	 #numero da rodada da simulacao
	rodada <- round_v
	
	#nome das colunas do evento, uso para calcular tambem o tamanho da matriz. 
	#Esse objeto eh apenas um vetor com os nomes das colunas para poder reaproveitar na hora de criar as matrizes
	#evento vai ter: [TlSource, TlDestination, Type, TotalFN, Time, RelativeTime, Round] 
	#TlSource = Quem esta enviando o post
	#TlDestination = Quem esta recebendo o post
	#Type = Se eh FakeNews(1) ou GoodNews(0)
	#TotalFN = Total de fakeNews no instante Time
	#RelativeTime = tempo que chegou o proximo post na timeline, aqui o tempo nao eh o total, eh o relativo, ou seja, eh o espaco de tempo entre o ultimo post e a hora desse post.
	#TotalTime = Tempo total da simulação ate o momento
	#Round = Rodada da simulacao
	colname <- c("TlSource", "TlDestination", "Type", "TotalFN", "TotalTime", "RelativeTime", "Round")

	#Timeline eh criada da seguinte forma: [top, 0, 0, ..., 0, 0, bottom] para cada linha
	#, ou seja, SE top = 1 e botton = 1, teremos todas as linhas
	#como [1,0,...,0,1], timeline comecaa com fakenews e termina com fakenews
	#Caso o parametro isRandom for = 1, então a minha timeline inicializa totalmente aleatória
	if (isRandom == 1) {
		timeLine <- matrix(sample(0:1,N*K, replace = TRUE), nrow = N, byrow = T)
		colnames(timeLine) <- c("Top", rep("middle", K-2), "Botton")
	}
	else if (isRandom == 2){
	  timeLine <- matrix(c(1, rep(0,(N*K)-1)), nrow = N, byrow = T)
	}
	else {
		timeLine <- matrix(rep(c(top, rep(0,K-2), botton),N), nrow = N, byrow = T)
		colnames(timeLine) <- c("Top", rep("middle", K-2), "Botton")		
	}
	
	#crio minha matriz de eventos vazia, apenas com o numero de FakeNews Total e o numero da rodada
	eventos <- matrix(c(rep(0,length(colname)-1),rodada), ncol = length(colname))
	eventos[4] <- Reduce(`+`, (colSums(timeLine)))
	colnames(eventos) <- colname

	#cria matriz para receber o proximo evento
	#essa matriz vai receber o valor da funcao calculaProxEvento
	prox_evento <- matrix(rep(0, length(colname)), nrow = 1, ncol = length(colname))
	colnames(prox_evento) <- colname

	#contador do loop
	l<-0
	#Tempo total, sempre comeca em zero.
	total_time <- 0
	
	#Reduce(`+`, (colSums(timeLine) # Essa conta serve para calcular quantas FakeNews tem, no total, nas timelines
	#Meu loop roda "maxLoop" vezes ou para se minha timeLine estiver totalmente com GoodNews ou totalmente com FakeNews
	while( (l <= maxLoop) & (Reduce(`+`, (colSums(timeLine))) != 0) &  (Reduce(`+`, (colSums(timeLine))) != K*N) ){
		
	  #uso a function para calcular o proximo evento
		prox_evento <- calculaProxEvento(timeLine, colname, N, K, total_time)

		if(isFIFO == 1){
		  #faco um "shift down" da timeLine, ou seja, o top recebe o proximo post que pode ser FN ou GN e os outros posts da timeline "descem".
		  for(i in K:2){
		    timeLine[prox_evento[2], i] <- timeLine[prox_evento[2], i-1] #top vai pra botton
		  }
		  timeLine[prox_evento[2], 1] <- prox_evento[3] #evento entra no top da timeline
		}
		else{
		  #para RDN podemos escolher aleatoriamente onde a mensagem vai entrar,
		  #entao basicamente vamos soh substituir um post aleatoriamente
		  timeLine[prox_evento[2], sample(1:K,1)] <- prox_evento[3] #top vai pra botton
		}
		
		
		#calcula quantas FakeNews tem no total e adiciono na coluna correta
		prox_evento[4] <- Reduce(`+`, (colSums(timeLine)))
		prox_evento[7] <- rodada
		
		#adiciona tempo total ate o momento na minha variavel
		total_time <- prox_evento[5]
		
		#rbind serve para adicionar uma row na matriz, ou seja, 
		#estou adicionando o prox_evento na minha matriz de eventos
		eventos <- rbind(eventos, prox_evento)
		
		l <- l+1

	}
	
	return(eventos)
}

#lista com todos os eventos de todas as rodadas
#descomentar caso queira gerar um arquivo de output
#sim_full_list <- vector(mode = "list")

#inicializar variavel que vai conter o tempo total de cada simulacao
total_sim_time <- c()
#inicializar variavel que vai conter a porcentagem de FakeNews que terminam em cada simualacao
total_fk_timeline <- c()

#FOR para cada rodada, vamos ter sempre 30 rodadas
for (i in 1:30){
  #inicializa um vetor vazio para inserir todos os tempos de simulacao das rodadas, para tirarmos a media
  sim_time <- c()
  #inicializa um vetor vazio para inserir todos os estados finais (timeLine) de simulacao
  sim_TL <- c()
  
  #numSim eh o numero de vezes que vamos rodar a simulacao para gerar uma media
  numSim <- 100
  #realiza as simulacoes e insere na lista sim_events
  sim_events <- replicate(numSim,roda_sim(MaxLoop_G, Top_G, Bot_G, K_G, N_G, i, isRandom_G, isFIFO_G))
  
  for(i in 1:numSim) {
    #insere os dados de tempo de cada simulacao
    sim_time <- c(sim_time,(sim_events[[i]][nrow(sim_events[[i]]),5]))
    #insere o estado final da time line de cada simulacao
    sim_TL <- c(sim_TL,(sim_events[[i]][nrow(sim_events[[i]]),4]))
  }

  #Calcula media de tempo
  total_sim_time <- c(total_sim_time,mean(sim_time))
  #Calcula % de vezes que a TimeLine terminou com All FakeNews
  total_fk_timeline <- c(total_fk_timeline, sum(sim_TL == (K_G*N_G))/length(sim_TL))
  
  
  #Caso necessario gerar output
  #adiciona as rodadas atuais na minha lista total de eventos
  #sim_full_list <- c(sim_full_list, sim_events)
  #reseto minha lista de eventos
  sim_events <<- NULL
  
}

#media com o intervalo de confianca da porcentagem de FakeNews
mean(total_fk_timeline) - (( 1.96*sqrt(sum((total_fk_timeline-mean(total_fk_timeline))^2/(length(total_fk_timeline))))) / sqrt(length(total_fk_timeline)))
mean(total_fk_timeline)
mean(total_fk_timeline) + (( 1.96*sqrt(sum((total_fk_timeline-mean(total_fk_timeline))^2/(length(total_fk_timeline))))) / sqrt(length(total_fk_timeline)))

#Pegar media de cada simulacao que acabou com GoodNews
total_gn_timeline <- 1-total_fk_timeline

#media com o intervalo de confianca da porcentagem de GoodNews
mean(total_gn_timeline) - (( 1.96*sqrt(sum((total_gn_timeline-mean(total_gn_timeline))^2/(length(total_gn_timeline))))) / sqrt(length(total_gn_timeline)))
mean(total_gn_timeline)
mean(total_gn_timeline) + (( 1.96*sqrt(sum((total_gn_timeline-mean(total_gn_timeline))^2/(length(total_gn_timeline))))) / sqrt(length(total_gn_timeline)))

#media com o intervalo de confianca do tempo para terminar a simulacao
mean(total_sim_time) - (( 1.96*sqrt(sum((total_sim_time-mean(total_sim_time))^2/(length(total_sim_time))))) / sqrt(length(total_sim_time)))
mean(total_sim_time)
mean(total_sim_time) + (( 1.96*sqrt(sum((total_sim_time-mean(total_sim_time))^2/(length(total_sim_time))))) / sqrt(length(total_sim_time)))


#Para gerar um arquivo csv como output
#for(i in 1:length(sim_full_list)){
#  write.table(sim_full_list[[i]], file = "D:\\Alexandre\\Alexandre\\UFRJ\\AD\\202001\\Simulação\\endogenas_RDN_Top1_Bot0.csv", sep = ",", append = TRUE, quote = FALSE,col.names = FALSE, row.names = FALSE)
#}


