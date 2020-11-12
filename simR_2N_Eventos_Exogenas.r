
#set.seed(10)
#calcula o proximo evento
calculaProxEvento <- function(tl, colname, n, k, tt) {
  
	#inicializo uma matriz para inserir os eventos que vão ser gerados para poder pegar o proximo evento, sendo que esse evento é o com menor tempo (por falta de memoria podemos fazer assim)
	prox <- matrix(rep(0,), ncol = length(colname), nrow = n*2)
	colnames(prox) <- colname
	
	#Faço um for para calcular o próximo evento de cada usuário
	#Cada usuario propaga uma FakeNews e uma GoodNews. Se a timeline dele for soh FakeNews(ou soh goodNews) ele propaga a FakeNews(ou a GoodNews) com probabilidade 1.
	
	for(i in 1:n){
	
		#calcula prox FakeNews propagado do usuario i
		vizinho <- sample(1:(n-1), 1) #numero aleatorio entre 1 e K-1, daqui fazemos a conta para saber qual o vizinho que vai receber o post. A distribuição é uniforme.
		if(vizinho == i) {vizinho <- n} #A probabilidade é 1/N-1, porque ele nao tem auto referencia, entao se cair nele, vai ser o N-esimo elemento. #Um pouco de gambiarra, mas funciona
		#popula meus dados
		prox[i,1] <- i #TlSource
		prox[i,2] <- vizinho #TlDestination
		
		if(rowSums(tl)[i] == k){#todos os posts do propagador sao FakeNews, entao ele propaga FN
		  prox[i,3] <- 1
		} else if(rowSums(tl)[i] == 0){#todos os posts do propagador sao GoodNews, entao ele propaga GN
		  prox[i,3] <- 0
		} else {
		  prox[i,3] <- 1 #Type # 0 = GN, 1 = FN #aqui ta prob 1/2, tem que alterar depois, esta assim por questão de testes
		}
		##prox[i,3] <- if(sample(0:4,1) > 3) {0} else {1} #Aqui, por questão de testes tmb, coloquei uma prob de 3/5 para receber GoodNews e 2/5 para FakeNews.
		#prox[i,4] <- 0 #Total_FakeNews, vai ser calculado depois, coloco 0 para não ficar vazio
		prox[i,6] <- rexp(1) #Time #tempo exponencial usando função do próprio R.
		prox[i,5] <- prox[i,6] + tt
		#calcula prox GoodNews propagado do usuario i
		vizinho <- sample(1:(n-1), 1) #numero aleatorio entre 1 e K-1, daqui fazemos a conta para saber qual o vizinho que vai receber o post. A distribuição é uniforme.
		if(vizinho == i) {vizinho <- n} #A probabilidade é 1/N-1, porque ele nao tem auto referencia, entao se cair nele, vai ser o N-esimo elemento. #Um pouco de gambiarra, mas funciona
		#popula meus dados
		prox[n+i,1] <- i #TlSource
		prox[n+i,2] <- vizinho #TlDestination
		
		if(rowSums(tl)[i] == k){#todos os posts do propagador sao FakeNews, entao ele propaga FN
		  prox[n+i,3] <- 1
		} else if(rowSums(tl)[i] == 0){#todos os posts do propagador sao GoodNews, entao ele propaga GN
		  prox[n+i,3] <- 0
		} else {
		  prox[n+i,3] <- 0 #Type # 0 = GN, 1 = FN #aqui ta prob 1/2, tem que alterar depois, esta assim por questão de testes
		}
		##prox[i,3] <- if(sample(0:4,1) > 3) {0} else {1} #Aqui, por questão de testes tmb, coloquei uma prob de 3/5 para receber GoodNews e 2/5 para FakeNews.
		#prox[i,4] <- 0 #Total_FakeNews, vai ser calculado depois, coloco 0 para não ficar vazio
		
		prox[n+i,6] <- rexp(1) #Time #tempo exponencial usando função do próprio R.
		prox[n+i,5] <- prox[n+i,6] + tt
	
	}
	
	#orderna evento pela coluna "Time" em ordem crescente e adiciona esse evento no proximo evento.
	#Esse é o primeiro evento que vai acontecer pois é o com menor tempo.
	prox[order(prox[,ncol(prox)]),][1,]
	#return(prox)
}

calculaGoodNewsExogena <- function(n, tt, colname, lambda){
	prox <- matrix(rep(0,), ncol = length(colname), nrow = n)
	colnames(prox) <- colname
	for(i in 1:n){
		#calcular chegada para todos os usuários, fazer um for aqui
		prox[i,1] <- 0 #TlSource #0 para chegadas exogenas
		prox[i,2] <- i #TlDestination
		prox[i,3] <- 0 #GoodNews
		prox[i,6] <- rexp(1,lambda) + tt#Time #tempo exponencial usando função do próprio R.
		prox[i,5] <- prox[i,6] + tt
	}
	
	prox[order(prox[,ncol(prox)]),][1,]
}

calculaFakeNewsExogena <- function(n, tt, colname, lambda){
	prox <- matrix(rep(0,), ncol = length(colname), nrow = n)
	colnames(prox) <- colname
	for(i in 1:n){
		#calcular chegada para todos os usuários, fazer um for aqui
		prox[i,1] <- 0 #TlSource #0 para chegadas exogenas
		prox[i,2] <- i #TlDestination
		prox[i,3] <- 1 #FakeNews
		prox[i,6] <- rexp(1,lambda) + tt #Time #tempo exponencial usando função do próprio R.
		prox[i,5] <- prox[i,6] + tt
	}
	prox[order(prox[,ncol(prox)]),][1,]
}


#################################
roda_sim <- function() {

	#Cria timeLine com os valores "F" = 1,"G" = 0, podemos modificar do jeito que quisermos
	#GoodNews = 0
	#FakeNews = 1

	#set.seed(10)
	maxLoop <- 200
	#qual valor vai ser inicializado no top das timeLines
	top <- 1 #Vai começar com FakeNews no top da timeline
	#qual valor vai ser inicializado no botton das timeLines
	botton <- 0 #Vai começar com GoodNews no booton da timeline
	#Numero posts na timeline
	K <- 2 #Minha timeline tem 2 posts só
	#Numero de usuarios
	N <- 5 #5 usuários
	#lambidas
	lambda0 <- 0.1 #Lambda do goodNews
	lambda1 <- 0.2 #Lambda do FakeNews

	#nome das colunas do evento, uso para calcular tambem o tamanho da matriz. Esse objeto é apenas um vetor com os nomes das colunas para poder reaproveitar na hora de criar as matrizes
	#evento vai ter: [TlSource, TlDestination, Type, TotalFN, Time] #Tempo precisa ser sempre o ultimo elemento da lista para nao dar problema na function calculaProxEvento
	#TlSource = Quem está enviando o post
	#TlSource = Quem está recebendo o post
	#Type = Se é FakeNews(1) ou GoodNews(0)
	#TotalFN = Total de fakeNews no instante Time
	#Time = tempo que chegou o próximo post na timeline, aqui o tempo não é o total, é relativo, ou seja, é o espaço de tempo entre o ultimo post e a hora desse post
	colname <- c("TlSource", "TlDestination", "Type", "TotalFN", "TotalTime", "RelativeTime")

	#Timeline é criada da seguinte forma: [top, 0, 0, ..., 0, 0, booton] para cada linha, ou seja, se top = 1 e botton = 1, teremos todas as linhas como [1,0,...,0,1], timeline começa com fakenews e termina com fakenews
	timeLine <- matrix(rep(c(top, rep(0,K-2), botton),N), nrow = N, byrow = T)
	colnames(timeLine) <- c("Top", rep("middle", K-2), "Botton")

	#crio minha matriz de eventos
	eventos <- matrix(rep(0,length(colname)), ncol = length(colname))
	eventos[4] <- Reduce(`+`, (colSums(timeLine)))
	colnames(eventos) <- colname

	#cria matriz para receber o proximo evento
	prox_evento <- matrix(rep(0, length(colname)), nrow = 1, ncol = length(colname))
	colnames(prox_evento) <- colname

	#Reduce(`+`, (colSums(timeLine) # Essa conta serve para calcular quantas FakeNews tem, no total, nas timelines
	l<-1
	flag_fake <- 0
	flag_good <- 0
	flag_endogeno <- 0
	totalTime <- 0
	#Meu loop roda 1000 vezes se minha timeLine não estiver totalmente com GoodNews ou totalmente com FakeNews, caso tenham todas as timeLines com FakeNews ou GoodNews, ele para
	#while( (l <= maxLoop) & (Reduce(`+`, (colSums(timeLine))) != 0) &  (Reduce(`+`, (colSums(timeLine))) != K*N) ){
	while( l < maxLoop){
		#uso a function para calcular o proximo evento
		if (flag_endogeno == 0) { 
			prox_evento_endogeno <- calculaProxEvento(timeLine, colname, N, K, totalTime)
			flag_endogeno <- 1
		}
		if(flag_fake == 0) {
			prox_fakeNewsExogena <- calculaFakeNewsExogena(N, totalTime, colname, lambda1) 
			flag_fake <- 1
		}
		if(flag_good == 0) {
			prox_goodNewsExogena <- calculaGoodNewsExogena(N, totalTime, colname, lambda0)
			flag_good <- 1
		}
		
		if (prox_evento_endogeno[5] <= prox_fakeNewsExogena[5] && prox_evento_endogeno[5] <= prox_goodNewsExogena[5])
		{
			prox_evento <- prox_evento_endogeno
			flag_endogeno <- 0
		} else if (prox_fakeNewsExogena[5] <= prox_evento_endogeno[5] && prox_fakeNewsExogena[5] <= prox_goodNewsExogena[5]) {
			prox_evento <- prox_fakeNewsExogena
			flag_fake <- 0
		} else {
			prox_evento <- prox_goodNewsExogena
			flag_good <- 0
		}
		#preciso descobrir quem eh o proximo evento, tenho 3 opções
		
	
		#faço a rotação da timeLine, ou seja, o top recebe o próximo post que pode ser FN ou GN e os outros posts da timeline "descem".
		for(i in K:2){
			timeLine[prox_evento[2], i] <- timeLine[prox_evento[2], i-1] #top vai pra botton
		}
		timeLine[prox_evento[2], 1] <- prox_evento[3] #evento entra no top da timeline
		
		######################################################################
		## RDN Case, mas validar probabilidade se eh linearmente distribuido ##
		######################################################################
		#para RDN podemos escolher aleatoriamente onde a mensagem vai entrar, entao basicamente vamos soh substituir um post aleatoriamente
		#timeLine[prox_evento[2], sample(1:K,1)] <- prox_evento[3] #top vai pra botton
		
		#calcula quantas FakeNews tem no total e adiciono na coluna correta
		prox_evento[4] <- Reduce(`+`, (colSums(timeLine)))
		#adiciona tempo total ate o momento
		#prox_evento[5] <- colSums(eventos)[6]+prox_evento[6]
		totalTime <- prox_evento[5]
		
		#rbind serve para adicionar uma row na matriz, ou seja, estou adicionando o prox_evento na minha matriz de eventos
		eventos <- rbind(eventos, prox_evento)
		
		l <- l+1

	}
	
	return(eventos)
	#minha timeLine após o loop
	#timeLine
	#quantos eventos foram gerados
	#nrow(eventos)
	#plota a quantodade de FakeNews por evento
	#plot(eventos[,4], )
}


total_sim_time <- c()
total_fk_timeline <- c()
total_gn_timeline <- c()

for (i in 1:30){
  #inicializa um vetor vazio para inserir todos os tempos de simulação, para tirarmos a média
  sim_time <- c()
  #inicializa um vetor vazio para inserir todos os estados finais da simulação, para saber qual porcentagem de FakeNews e GoodNews do total das simulações
  sim_TL <- c()
  
  #numSim é o número de simulações que iremos realizar
  numSim <- 100
  #realiza as simulações e insere na lista sim_events
  sim_events <- replicate(numSim,roda_sim())
  
  #Preciso fazer o seguinte, pegar todos os TotalTime que TotalFN == 10, ver como fazer isso
  #Fazer a mesma coisa para TotalFN==0

  #sim_events[,,9]
  
  #Pego ultima linha da simula��o i
  
  for(i in 1:numSim) {
    #insere os dados de tempo de cada simulacao
    sim_TL <- c(sim_TL,(sim_events[nrow(sim_events[,,i]),4,i]))
    #insere os dados de tempo de cada simulacao
    sim_time <- c(sim_time,(sim_events[nrow(sim_events[,,i]),5,i]))
  }
  
  #sumFN <- sum(sim_TL == 10)
  #sumGN <- sum(sim_TL == 0)
  
  #media de tempo
  total_sim_time <- c(total_sim_time,mean(sim_time))
  total_fk_timeline <- c(total_fk_timeline, sum(sim_TL == 10)/length(sim_TL))
  total_gn_timeline <- c(total_gn_timeline, sum(sim_TL == 0)/length(sim_TL))
  #Porcentagem de vezes que teve FakeNews
  
  
  #reseto minha lista de eventos
  sim_events <<- NULL
}


#media com o intervalo de confianca da porcentagem de FakeNews
mean(total_fk_timeline) - (( 1.96*sqrt(sum((total_fk_timeline-mean(total_fk_timeline))^2/(length(total_fk_timeline))))) / sqrt(length(total_fk_timeline)))
mean(total_fk_timeline)
mean(total_fk_timeline) + (( 1.96*sqrt(sum((total_fk_timeline-mean(total_fk_timeline))^2/(length(total_fk_timeline))))) / sqrt(length(total_fk_timeline)))

#media com o intervalo de confianca do tempo para terminar a simulacao
mean(total_sim_time) - (( 1.96*sqrt(sum((total_sim_time-mean(total_sim_time))^2/(length(total_sim_time))))) / sqrt(length(total_sim_time)))
mean(total_sim_time)
mean(total_sim_time) + (( 1.96*sqrt(sum((total_sim_time-mean(total_sim_time))^2/(length(total_sim_time))))) / sqrt(length(total_sim_time)))

#media com o intervalo de confianca da porcentagem de GoodNews
mean(total_gn_timeline) - (( 1.96*sqrt(sum((total_gn_timeline-mean(total_gn_timeline))^2/(length(total_gn_timeline))))) / sqrt(length(total_gn_timeline)))
mean(total_gn_timeline)
mean(total_gn_timeline) + (( 1.96*sqrt(sum((total_gn_timeline-mean(total_gn_timeline))^2/(length(total_gn_timeline))))) / sqrt(length(total_gn_timeline)))


#Porcentagem de vezes que teve FakeNews
sumFN <- sum(sim_TL == 10)
sumGN <- sum(sim_TL == 0)
#% de FakeNews
sumFN/length(sim_TL)
#% de goodNews
sumGN/length(sim_TL)

#plot do primeiro evento (tempo X #FakeNews)
for(i in 1:10) {
  plot(sim_events[,5,i], sim_events[,4,i], type="s", xlab = "Tempo", ylab = "#FakeNews", col="blue")
}
#plot(sim_events[,5,i], sim_events[,4,i], type="l")
#plot(c(1,2,3,4), c(1,2,1,2), type="l")
#sim_events[[1]][1,]
#site para ajudar a configurar o plot do grafico
#https://www.datamentor.io/r-programming/plot-function/
#plot(sim_events[,5], sim_events[,4], type="l", xlab = "Tempo", ylab = "#FakeNews", col="blue")
