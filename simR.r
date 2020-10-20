
#Cria timeLine com os valores "F" = 1,"G" = 0, podemos modificar do jeito que quisermos
#GoodNews = 0
#FakeNews = 1

maxLoop <- 1000
#qual valor vai ser inicializado no top das timeLines
top <- 1 #Vai começar com FakeNews no top da timeline
#qual valor vai ser inicializado no botton das timeLines
botton <- 0 #Vai começar com GoodNews no booton da timeline
#Numero posts na timeline
K <- 2 #Minha timeline tem 2 posts só
#Numero de usuarios
N <- 5 #5 usuários
#lambidas
lambda0 <- 0 
lambda1 <- 0 

#nome das colunas do evento, uso para calcular tambem o tamanho da matriz. Esse objeto é apenas um vetor com os nomes das colunas para poder reaproveitar na hora de criar as matrizes
#evento vai ter: [TlSource, TlDestination, Type, TotalFN, Time] #Tempo precisa ser sempre o ultimo elemento da lista para nao dar problema na function calculaProxEvento
#TlSource = Quem está enviando o post
#TlSource = Quem está recebendo o post
#Type = Se é FakeNews(1) ou GoodNews(0)
#TotalFN = Total de fakeNews no instante Time
#Time = tempo que chegou o próximo post na timeline, aqui o tempo não é o total, é relativo, ou seja, é o espaço de tempo entre o ultimo post e a hora desse post
colname <- c("TlSource", "TlDestination", "Type", "TotalFN", "Time")

#crio as timelines para cada usuario
timeLine <- matrix(rep_len(c(top,botton), K*N), nrow = N, byrow = T)
colnames(timeLine) <- c("Top", "Botton")

#crio minha matriz de eventos
eventos <- matrix(rep(0,length(colname)), ncol = length(colname))
colnames(eventos) <- colname

#cria matriz para receber o proximo evento
prox_evento <- matrix(rep(0, length(colname)), nrow = 1, ncol = length(colname))
colnames(prox_evento) <- colname


#calcula o proximo evento
calculaProxEvento <- function() {
	#inicializo uma matriz para inserir os eventos que vão ser gerados para poder pegar o proximo evento, sendo que esse evento é o com menor tempo (por falta de memoria podemos fazer assim)
	prox <- matrix(rep(0,), ncol = length(colname), nrow = N)
	colnames(prox) <- colname
	
	#Faço um for para calcular o próximo evento de cada usuário
	for(i in 1:N){
		vizinho <- sample(1:(N-1), 1) #numero aleatorio entre 1 e K-1, daqui fazemos a conta para saber qual o vizinho que vai receber o post. A distribuição é uniforme.
		if(vizinho == i) {vizinho <- N} #A probabilidade é 1/N-1, porque ele nao tem auto referencia, entao se cair nele, vai ser o N-esimo elemento. #Um pouco de gambiarra, mas funciona
		#popula meus dados
		prox[i,1] <- i #TlSource
		prox[i,2] <- vizinho #TlDestination
		prox[i,3] <- sample(0:1,1) #Type # 0 = GN, 1 = FN #aqui ta prob 1/2, tem que alterar depois, esta assim por questão de testes
		##prox[i,3] <- if(sample(0:4,1) > 3) {0} else {1} #Aqui, por questão de testes tmb, coloquei uma prob de 3/5 para receber GoodNews e 2/5 para FakeNews.
		prox[i,4] <- 0 #Total_FakeNews, vai ser calculado depois, coloco 0 para não ficar vazio
		prox[i,5] <- rexp(1,1) #Time #tempo exponencial usando função do próprio R.
	}
	
	#orderna evento pela coluna "Time" em ordem crescente e adiciona esse evento no proximo evento.
	#Esse é o primeiro evento que vai acontecer pois é o com menor tempo.
	prox[order(prox[,ncol(prox)]),][1,]
}


#################################

#Reduce(`+`, (colSums(timeLine) # Essa conta serve para calcular quantas FakeNews tem, no total, nas timelines
l<-0
#Meu loop roda 1000 vezes se minha timeLine não estiver totalmente com GoodNews ou totalmente com FakeNews, caso tenham todas as timeLines com FakeNews ou GoodNews, ele para
while( (l <= maxLoop) & (Reduce(`+`, (colSums(timeLine))) != 0) &  (Reduce(`+`, (colSums(timeLine))) != K*N) ){
	#uso a function para calcular o proximo evento
	prox_evento <- calculaProxEvento()

	#faço a rotação da timeLine, ou seja, o que está no top vai pro botton e o top recebe o próximo post que pode ser FN ou GN
	timeLine[prox_evento[2], 2] <- timeLine[prox_evento[2], 1] #top vai pra botton
	timeLine[prox_evento[2], 1] <- prox_evento[3] #evento entra no top da timeline
	
	#calcula quantas FakeNews tem no total e adiciono na coluna correta
	prox_evento[4] <- Reduce(`+`, (colSums(timeLine)))
	
	#rbind serve para adicionar uma row na matriz, ou seja, estou adicionando o prox_evento na minha matriz de eventos
	eventos <- rbind(eventos, prox_evento)
	
	l <- l+1

}
#minha timeLine após o loop
timeLine
#quantos eventos foram gerados
nrow(eventos)