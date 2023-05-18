library(igraph)



N<-26

# 1. Создайте кольцевой граф g со случайным числом вершин G_size (от N+10 до (N/10+5)^2+5N). Выведите 
# число ребер и вершин этого графа. Постройте граф, выведите его матрицу смежности.
G_size<-sample((N+10):((N%/%10+5)**2+5*N), 1)
g<-graph.ring(n=G_size)
ecount(g)
vcount(g)
plot(g, edge.arrow.size=.2, vertex.size=13)
g[]

# 2. Создайте граф g1 из пустого графа с числом вершин G_size желтого цвета. Добавьте ему 8N случайных 
# ребер, сформированных из вектора вершин, окрасьте ребра красным цветом, нарисуйте граф и выведите его 
# матрицу смежности. Добавьте графу g1 еще 10N случайных ребер, сформированных из вектора вершин, 
# окрасьте ребра синим цветом, нарисуйте граф и выведите его матрицу смежности.
g1<-graph.empty()+vertices(1:G_size, color='yellow')
g1<-g1+edges(sample(V(g1), 2*8*N, replace=TRUE), color='red')
plot(g1, edge.arrow.size=.2, vertex.size=13)
# g1[]
g1<-g1+edges(sample(V(g1), 2*10*N, replace=TRUE), color='blue')
plot(g1, edge.arrow.size=.2, vertex.size=13)
# g1[]

# 3. Добавьте ребра между вершиной 2N+23 и 2N+20, 2N+12 и N+15, 2N-1 и N+8, 2N и 2N+1, N+7 и N+13, окрасьте 
# их в черный цвет (предварительно проверьте существуют ли такие вершины – функцией %in% либо match, для 
# несуществующих вершин ребра не добавляйте). Нарисуйте граф. Выведите соседей N-й вершины, ребра, 
# инцидентные этой вершине. Соединены ли вершины N+10 и N+12? Выведите матрицу смежности.
v<-c(2*N+23, 2*N+20, 2*N+12, N+15, 2*N-1, N+8, 2*N, 2*N+1, N+7, N+13)
for (i in seq(1, length(v), 2)) {
  if (v[i] %in% V(g1) && v[i+1] %in% V(g1)) {
    g1<-add.edges(g1, c(v[i],v[i+1]), color='black')
  }
}
plot(g1, edge.arrow.size=.2, vertex.size=13)
neighbors(g1, V(g1)[N], mode='out')
incident(g1, V(g1)[N], mode='all')
if ((N+10) %in% V(g1) && (N+12) %in% V(g1)) {
  are.connected(g1, V(g1)[N+10], V(g1)[N+12])
}
# g1[]

# 4. Добавьте еще одну вершину и подключите ее к той, которая имеет наибольшее количество связанных с ней 
# узлов. Присвойте имена всем вершинам (например, буквы в алфавитном порядке – используйте заглавные и, 
# если не хватит, строчные буквы). Выведите матрицу смежности. Выберите вершины, для которых значение 
# связности меньше 5 и больше 2.
x<-length(V(g1))+1
g1<-g1+vertices(x, color='green')
deg<-degree(g1, mode='all')
for (i in which(deg==max(deg))) {
  g1<-g1+edges(c(x,i, i,x), color='green')
}
v<-c(toupper(letters[1:26]), tolower(letters[1:26]))
mn<-min(length(V(g1)), length(v))
g1<-set_vertex_attr(g1, 'name', 1:mn, v[1:mn])
plot(g1, edge.arrow.size=.2, vertex.size1=13)
# g1[]
v<-names(V(g1))
deg<-degree(g1, mode='all')
v[which(deg<5&deg>2)]

# 5. Испробуйте алгоритмы размещения Вашего графа (in_circle, as_tree, lattice). Результаты включить в 
# отчет.
coords<-layout_(g1, in_circle())
plot(g1, layout=coords, edge.arrow.size=.2)
coords<-layout_(g1, as_tree())
plot(g1, layout=coords, edge.arrow.size=.2)

# 6. Выполните измерение диаметра графа g1, выведите список самых коротких путей для каждой вершины и 
# откалибруйте величины вершин согласно их степеней.
diameter(g1)
all_shortest_paths(g1, 1, to=V(g1), mode='all', weights=NULL)
deg<-degree(g1, mode='all')
plot(g1, edge.arrow.size=.2, vertex.size=deg)

g<-graph.lattice(length=100,dim=1,nei=5, circular = TRUE)
plot(g,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai)


#7
#Имеется M (M <= 50) городов, в каждом из которых открылась кооперативная
#парикмахерская. Известна стоимость стрижки в каждом городе и стоимость проезда между
#городами. Известно, что не между всеми городами есть прямая дорога. Стоимость и проезд
#выражаются неотрицательными вещественными числами. Для агентов каждого города
#определить город, куда им следует съездить, чтобы подстричься самым дешевым образом (в                                                                                    своем собственном городе из соображений секретности стричься нельзя). Данные
#сформировать случайным образом.


library(igraph)

# Ввод количества городов
M <- as.integer(readline("Введите количество городов (M): "))

# Создание случайных данных
haircut_costs <- runif(M, 10, 100)  # Стоимость стрижки в каждом городе
travel_costs <- matrix(runif(M^2, 1, 10), nrow = M)  # Стоимость проезда между городами
diag(travel_costs) <- Inf  # Устанавливаем бесконечные стоимости проезда внутри одного города

# Создание графа
g <- graph.adjacency(travel_costs, mode = "undirected", weighted = TRUE)
V(g)$haircut_cost <- haircut_costs

# Визуализация графа
plot(g, edge.arrow.size = 0.2, vertex.size = 13)

# Поиск самого дешевого города для стрижки
cheapest_city <- which.min(V(g)$haircut_cost)
cat("Самый дешевый город для стрижки: Город", cheapest_city, "\n")
cat("Стоимость стрижки в этом городе:", V(g)$haircut_cost[cheapest_city])







