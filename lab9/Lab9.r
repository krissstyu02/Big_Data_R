library(igraph)



N<-26

# 1. �������� ��������� ���� g �� ��������� ������ ������ G_size (�� N+10 �� (N/10+5)^2+5N). �������� 
# ����� ����� � ������ ����� �����. ��������� ����, �������� ��� ������� ���������.
G_size<-sample((N+10):((N%/%10+5)**2+5*N), 1)
g<-graph.ring(n=G_size)
ecount(g)
vcount(g)
plot(g, edge.arrow.size=.2, vertex.size=13)
g[]

# 2. �������� ���� g1 �� ������� ����� � ������ ������ G_size ������� �����. �������� ��� 8N ��������� 
# �����, �������������� �� ������� ������, �������� ����� ������� ������, ��������� ���� � �������� ��� 
# ������� ���������. �������� ����� g1 ��� 10N ��������� �����, �������������� �� ������� ������, 
# �������� ����� ����� ������, ��������� ���� � �������� ��� ������� ���������.
g1<-graph.empty()+vertices(1:G_size, color='yellow')
g1<-g1+edges(sample(V(g1), 2*8*N, replace=TRUE), color='red')
plot(g1, edge.arrow.size=.2, vertex.size=13)
# g1[]
g1<-g1+edges(sample(V(g1), 2*10*N, replace=TRUE), color='blue')
plot(g1, edge.arrow.size=.2, vertex.size=13)
# g1[]

# 3. �������� ����� ����� �������� 2N+23 � 2N+20, 2N+12 � N+15, 2N-1 � N+8, 2N � 2N+1, N+7 � N+13, �������� 
# �� � ������ ���� (�������������� ��������� ���������� �� ����� ������� � �������� %in% ���� match, ��� 
# �������������� ������ ����� �� ����������). ��������� ����. �������� ������� N-� �������, �����, 
# ����������� ���� �������. ��������� �� ������� N+10 � N+12? �������� ������� ���������.
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

# 4. �������� ��� ���� ������� � ���������� �� � ���, ������� ����� ���������� ���������� ��������� � ��� 
# �����. ��������� ����� ���� �������� (��������, ����� � ���������� ������� � ����������� ��������� �, 
# ���� �� ������, �������� �����). �������� ������� ���������. �������� �������, ��� ������� �������� 
# ��������� ������ 5 � ������ 2.
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

# 5. ���������� ��������� ���������� ������ ����� (in_circle, as_tree, lattice). ���������� �������� � 
# �����.
coords<-layout_(g1, in_circle())
plot(g1, layout=coords, edge.arrow.size=.2)
coords<-layout_(g1, as_tree())
plot(g1, layout=coords, edge.arrow.size=.2)

# 6. ��������� ��������� �������� ����� g1, �������� ������ ����� �������� ����� ��� ������ ������� � 
# ������������ �������� ������ �������� �� ��������.
diameter(g1)
all_shortest_paths(g1, 1, to=V(g1), mode='all', weights=NULL)
deg<-degree(g1, mode='all')
plot(g1, edge.arrow.size=.2, vertex.size=deg)

g<-graph.lattice(length=100,dim=1,nei=5, circular = TRUE)
plot(g,vertex.size=2,vertex.label=NA,layout=layout.kamada.kawai)


#7
#������� M (M <= 50) �������, � ������ �� ������� ��������� �������������
#��������������. �������� ��������� ������� � ������ ������ � ��������� ������� �����
#��������. ��������, ��� �� ����� ����� �������� ���� ������ ������. ��������� � ������
#���������� ���������������� ������������� �������. ��� ������� ������� ������
#���������� �����, ���� �� ������� ��������, ����� ����������� ����� ������� ������� (�                                                                                    ����� ����������� ������ �� ����������� ����������� �������� ������). ������
#������������ ��������� �������.


library(igraph)

# ���� ���������� �������
M <- as.integer(readline("������� ���������� ������� (M): "))

# �������� ��������� ������
haircut_costs <- runif(M, 10, 100)  # ��������� ������� � ������ ������
travel_costs <- matrix(runif(M^2, 1, 10), nrow = M)  # ��������� ������� ����� ��������
diag(travel_costs) <- Inf  # ������������� ����������� ��������� ������� ������ ������ ������

# �������� �����
g <- graph.adjacency(travel_costs, mode = "undirected", weighted = TRUE)
V(g)$haircut_cost <- haircut_costs

# ������������ �����
plot(g, edge.arrow.size = 0.2, vertex.size = 13)

# ����� ������ �������� ������ ��� �������
cheapest_city <- which.min(V(g)$haircut_cost)
cat("����� ������� ����� ��� �������: �����", cheapest_city, "\n")
cat("��������� ������� � ���� ������:", V(g)$haircut_cost[cheapest_city])







