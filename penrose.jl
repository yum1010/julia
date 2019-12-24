using Nemo

mutable struct Point
    x::Integer
    y::Integer
end

struct Vec2
    x
    y
end

function isadjacent(f,g)
    numeq = 0
    for p in f
        for q in g
            if p == q
                numeq+=1
            end
        end
    end
    if numeq == 2
        return(true)
    else
        return(false)
    end
end

function expand(x)
    [-x[5], x[1], x[2], x[3], x[4]] + [x[2], x[3], x[4], x[5], -x[1]]
end




function subdivide(x,y)#拡大後細分
    m = expand(x);
    return[m[1]-x[1]+y[1],
	   m[2]-x[2]+y[2],
	   m[3]-x[3]+y[3],
	   m[4]-x[4]+y[4],
	   m[5]-x[5]+y[5]
	  ];
end

function project(x) #n次元を二次元座標に表す
    p=0;
    q=0;
    dim = length(x)
    for i in 1:dim
	p += x[i]*cos((i-1)*pi/dim);
	q += x[i]*sin((i-1)*pi/dim);
    end
    return Vec2(p,q);
    #return Vec2(400+p*40,400-q*40);
end

function AExpSub(T,vertices) #拡大細分後の４頂点を加えていく
    V = [];
    push!(V,expand(vertices[T[1]]));
    push!(V, expand(vertices[T[2]]));
    push!(V, expand(vertices[T[3]]));
    push!(V, subdivide(vertices[T[1]],vertices[T[2]])) #細分
    return(V);   
end
    
function BExpSub(T,vertices) #拡大細分後の４頂点を加えていく
    V = [];
    push!(V, expand(vertices[T[1]]))
    push!(V, expand(vertices[T[2]]))
    push!(V, expand(vertices[T[3]]))
    push!(V, subdivide(vertices[T[1]],vertices[T[2]])) #細分
    push!(V, subdivide(vertices[T[1]],vertices[T[3]])) #細分
    return(V);   
end


function pointEq(p,q)
    for i in 1:length(p) #iにindexが入る
	if p[i] != q[i]
            return false
        end
    end
    return true;
end

function contained(p,V)
    for  i in 1:length(V)
	if pointEq(p,V[i])
	    return i;
        end
    end
    return length(V)+1
end


function allAExpSub(Atriangles,Btriangles,vertices) #全てのA型三角を拡大細分してできる頂点集合を返す
    V = [];
    A = [];
    B = [];
    for T in Atriangles #一枚取り出す
	idx = [];
	for p in AExpSub(T,vertices) #拡大細分した4点の五次元座標を一つ取り出す
	    push!(idx,contained(p,V))#細分した4点のVにおけるインデックス
	    if contained(p,V)==length(V)+1 #含まれていないときは
		push!(V,p); #Vに付け足す
            end
        end
	push!(A, [idx[3],idx[4],idx[2]]) #A型三角形のVのインデックス
	push!(B, [idx[3],idx[4],idx[1]]) #B型三角形のVのインデックス
    end
    #この後にBを付け加える
    for T in Btriangles #(一枚取り出す)
	idx = [];
	for p in BExpSub(T,vertices) #(拡大細分して)
	    push!(idx, contained(p,V));
	    if contained(p,V)==length(V)+1 #含まれていないときは
		push!(V, p);
            end
	end
	push!(A, [idx[5],idx[4],idx[2]]);
	push!(B, [idx[5],idx[4],idx[1]]);
	push!(B, [idx[3],idx[5],idx[2]]);
    end
    return(Dict("V"=>V,"A"=>A,"B"=>B));
end


function makePenroseData(N)
    vertices = [[0,0,0,0,0],#中心
	        [1,0,0,0,0],#10等分したそれぞれの点を五次元座標に
	        [0,1,0,0,0],
	        [0,0,1,0,0],
	        [0,0,0,1,0],
	        [0,0,0,0,1],
	        [-1,0,0,0,0],
	        [0,-1,0,0,0],
	        [0,0,-1,0,0],
	        [0,0,0,-1,0],
	        [0,0,0,0,-1],
	        ];
    Atriangles=[[0,1,2],#三角形に向きをつけた
	        [0,3,2],
	        [0,3,4],
	        [0,5,4],
	        [0,5,6],
	        [0,7,6],
	        [0,7,8],
	        [0,9,8],
	        [0,9,10],
	        [0,1,10]
	        ];
    for i in 1:length(Atriangles)
        Atriangles[i] = Atriangles[i].+1
    end
    Btriangles = [];

    for i in 1:N
        tiling=allAExpSub(Atriangles,Btriangles, vertices) #allAExpSub() を実行し、V(頂点情報)、A(A型三角形のインデックス)、B(B型三角形のインデックス)が返る
        vertices=tiling["V"] #verticesにVを入れ
        Atriangles = tiling["A"]
        Btriangles = tiling["B"]
    end

    #ひし形作る
    Arhombi=[];
    for i in 1:length(Atriangles)
        for j in (i+1):length(Atriangles)
	    if (Atriangles[i][2]==Atriangles[j][2]&& Atriangles[i][3]==Atriangles[j][3])
	        push!(Arhombi, [Atriangles[i][1],Atriangles[i][2],Atriangles[j][1],Atriangles[j][3]])
            end
        end
    end
    Brhombi=[];
    for i in 1:length(Btriangles)
        for j in (i+1):length(Btriangles)
	    if (Btriangles[i][1]==Btriangles[j][1] &&  Btriangles[i][3]==Btriangles[j][3])
                push!(Brhombi, [Btriangles[i][1],Btriangles[i][2],Btriangles[j][3],Btriangles[j][2]])
            end
        end
    end
    Dict("R"=>vcat(Arhombi, Brhombi), "V"=>vertices)
end

function makePointsData(n)
    #points = Dict{String,Point}()
    points = []
    for x in (0:n)
        for y in (0:n)
            #points["$x,$y"]=Point(x,y)
            push!(points,[x,y])
        end
    end

    #faces = Dict()
    faces = []
    for x in 0:(n-1)
        for y in 0:(n-1)
            face = []
            for i in 1:length(points)
                if pointEq([x,y],points[i])
                    push!(face,i)
                end
            end
            for i in 1:length(points)
                if pointEq([x+1,y],points[i])
                    push!(face,i)
                end
            end
            for i in 1:length(points)
                if pointEq([x+1,y+1],points[i])
                    push!(face,i)
                end
            end
            for i in 1:length(points)
                if pointEq([x,y+1],points[i])
                    push!(face,i)
                end
            end
            push!(faces,face)
        end
    end
    return(Dict("V"=>points,"R"=>faces))
end

function laplacian(faces)
    N = length(faces)
    L=zeros(Int8,N,N)
    for i in 1:N
        L[i,i]=1
        for j in 1:N
            if isadjacent(faces[i],faces[j])
                L[i,j] = 1
            end
        end
    end
    L
end


function iszero(V)
    for x in V
        if x!=0
            return false
        end
    end
    return true
end

function gauss(A)
    M = size(A)[1] #行
    A = hcat(A,ones(Int8,M))
    N = size(A)[2] #列
    j = 1
    for i in 1:M #1行目からM行目まで
        while j<=N
            nonzero = i #0じゃない一番上の行
            found = false
            for k in i:M
                if A[k,j]!=0
                    nonzero = k
                    found = true
                break
                end
            end
            if !found
                j = j+1
            else
                for k in j:N #i行とnonzero行を交換
                    tmp = A[i,k]
                    A[i,k] = A[nonzero,k]
                    A[nonzero,k] = tmp
                end
                break
            end
        end #入れ替え完了
        if j>N #全て0
            break
        end
        
        for l in (i+1):M
            if A[l,j]!=0
                for k in j:N
                    A[l,k]=(A[l,k]-A[i,k]+2)%2
                end
            end
        end
    end
    
    corank=0
    for i in 0:M-1      
        if iszero(A[M-i,1:M])
            corank+=1
        else
            break
        end       
    end
    println("corank=",corank)
    return A
end

function allOntoalloff(L) #答え
    M=size(L)[1]
    ans=zeros(Int8,M)#答えをいれるベクトル
    N=M+1

    for i in 1:M
        ans[i]=L[i,N]      
    end
    for i in 0:(M-1)
        for j in (M-i+1):M
            ans[M-i]=(ans[M-i]+L[M-i,j]*ans[j])%2
        end
    end
    return ans
end


using Plots

#tiling=makePenroseData(9)
tiling=makePointsData(151)
L=laplacian(tiling["R"])
ans=allOntoalloff(gauss(L))

x=[]
y=[]
colors=[]
for t in tiling["R"]#R=ひし形,V=頂点
    for idx in t
        pt = project(tiling["V"][idx])
        push!(x,pt.x)
        push!(y,pt.y)
    end
    push!(x,NaN)
    push!(y,NaN)
end
for a in ans
    if a==1
        push!(colors,"red")
    else
        push!(colors,"green")
    end
end


#L=laplacian(tiling["R"])
#L=transpose(vcat(transpose(L), ones(Int8,size(L)[1])'))
plot(x,y, seriestype=:shape,
     aspect_ratio=1,
     linewidth=0.0,
     legend=false,
     axis=false,
     size=(1000,1000),
     fillcolor=colors
     )

