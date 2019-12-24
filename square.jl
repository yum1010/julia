using Nemo

mutable struct Point
    x::Integer
    y::Integer
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

function makePointsData(n) #面と点のデータ
    points = Dict{String,Point}()
    for x in (0:n)
        for y in (0:n)
            points["$x,$y"]=Point(x,y)
        end
    end

    faces = Dict()
    #faces = Dict()
    for x in 0:(n-1)
        for y in 0:(n-1)
            pts = String[]
            push!(pts, "$x,$y")
            push!(pts, "$(x+1),$y")
            push!(pts, "$x,$(y+1)")
            push!(pts, "$(x+1),$(y+1)")
            faces["$x,$y"]=pts
            #push!(faces,pts)
        end
    end
    return((points,faces))
end

function laplacian(faces) #スイッチ行列
    N = length(faces)
    L=zeros(Int,N,N)
    fkeys = collect(keys(faces))
    for i in 1:length(fkeys)
        L[i,i]=1
        for j in 1:length(fkeys)
            if isadjacent(faces[fkeys[i]],faces[fkeys[j]])
                #println("$(fkeys[i]) + -- + $(fkeys[j])")
                L[i,j] = 1
            end
        end
    end
    L
end

function gauss(A)
    M = size(A)[1] #行
    N = size(A)[2] #列
    j = 1
    for i in 1:M #1行目からM行目まで
        nonzero = i
        while j <= N 
              found = false
            for k in i:m
                if A[k,j]!=0
                    nonzero = k
                    found = true
                    break
                end
            end
            if !found && j < N
                j = j+1
            else
                break
            end
        end
        for k in j:N #i行とnonzero行を交換
            tmp = A[i,k]
            A[i,k] = A[nonzero,k]
            A[nonzero,k] = tmp
        end
        for l in (i+1):M
            if A[l,j]!=0
                for k in j:N
                    A[l,k]=A[l,k]-A[i,k]
                end
            end
        end
    end
end

for n in 2:30
    print("size: $n ")
    faces=makePointsData(n)[2]
    S=MatrixSpace(ZZ, n*n, n*n)
    L=S(laplacian(faces))
    L=reduce_mod(L,2)
    cansolve(L,ones(n*n))
    #L=laplacian(faces) 
    #d = (round(Int128,det(L)))
    d = det(L)
    if d%2 == 0
        println("not full rank")
    else
        println()
    end
end

#data=makePointsData(5)
#
#faces=data[2]
#points=data[1]
#
#for pt in points
#    println(pt)
#end
