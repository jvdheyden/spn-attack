require "oo"

-- in Lua 5.3 als // vorhanden, TeX hat aber nur Lua 5.2
div = function(a,b)
    return (a-(a % b))/b
end

--rekursive Implementationen des euklidischen Alg.
euklid = function(a,b)
    if b == 0 then
        return a
    else
        return(euklid(b,a % b))
    end
end

--rekursive Implementationen des euklidischen Alg.
erw_euklid = function(a,b,p,pl,q,ql)
    --Lua erlaubt Aufrufe mit bel. Argumentzahl
    --Alles nicht belegte ist nil
    if p == nil then
        p = 0
        pl = 1
        q = 1
        ql = 0
    end
    if b == 0 then
        --ggT, lambda, mu
        return a, pl, ql
    else
        local d = div(a,b)
        local pn = pl - d*p
        local qn = ql - d*q
--         print(d,p,pn,q,qn)
        return erw_euklid(b,a % b,pn,p,qn,q)
    end
end

--Chinesischer Restesatz:
--kongr ist eine Tabelle deren Schlüssel die Moduli und deren
--Werte die Reste sind
crs = function(kongr)
    --Wir dürfen nicht in kleineren RkRingen rechnen
    --Wegen überladener Multiplikation/Add. (__add/__mull s.u.)
    --stellen wir deswegen sicher, dass alle Werte normale Zahlen sind
    for m,r in pairs(kongr) do
        assert(type(m) == 'number')
    end
    --Gesamtmodulus mges
    local mges = prod(keys(kongr))
    --n enthält die n_i (Schlüssel sind die mi)
    local n = map(function(r,mi) return mges/mi end,kongr)
    --mu Inverse der n_i bzgl. der m_i
    local mu = map(function(ni,mi) return RkRing(mi):inv(ni).val end,n)
    local mubin = map(function(bi,mi) return RRElement(mges,bi*mu[mi]*n[mi]) 
        end,kongr)
    return sum(
        map(function(bi,mi) return RRElement(mges,bi*mu[mi]*n[mi]) end,kongr)
    )
end


--Primzahleniteratorgenerator
--primbekannt is global (außer bei nocache)
--i,n sind "static" für jeden erzeugten generierten Iterator
primbekannt = {2,3}
primbis = function(n,nocache)
    local primbekannt = (nocache and {2,3}) or primbekannt 
    local i = 0
    return function()
        i=i+1
        if primbekannt[i] and primbekannt[i] <= n then
            return primbekannt[i]
        end
        for kand=primbekannt[i-1]+2,n,2 do
            for k,test in pairs(primbekannt) do
                if kand % test == 0 then
                    goto nonpr
                end
            end
            --normales Ende der Schleife, also prim:
            primbekannt[i] = kand
            --return muss in Lua am Blockende stehen
            do return kand end
            :: nonpr ::
        end
        --hier geben wir nichts zurück (kand ist nun > n)
    end
end

--gibt die enthaltene Potenz pot von fact zurück
--sowie n/(fact^pot)
faktorEnthalten = function(n,fact,nullWert)
    if n == 0 and nullWert then
        return nullWert
    end

    local pot = 0
    while n > 1 and n % fact == 0 do
        n = n/fact
        pot = pot + 1
    end
    return pot, n
end

--Primfaktoreniteratorgenerator
primfaktbekannt = {}
primfakt = function(n,nocache)
    if primfaktbekannt[n] then
        return statefull(pairs,primfaktbekannt[n])
    end

    if not nocache then
        primfaktbekannt[n] = {}
    end
    local norig = n
    local pr = primbis(n,nocache)
    return function()
        if n <= 1 then
            return
        end
        local p
        repeat p = pr() until n % p == 0
        local pot
        pot, n = faktorEnthalten(n,p)
        if not nocache then
            primfaktbekannt[norig][p] = pot
        end
        return p,pot
    end
end

RkRing = {
    new = function(m)
        local rr
        if ringArr[m] ~= nil then
            rr = ringArr[m]
        else
            rr = {modulo=m,elArray = {},}
            setmetatable(rr,RkRing)
            ringArr[m] = rr
        end
        return rr
    end,

    el = function(rr,num)
        return RRElement(rr,num)
    end,

    __tostring = function(rr)
        return "Restklassenring modulo " .. rr.modulo
    end,

    elToString = function(rr,el)
        return el.val .. " modulo " .. rr.modulo
    end,

    elToShortString = function(rr,el)
        return tostring(el.val)
    end,

    inv = function(rr,num)
        local ggT,lambda,mu = erw_euklid(rr.modulo,num)
--         assert(ggT == 1,"Not Invertible")
        if ggT ~= 1 then
            return RRElement(rr,0/0)
        else
            return RRElement(rr,mu % rr.modulo)
        end
    end,
    
    __eq = function(rr1,rr2)
        return rr1.modulo == rr2.modulo
    end,
}
makeclass(RkRing)

--globale Tabelle aller benutzen Ringe
--Ringe mit demselben Modulus sollen nicht mehrfach im Speicher sein
ringArr = {}

RRElement = {
    new = function(rr,num)
        if type(rr) == 'number' then
            rr = RkRing(rr)
        end
        if type(num) == 'table' then
            num = num.val
        end
        num = num % rr.modulo
        local el
        local lonum = (finite(num) and num) or "nan"
        if rr.elArray[lonum] ~= nil then
            el = rr.elArray[lonum]
        else
            el = {ring=rr,val=num % rr.modulo}
            setmetatable(el,RRElement)
            rr.elArray[lonum] = el
        end
        return el
    end,

    __tostring = function(el)
        return el.ring:elToString(el)
    end,

    __eq = function(el,el2)
        return (el.val == el2.val) and 
            (el.ring.modulo == el2.ring.modulo)
    end,

    inv = function(el)
        return el.ring:inv(el.val)
    end,
    
    finite = function(el)
        return finite(el.val);
    end,

    __unm = function(el)
        return RRElement(el.ring,-el.val)
    end,

    __add = function(el,a)
        el, a = ensureElementNumber(el,a)
        return RRElement(el.ring,el.val+a)
    end,

    __sub = function(el,a)
        el, a = ensureElementNumber(el,a)
        return RRElement(el.ring,el.val-a)
    end,

    __mul = function(el,a)
        el, a = ensureElementNumber(el,a)
        return RRElement(el.ring,el.val*a)
    end,

    __div = function(el,a)
        el, a = ensureElementNumber(el,a)
        return el*el.ring:inv(a)
    end,

    --naive Implementation:
    __pow = function(el,a)
        el, a = ensureElementNumber(el,a)
        if a == 0 then
            return RRElement(el.ring,1)
        elseif a < 0 then
            return RRElement(el.ring,(el:inv().val)^(-a))
        else
            return RRElement(el.ring,el.val^a)
        end
    end,
}
makeclass(RRElement)
ensureElementNumber = function(el,a)
    if type(el) == 'number' then --a ist dann Ringelement!
        el = RRElement(a.ring,el)
    end
    if type(a) ~= 'number' then
        a = a.val
    end
    return el, a
end

--finite testet, ob eine Zahl weder +inf,-inf noch NaN ist
finite = function(i)
    if type(i) == 'table' and i.finite then
        return i:finite()
    end
    return (i == i and i ~= (1/0) and i ~= -(1/0))
end


--Matrizen können entweder Elemente eines RR enthalten oder float
--es wird stets der Ring des ersten Elements als Ring der Matrix gesetzt,
--sofern kein expliziter Ring angegeben wird
Matrix = {
    new = function(init,ncol,ring)
        local mat
        if type(init) == 'number' then
            mat = Matrix.newNoInit(init,ncol,ring)
            for i=1,mat.nrow do
                mat[i] = {}
                for j=1,mat.ncol do
                    mat[i][j] = 0
                end
            end
        elseif type(init) == 'table' and --1-dim-Array
                (type(init[1]) == 'number' or init[1].val) then
            mat = Matrix.newNoInit(div(#(init),ncol),ncol,
                ring or (type(init[1]) == 'table' and init[1].ring))
            for i=1,mat.nrow do
                mat[i] = {}
                for j=1,mat.ncol do
                    mat[i][j] = init[(i-1)*mat.ncol+j] or 0
                end
            end
        else --2-dim-Array (nrow,ncol implizit)
            assert(type(init[1]) == 'table')
            mat = Matrix.newNoInit(#(init),ncol or #(init[1]),
                ring or (type(init[1][1]) == 'table' and init[1][1].ring))
--             mat.nrow = #(init)
--             mat.ncol = #(init[1])
            for i=1,mat.nrow do
                mat[i] = {}
                for j=1,mat.ncol do
                    mat[i][j] = init[i][j] or 
                            ((mat.ring and mat.ring:el(0)) or 0)
                end
            end
        end
        if type(ring) == "number" then
            ring = RkRing(ring)
        end
        if ring ~= nil then 
            for i=1,mat.nrow do
                for j=1,mat.ncol do
                    mat[i][j] = ring:el(mat[i][j])
                end
            end
        end
        if mat.nrow >= 1 and type(mat[1][1]) == 'table' then
            mat.ring = mat[1][1].ring
        end
        return mat
    end,

    newNoInit = function(nrow,ncol,ring)
        local mat = {nrow=nrow,ncol=ncol,ring=ring}
        setmetatable(mat,Matrix)
        return mat
    end,

    eins = function(n,ring)
        local mat = Matrix(n,n,ring)
        if type(ring) == "number" then
            ring = RkRing(ring)
        end
        for i=1,n do
            mat[i][i] = (ring and ring:el(1)) or 1
        end
        return mat
    end,
    
    t = function(mat)
        return Matrix(transp(mat),mat.nrow,mat.ring)
    end,

    
    kleb = function(mat,mat2)
        mat = Matrix(mat)
        for i=1,mat.nrow do
            for j=1,mat2.ncol do
                mat[i][mat.ncol+j] = mat2[i][j]
            end
        end
        return mat
    end,

    rs = function(mat,ncol)
        ncol = ncol or div(mat.ncol,2)
        local ret = Matrix(mat.nrow,div(mat.ncol,2),mat.ring)
        local o = mat.ncol - ret.ncol
        for i=1,ret.nrow do
            for j=1,ret.ncol do
                ret[i][j] = mat[i][o+j]
            end
        end
        return ret
    end,
    
    
    diag = function(mat)
        local dmin = (mat.nrow < mat.ncol) and mat.nrow or mat.ncol
        local ret = {}
        for i=1,dmin do
            ret[i]=mat[i][i]
        end
        return ret
    end,

    __tostring = function(mat)
        local ts
        if mat.ring ~= nil then
            ts = function(el) 
                return mat.ring:elToShortString(el)
            end
        else 
            ts = function(n)
                return n
            end
        end
        s = ""
        for i=1,mat.nrow do
            s = s .. table.concat(map(ts,mat[i]), "  ")
            if i < mat.nrow then 
                s = s .. "\n"
            end
        end
        if mat.ring ~= nil then
            s = s .. "\n modulo " .. mat.ring.modulo
        end
        return s
    end,

    __index = function (mat, key)
        --Für normale Schlüssel (keine Tabellen)
        --dasselbe Verhalten wie sonst durch makeclass erzeugt,
        --d.h. nicht vorhandene Einträge werden in der Klasse gesucht.
        if type(key) ~= 'table' then
            return Matrix[key]
        --falls der Schlüssel eine Zeilennummer ist, gebe Zeilenvektor zurück
        elseif #(key) == 1 and type(key[1]) == 'number' 
                and key[1] > 0 then
            local ret = Matrix.newNoInit(1,mat.ncol,mat.ring)
            ret[1] = mat[key[1]]
            return ret
        end
        error("Matrixindizierung")
    end,

    __add = function(mat,mat2)
        assert(mat.nrow == mat2.nrow and mat.ncol == mat2.ncol,
            "Dim passen nicht")
        local ring = mat.ring or mat2.ring
        local ret = Matrix(mat.nrow,mat.ncol,ring)
        for i=1,mat.nrow do
            for j=1,mat.ncol do
                ret[i][j] = mat[i][j] + mat2[i][j]
            end
        end
        return ret
    end,

    __sub = function(mat,mat2)
        assert(mat.nrow == mat2.nrow and mat.ncol == mat2.ncol,
            "Dim passen nicht")
        local ring = mat.ring or mat2.ring
        local ret = Matrix(mat.nrow,mat.ncol,ring)
        for i=1,mat.nrow do
            for j=1,mat.ncol do
                ret[i][j] = mat[i][j] - mat2[i][j]
            end
        end
        return ret
    end,

    __mul = function(mat,mat2)
        if type(mat) == 'number' then
            mat,mat2=mat2,mat
        end
        --Ring richtet sich nach Matrix, nicht nach Skalar
        if getmetatable(mat2) == RRElement then
            mat2 = mat2.val
        end
        --skalare Multiplikation
        if type(mat2) == 'number' then
            local ret = Matrix.newNoInit(1,mat.ncol,mat.ring)
            for i=1,mat.nrow do
                ret[i] = map(function(x) return x*mat2 end, mat[i])
            end
            return ret
        end
        
        --von nun an Matrixmultiplikation
        assert(mat2.nrow == mat.ncol,"Dim passen nicht")
        local ring = mat.ring or mat2.ring
        local ret = Matrix(mat.nrow,mat2.ncol,ring)
        for i=1,ret.nrow do
            for j=1,ret.ncol do
                --sum hat automatisch
                --den Typ entsprechend ret.ring
                local sum = 0
                for k=1,mat.ncol do
                    sum = sum + mat[i][k] * mat2[k][j]
                end
                ret[i][j] = sum
            end
        end
        return ret
    end,
    
    __eq = function(m1,m2)
        if m1.ring ~= m2.ring or m1.nrow ~= m2.nrow or
                m1.ncol ~= m2.ncol then
            return false
        end
        for i=1,m1.nrow do
            for j=1,m2.nrow do
                if m1[i][j] ~= m2[i][j] then
                    return false
                end
            end
        end
        --alle Elemente gleich, also:
        return true
    end,

    gaussSimpel = function(mat)
        mat = Matrix(mat)
        local cprod = 1
        for runde=1,math.min(mat.nrow,mat.ncol) do
            local ierfolg, c = nil, nil
            for i=runde,mat.nrow do
                c = mat[i][runde]
                if finite(c^(-1)) then
                    ierfolg = i
                    break
                end
            end

            if not ierfolg then
                return nil, nil
            end

            cprod = cprod * c
            c = c^(-1)
            if ierfolg ~= runde then
                mat[runde], mat[ierfolg] = mat[ierfolg], mat[runde]
                cprod = -cprod
            end
            mapmod(function(x) return c*x end, mat[runde])

            for i=1,mat.nrow do
                if i ~= runde then
                    local c = -mat[i][runde]
                    mat[i] = (mat[{runde}]*c+mat[{i}])[1]
                end
            end
        end
        return mat, cprod
    end,

    gaussPrimPot = function(mat)
        local pr  = primfakt(mat.ring.modulo)
        local p,expmax = pr({})
       
        assert(p and not pr(),"keine Primzahlpotenz")
        if expmax == 1 then 
            return mat:gaussSimpel()
        end

        mat = Matrix(mat)
        local cprod = 1
        for runde=1,math.min(mat.nrow,mat.ncol) do
            local minEnth,imin = expmax, nil
            for i=runde,mat.nrow do
                local enth = 
                    faktorEnthalten(mat[i][runde].val,p,expmax)
                if enth < minEnth then
                    minEnth = enth
                    imin = i
                end
            end

            --Enthält Spalte nur Nullen auf und unter der Diagonale?
            if not imin then 
                return nil, nil
            end

            --hier sind Konvertierungen nach number and 
            --dann RRElement nötig:
            local c = mat.ring:el(mat[imin][runde].val/(p^minEnth))
            cprod = cprod * c
            c = c^(-1)
            if imin ~= runde then
                mat[runde], mat[imin] = mat[imin], mat[runde]
                cprod = -cprod
            end
            mapmod(function(x) return c*x end, mat[runde])

            for i=1,mat.nrow do
                if i ~= runde then
                    local c = -mat[i][runde]
                    mat[i] = (mat[{runde}]*c+mat[{i}])[1]
                end
            end
        end
        return mat, cprod
    end,
    
    gaussZusGes = function(mat)
        mat = Matrix(mat)
        local cprod = 1
        for runde=1,math.min(mat.nrow,mat.ncol) do
            if runde ~= mat.nrow then
                for i=mat.nrow-1,runde,-1 do
                    local ggT,lambda,mu = 
                        erw_euklid(mat[i][runde].val,
                            mat[i+1][runde].val)
                    if ggT == 0 then
                        goto continue
                    end
                    local di,dip = 
                        (mat[i][runde].val)/ggT,
                            (mat[i+1][runde].val)/ggT
                    local temp = mat[i]
                    mat[i] = {}
                    for j=1,mat.ncol do
                        mat[i][j] = (lambda*temp[j])+(mu*mat[i+1][j])
                        mat[i+1][j] = (-dip*temp[j])+(di*mat[i+1][j])
                    end
                    :: continue ::
                end
            end
            local c = mat[runde][runde]
            if finite(c^(-1)) then
                cprod = cprod * c
                c = c^(-1)
                mapmod(function(x) return c*x end, mat[runde])
                for i=1,mat.nrow do
                    if i ~= runde then
                        local ci = -mat[i][runde]
                        mat[i] = (mat[{runde}]*ci+mat[{i}])[1]
                    end
                end
            end
        end
        return mat, cprod
    end,

    inv = function(mat)
        if mat.nrow ~= mat.ncol then
            return nil
        end
        if mat.invpre then
            return mat.invpre 
        end
        if not mat.ring then --Matrix aus float
            return mat:invFloat()
        end
        local erw,cprod = mat:kleb(Matrix.eins(mat.nrow,mat.ring)),nil
        erw,cprod = erw:gaussZusGes()
        mat.detpre = prod(erw:diag())*cprod
        --Wenn mat invertierbar ist, ist das Produkt der 
        --Diagonale auch 1,
        --da wir in diesem Fall in gaussZusGes()
        --mit Inversem multiplizieren
        if not finite(mat.detpre^(-1)) then
            return nil
        end
        mat.invpre = erw:rs()
        return mat.invpre
    end,
    
    invFloat = function(mat)
        local erw,cprod = mat:kleb(Matrix.eins(mat.nrow,mat.ring)),nil
        erw,cprod = erw:gaussSimpel()
        if not erw then
            mat.detpre = 0
            return nil
        end
        mat.invpre = erw:rs()
        mat.detpre = cprod
        return mat.invpre
    end,
    
    det = function(mat)
        --Versuche(!) zu invertieren, dabei wird auch immer
        --die Det berechnet.
        mat:inv()
        return mat.detpre
    end,
    
    --berechnet die Determinante mittels CRS
    detCRS = function(mat)
        --Primzahlpotenz?
        if rangemany(primfakt(mat.ring.modulo),1,1) then
            --erweiterte Matrix (mit Identität)
            local erw,cprod = 
                mat:kleb(Matrix.eins(mat.nrow,mat.ring)),nil
            erw, cprod = erw:gaussPrimPot()
            --bricht nur ab, wenn alle Elemente unter
            --der Diag 0 sind, dann ist auch Det. 0
            if not erw then
                return mat.ring:el(0)
            end
            return prod(erw:diag())*cprod
        end
        
        --keine Primpotenz, wir berechnen mit CRS
        local kongr = {}
        for p,exp in primfakt(mat.ring.modulo) do
            local matp = Matrix(mat,mat.ncol,RkRing(p^exp))
            kongr[p^exp] = matp:detCRS().val
        end
        return crs(kongr)
    end,
    
}
makeclass(Matrix)


Bitarray = {
    new = function(init)
        local a = {}
        setmetatable(a,Bitarray)
        if not init then
            return a
        end
        for i=1,#init do
            a[i] = init[i]
        end
        return a
    end,
    
    __add = function(a,b)
        local c = Bitarray()
        for i=1,#a do
            c[i] = (a[i]+b[i])%2
        end
        return c
    end,

    __mul = function(a,b)
        local c = Bitarray()
        for i=1,#a do
            c[i] = a[i]*b[i]
        end
        return c
    end,
    
    __eq = function(a,b)
        if #a ~= #b then
            return false
        end
        for i=1,#a do
            if a[i] ~= b[i] then
                return false
            end
        end
        return true
    end,
    
    __tostring = function(a)
        local s = "Bitarray({"
        for i=1,#a-1 do
            s = s .. a[i] .. ","
            if i % 4 == 0 then
                s = s .. "  "
            end
        end
        s = s .. a[#a] .. "})"
        return s
    end,
    
    --Parität
    par = function(a)
        return sum(a) % 2
    end,
    
    
}
makeclass(Bitarray)

        
Alpha = RkRing(26)
Alpha.printAsNumbers = false
Alpha.elToString = function(rr,el)
    if Alpha.printAsNumbers then
        return tostring(el.val)
    else
        return el.alph
    end
end
Alpha.elToShortString = Alpha.elToString
for i=0,25,1 do
    local t = Alpha:el(i)
    t.alph = string.char(string.byte("A")+i)
    Alpha[string.char(string.byte("a")+i)] = t
    Alpha[string.char(string.byte("A")+i)] = t
end

Alpha.all = function()
    local i = -1
    return function()
        i = i+1
        if i <= 25 then
            return Alpha:el(i)
        else
            return nil
        end
    end
end

stringToAlphaArray = function(s)
    s = s:upper()
    local a = {}
    for i=1,s:len(),1 do
        a[i] = Alpha[s:sub(i,i)]
    end
    return a
end

alphaArrayToString = function(a)
    b = {}
    for i,el in ipairs(a) do
        b[i] = el.alph
    end
    return table.concat(b,"") 
end

hexToBitArray = {
    ["0"] = Bitarray({0,0,0,0}),
    ["1"] = Bitarray({0,0,0,1}),
    ["2"] = Bitarray({0,0,1,0}),
    ["3"] = Bitarray({0,0,1,1}),
    ["4"] = Bitarray({0,1,0,0}),
    ["5"] = Bitarray({0,1,0,1}),
    ["6"] = Bitarray({0,1,1,0}),
    ["7"] = Bitarray({0,1,1,1}),
    ["8"] = Bitarray({1,0,0,0}),
    ["9"] = Bitarray({1,0,0,1}),
    ["A"] = Bitarray({1,0,1,0}),
    ["B"] = Bitarray({1,0,1,1}),
    ["C"] = Bitarray({1,1,0,0}),
    ["D"] = Bitarray({1,1,0,1}),
    ["E"] = Bitarray({1,1,1,0}),
    ["F"] = Bitarray({1,1,1,1}),
}

--für Lab und Dab (s. analyse) ist 16 sinnvoller Wert, ergo [16]="G"
hex = {"1","2","3","4","5","6","7","8","9",
        "A","B","C","D","E","F","G",[0]="0"}
        
bitArrayToHex = function(a)
    return hex[a[4]+2*a[3]+4*a[2]+8*a[1]]
end

hexStringToBitArray = function(s)
    s = s:upper()
    local a = Bitarray()
    for i=1,s:len(),1 do
        local j = 4*(i-1)
        local h = hexToBitArray[s:sub(i,i)]
        a[j+1] = h[1]
        a[j+2] = h[2]
        a[j+3] = h[3]
        a[j+4] = h[4]
    end
    return a
end

bitArrayToHexString = function(a)
    local s = ""
    for i=1,(#a)/4,1 do
        local j = 4*(i-1)
        s = s .. bitArrayToHex({a[j+1],a[j+2],a[j+3],a[j+4]})
    end
    return s
end

intToBitArray = function(x,bits)
    local a = Bitarray()
    local l = bits-1
    for i=1,bits do
        a[i] = (x >= 2^(bits-i) and 1) or 0
        x = x % (2^(bits-i))
    end
    return a
end
