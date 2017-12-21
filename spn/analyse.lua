require "oo"
require "grundl"
require "krypto"

--n-Grammanalyse für additive Chiffre
--h Anzahl der häufigsten n-Gramme, die probiert werden
--Vorteil kleiner n: rauhere Verteilung
--Vorteil größerer n: Konsistenzprüfung für Kandidaten möglich
additivngramm = function(y,spr,n,h)
    local ngr = ngramm_anteil(y,n)
    local sngr = nil
    local kand = {}
    h = h or 3
    if l == 1 then
        sngr = spr.mono
    elseif l == 2 then
        sngr = spr.bi
    else
        sngr = spr.tri
    end
    for i=1,h do
        for j=1,h do
            local ites = stringToAlphaArray(ngr:sorted()[i])
            local jtes = stringToAlphaArray(sngr:sorted()[j])
            local k = ites[1]-jtes[1]
            for l=2,n do
                k = ((ites[l]-jtes[l] == k) and k) or nil
            end
            if k then
                kand[#kand+1] = {k,
                    alphaArrayToString( 
                        affin_ent({1,k},stringToAlphaArray(y)))}
            end
        end
    end
    return kand
end


--Wir versuchen den Schlüssel mit der besten "Passung" des
--resultierenden Klartexts (s.u.) zu finden
additivpassend = function(y,spr,ngrnutzen)
    local kmax, passmax, xmax = 0, 0, nil
    for i = 0,25 do
        local k = Alpha:el(i)
        local x = alphaArrayToString( 
                        affin_ent({1,k},stringToAlphaArray(y)))
        local pass = sprachpassend(x,spr,ngrnutzen)
        if passmax < pass then
            kmax = k
            passmax = pass
            xmax = x
        end
    end
    return kmax,xmax
end



hillklartext = function(x,y,l,volleSuche)
    local onStrings
    x,y,onStrings = arrOrStringArg(x,y)
    if #x % l ~= 0 then
        return nil
    end
    xbl = blocks(x,l)
    ybl = blocks(y,l)

    --falls eine der Matrizen xmat aus l
    --Klatextblöcken invertierbar ist,
    --muss k = xmat^(-1)*ymat sein, wobei
    --ymat aus den passenden Kryptotextblöcken besteht.
    --Diesen Ansatz verfolgen wir zuerst
    
    local xmat = nil
    local ymat = nil
    --Schleife über alle Indexmengen der Größe l
    for s in choose(keys(xbl),l) do
        xmat = {}
        
        --Bilde eine lxl-Matrix
        for i,j in pairs(s) do
            xmat[i] = xbl[j]
        end
        xmat = Matrix(xmat)
    
        --Versuche sie zu invertieren
        xmat = xmat:inv()
        --Klappt dies, berechne die passende Kryptotextmatrix
        if  xmat then
            ymat = {}
            for i,j in pairs(s) do
                ymat[i] = ybl[j]
            end
            ymat = Matrix(ymat)
            --Invertierbar*Invertierbar=Invertierbar
            --Daher muss auch ymat invertierbar sein, sonst
            --ist es k nicht
            if ymat:inv() then
                break
            else
                xmat = nil--xmat wird auch als Flag benutzt, s.u.
            end
        end
    end
    if xmat then
        --xmat ist bereits invertiert, s.o.
        k = xmat*ymat
        --Die Matrix ist der einzige Schlüssel für die
        --Blöcke in xmat, aber ein Schlüssel muss für alle
        --Blöcke 
        local ytest = hill_ver(k,x)
        for i=1,#y do
            if y[i] ~= ytest[i] then
                return "Widerspruch"
            end
        end
        return k
    end
    
-- Das Folgende erläutert am Fall l=2:
--     
-- x1x2x3x4x5x6  k1 k3    = y1y2y3y4y5y6
--               k2 k4
-- umformen zu:
-- x1 x2  0  0   k1       = y1
--  0  0 x1 x2   k2       = y2
-- x3 x4  0  0   k3       = y3
--  0  0 x3 x4   k4       = y4
-- x5 x6  0  0            = y5
--  0  0 x5 x6            = y6
--
-- Nun können wir gaußsche Elimination auf die (|x|+1)-mal-l^2-Matix
-- (ohne die ki) anwenden.
    
    local lgs = {}
    for _,b in pairs(xbl) do
        for j=1,l do
            local lgszeile = {}
            for h=1,l do
                lgszeile[(j-1)*l+h] = b[h]
            end
            lgs[#lgs+1] = lgszeile
        end
    end
    lgs = Matrix(lgs,l^2)
    local ycol = Matrix(y,1)
    lgs = lgs:kleb(ycol)
    print(lgs:gaussZusGes())

-- Das LGS ist lösbar, wenn es keine Zeilen gibt, in denen
-- das letzte Element nicht 0 ist und von keinem der anderen
-- geteilt wird.
-- TODO: algorithmisch testen

    if volleSuche then
        for a in Alpha.all() do
            for b in Alpha.all() do
                for c in Alpha.all() do
                    for d in Alpha.all() do
        local k = {a,b,c,d}
        local xe = hill_ent(k,y)
        if xe ~= "ungültiger Schlüssel" then
            xe = alphaArrayToString(xe)
        end
--         print(alphaArrayToString(xe))
        if xe ~= "ungültiger Schlüssel" and 
ngramm_hauefig(xe,2)["ER"] then
                
            print(xe)
        end
                    end end end end
    end
end

--immer onStrings
hillngrammblock = function(y,l,spr)
    if y:len() % l ~= 0 then
        return nil
    end
    
    --n-Grammttabelle des Kryptotexts
    local ngr = ngramm_anteil(y,l,true)--true:blockweise
    --n-Grammtabelle der vermuteten Sprache
    local sngr = nil
    if l == 1 then
        sngr = spr.mono
    elseif l == 2 then
        sngr = spr.bi
    else
        sngr = spr.tri
    end

    --wie beim Angriff mit bekanntem Klartext
    --versuchen wir 2 invertiere Matrizen aus Klartext-Blöcken
    --(hier Sprach-n-Gramme) und Kryptotextblöcken zu bilden
    
    local xmat = nil
    local ymat = nil
    --Schleife über alle l-elementigen Mengen aus
    --4 häufigsten n-Grammen für Sprache und Kryptotext
    local kand = {}--mögliche Schlüssel
    local kandx = {}--passende Klartexte
    local kandst = {}--Positionen (1-4) der benutzten n-Gramme
    --in der Liste der häufigsten n-Gramme
    
    for s in choose({1,2,3,4},l) do--alle Komb. aus ersten 4 n-Grammen
        for t in kperm({1,2,3,4},l) do--alle Variationen d. 1. 4 n-Gr.
            xmat = {}
            
            --Bilde eine lxl-Matrix
            for i,j in pairs(s) do
                xmat[i] = stringToAlphaArray(sngr:sorted()[j])
            end
            xmat = Matrix(xmat)
        
            --Versuche sie zu invertieren
            xmat = xmat:inv()
            --Klappt dies, berechne die passende Kryptotextmatrix
            if  xmat then
                ymat = {}
                for i,j in pairs(t) do
                    ymat[i] = stringToAlphaArray(ngr:sorted()[j])
                end
                ymat = Matrix(ymat)
                --Invertierbar*Invertierbar=Invertierbar
                --Daher muss auch ymat invertierbar sein, sonst
                --ist es k nicht
                if ymat:inv() then
                    print(dump(s),dump(t))
                    kand[#kand+1] = xmat*ymat
                    kandx[#kand] = alphaArrayToString(
                        hill_ent(kand[#kand],stringToAlphaArray(y)))
                    kandst[#kand] = "Sprache:".. dumpNoKeys(s) ..
                        " Krypto: "..dumpNoKeys(t)
                end
            end
        end
    end
    return kand,kandx,kandst
end

Ngrammtab = {
    new = function(tab)
        tab = dcopy(tab)
        setmetatable(tab,Ngrammtab)
        return tab
    end,
    
    sorted = function(ntab)
        return ikeyssort(ntab,numval("desc"))
    end,

    __tostring = function(ntab)
        local s = "{"
        for i,k in ipairs(ntab:sorted()) do
            s = s .. k .. ":"  .. ntab[k] .. ", "
        end
        return s .. "}"
    end,
}
makeclass(Ngrammtab)

lang = {
    en = {
        mono= Ngrammtab ({
            A=0.0804,
            B=0.0154,
            C=0.0306,
            D=0.0399,
            E=0.1251,
            F=0.0230,
            G=0.0196,
            H=0.0549,
            I=0.0726,
            J=0.0016,
            K=0.0067,
            L=0.0414,
            M=0.0253,
            N=0.0709,
            O=0.0760,
            P=0.0200,
            Q=0.0011,
            R=0.0612,
            S=0.0654,
            T=0.0925,
            U=0.0271,
            V=0.0099,
            W=0.0192,
            X=0.0019,
            Y=0.0173,
            Z=0.0009
        }),
        bi = Ngrammtab( {
            TH=0.0315,
            EN=0.0251,
            AN=0.0172,
            IN=0.0196,
            ER=0.0154,
            RE=0.0148,
            ON=0.0145,
            ES=0.0145,
            TI=0.0128,
            AT=0.0124,
            ST=0.0121,
            EN=0.0120,
            OR=0.0113,
            ND=0.0118,
            TO=0.0111,
            NT=0.0110,
            ED=0.0107,
            ED=0.0107,
            IS=0.0106,
            AR=0.0101,
            OU=0.0096,
            OF=0.0094,
            TE=0.0094
        }),
        tri = Ngrammtab ({
            THE=0.0353,
            ING=0.0111,
            AND=0.0102,
            ION=0.0075,
            TIO=0.0075,
            ENT=0.0073,
            ERE=0.0069,
            HER=0.0068,
            ATE=0.0066,
            VER=0.0064,
            TER=0.0063,
            THA=0.0062,
            ATI=0.0059,
            FOR=0.0059,
            HAT=0.0055,
            ERS=0.0054,
            HIS=0.0052,
            RES=0.0050,
            ILL=0.0047
        }),
        icl = 0.0687,
    },
    de = {
        mono = Ngrammtab ({
            A=0.0647,
            B=0.0193,
            C=0.0268,
            D=0.0487,
            E=0.1748,
            F=0.0165,
            G=0.0306,
            H=0.0425,
            I=0.0773,
            J=0.0027,
            K=0.0146,
            L=0.0349,
            M=0.0258,
            N=0.0982,
            O=0.0298,
            P=0.0096,
            Q=0.0002,
            R=0.0754,
            S=0.0683,
            T=0.0613,
            U=0.0417,
            V=0.0094,
            W=0.0148,
            X=0.0004,
            Y=0.0008,
            Z=0.0114
        }),
        bi = Ngrammtab ({
            ER=0.0409,
            EN=0.0040,
            CH=0.0242,
            DE=0.0227,
            EI=0.0193,
            ND=0.0187,
            TE=0.0185,
            IN=0.0168,
            IE=0.0163,
            GE=0.0147,
            ES=0.0140,
            NE=0.0122,
            UN=0.0119,
            ST=0.0116,
            RE=0.0112,
            HE=0.0102,
            AN=0.0102,
            BE=0.0101,
            SE=0.0099,
            NG=0.0094,
            DI=0.0093,
            SC=0.0089
        }),
        tri = Ngrammtab ({
            EIN=0.0122,
            ICH=0.0111,
            NDE=0.0089,
            DIE=0.0087,
            UND=0.0087,
            DER=0.0086,
            CHE=0.0075,
            END=0.0075,
            GEN=0.0071,
            SCH=0.0066,
            CHT=0.0061,
            DEN=0.0057,
            INE=0.0053,
            NGE=0.0052,
            NUN=0.0048,
            UNG=0.0048,
            DAS=0.0047,
            HEN=0.0047,
            IND=0.0046
        })
    }
}

--blockweise zählt nur 1 bis n,n+1 bis 2n usw.
--wenn das false gesetzt ist, wird 1 bis n, 2 bis n+1 usw. gezählt
ngramm_hauefig = function(x,n,blockweise)
    x = x:upper()
    local schritt = (blockweise and n) or 1
    local tab = Ngrammtab({})
    for i=1,x:len()-n+1,schritt do
        tab[x:sub(i,i+n-1)] = 
            (tab[x:sub(i,i+n-1)] and tab[x:sub(i,i+n-1)]+1) or 1
    end
    return tab 
end

ngramm_anteil = function(x,n,blockweise)
    local tab = ngramm_hauefig(x,n,blockweise)
    local gesngr = (blockweise and div(x:len(),n)) or x:len()-n+1
    return Ngrammtab(map(function(x) return x/gesngr end,tab))
end


--Schätze eine Wkt. dafür das ein String aus Sprache spr  String x 
--ist. Dafür die (falsche) Annahmen benutzt, dass n-Gramme
--nacheimander gezogen werden.
--Falls möglich nuzten wir Bi- und Trigramme für eine
--bessere Schätzung, dies muss für z.B. Teilschlüssel der
--Vigenère-Chiffre abgestellt werden, da dort n-Gramme nicht
--aus dem Klartext stammen. Siehe auch Aufgabe 23 (Blatt 5).
sprachpassend = function(x,spr,ngrnutzen)
    ngrnutzen = ngrnutzen or true
    x = x:upper()
    local pos,pass = 1, 1
    while pos <= x:len() do
        if ngrnutzen and pos+3 <= x:len() and 
                spr.tri[x:sub(pos,pos+2)] then
            pass = pass * spr.tri[x:sub(pos,pos+2)] * 26^3
            pos = pos + 3
        elseif ngrnutzen and pos+2 <= x:len() and 
                spr.tri[x:sub(pos,pos+1)] then
            pass = pass * spr.bi[x:sub(pos,pos+1)] * 26^2
            pos = pos + 2
        else
            pass = pass * spr.mono[x:sub(pos,pos)] * 26
            pos = pos + 1
        end
    end
--     print(pass)
    return pass
end


IC = function(y)
    local tab = ngramm_hauefig(y,1)
    return sum(map(function(x)return x*(x-1) end,tab)) /
        (y:len()*(y:len()-1))
end

ICgem = function(x,y)
    local xtab = ngramm_hauefig(x,1)
    local ytab = ngramm_hauefig(y,1)
    return sum(map(function(p,a)return p*(ytab[a]or 0) end,xtab)) /
        (x:len()*y:len())
end

ICL = function(spr)
    return spr.icl or sum(map(function(x)return x^2 end,spr.mono))
end

erwICnd = function(d,n,m,spr)
    local icl = ICL(spr)
    return ((n-d)/(d*(n-1))) * icl + (n*(d-1))/(d*(n-1)) * (1/m)
end


vigenereFixD = function(y,spr,d,nutzeICgem)
    local spalten = spliceStr(y:upper(),d)
    local k,x = {},nil
    if nutzeICgem then
        for i=2,d do
            local deltamax = 0
            local icgmax = 0
            for delta in Alpha.all() do
                local spalte_delta = affin_ent(
                    alphaArrayToString({Alpha.B,delta}),spalten[i])
                local icg = ICgem(spalten[1],spalte_delta)
                if icg > icgmax then
                    icgmax = icg
                    deltamax = delta
                end
            end
            k[i] = deltamax--temporär, später korrekter Schlüssel
            spalten[i] = affin_ent(
                alphaArrayToString({Alpha.B,deltamax}),spalten[i])
        end
        y = despliceStr(spalten)
        k[1],x = additivpassend(y,spr)-- dürfen n-Gr nutzen
        for i=2,d do
            k[i] = k[i]+k[1]
        end
        k = alphaArrayToString(k)
    else
        for i=1,d do
            --false: keine n-Gramme nutzen, da nicht im Klartext
            --zusammenstehend
            k[i] = additivpassend(spalten[i],spr,false)
        end
        k = alphaArrayToString(k)
        x = vigenere_ent(k,y)
    end
    return k,x

end

--Wähle d mit maximalem mittleren IC über alle Spalten von y
--(eine Spalte ist der Teilstring an gleichverschlüsselten 
--Positionen)
vigenereICmax = function(y,spr,dlimit,nutzeICgem,dstart)
    local icmax,dmax = 0, 0
    dstart = dstart or 1
    for d=dstart,dlimit do
       local spalten = spliceStr(y:upper(),d)
       local mittelic = (1/d)*sum(map(IC,spalten))
       if icmax < mittelic then
           icmax = mittelic
           dmax = d
       end
    end
    return dmax, vigenereFixD(y,spr,dmax,nutzeICgem)
end


--Wähle d mit minimaler Differenz |E_d(IC(Y))-IC(Y)|
vigenereICerw = function(y,spr,dlimit,nutzeICgem,fenster)
    local icdiff,dmin = 1, 0
    local icy = IC(y)
    fenster = fenster or 0
    for d=1,dlimit do
       local erwic = erwICnd(d,y:len(),26,spr)
       if icdiff > math.abs(icy-erwic) then
           icdiff = math.abs(icy-erwic)
           dmin = d
       end
    end
    if fenster > 0 then
        return vigenereICmax(y,spr,dmin+fenster,nutzeICgem,
            dmin-fenster)
    else
        return dmin, vigenereFixD(y,spr,dmin,nutzeICgem)
    end
end

Lab = function(sbox,a,b)
    local count=0
    local abits = #a
    for i=0,2^(abits)-1 do
        local x = intToBitArray(i,abits)
        local y = sbox(x)
        x = a*x
        y = b*y
        count = (y:par() == x:par() and count+1) or count
    end
    return count
end

Dab = function(sbox,a,b)
    local count=0
    local abits = #a
    for i=0,2^(abits)-1 do
        local x = intToBitArray(i,abits)
        local x2 = x + a
        local y = sbox(x)
        local y2 = sbox(x2)
        count = (y2+y == b and count+1) or count
    end
    return count
end

hextable = function(sbox,Xab)
    local s = ""
    for ia=0,15 do
        local a = intToBitArray(ia,4)
        for ib=0,15 do
            local b = intToBitArray(ib,4)
            local c = Xab(sbox,a,b)
            s = s .. hex[c] .. "  "
        end
        s = s .. "\n"
    end
    return s
end
