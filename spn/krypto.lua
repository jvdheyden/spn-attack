require "oo"
require "grundl"

--folgende Funktionen wandeln von/nach Strings nach/von Z_26-Arrays
--falls die ursprüngliche Einagbe aus Strins besteht
--intern rechnen alle Funktionen mit Arrays aus RRElement
arrOrString = function(a,onStrings)
    if onStrings then
        return alphaArrayToString(a)
    else
        return a
    end
end

arrOrStringArg = function(a,b)
    local onStrings = (type(b) == 'string')
    if onStrings then
        a = stringToAlphaArray(a)
        b = stringToAlphaArray(b)
    end
    return a,b,onStrings
end

--Alle Funktionen erwarten entweder Strings oder Elem. desselben RR
--k ist ein Paar (a,b), sodass affin(k,x) = ax+b (für x d.L. 1)
affin_ver = function(k,x)
    local onStrings
    k,x,onStrings = arrOrStringArg(k,x)
    local y = {}
    for i=1,#x,1 do
        y[i] = k[1]*x[i] + k[2]
    end
    return arrOrString(y,onStrings)
end

--Wir führen affin_ent auf affin_ver zurück
--y=bx+c <=> b^(-1)y + (-b^(-1)c) = x 
affin_ent = function(k,y)
    local onStrings
    k,y,onStrings = arrOrStringArg(k,y)
    
    k[1] = k[1]^(-1)--inverses b berechnen
    if not finite(k[1]) then
        return "ungültiger Schlüssel"
    end
    k[2] = -k[1]*k[2]-- c aus *inversem* b
    
    return arrOrString(affin_ver(k,y),onStrings)
end

--auch Optionen für Beaufort und Autokey sowie Entschlüsselung
vigenere_ver = function(k,x,xfaktor,kfaktor,autokey)
    xfaktor = xfaktor or 1
    kfaktor = kfaktor or 1
    local onStrings
    k,x,onStrings = arrOrStringArg(k,x)
    local y = {}
    for i=1,#x,1 do
        y[i] = kfaktor*k[((i-1)%#k)+1] + xfaktor*x[i]
        if autokey == "klar" then
            k[#k+1] = x[i]
        elseif  autokey == "krypto" then
            k[#k+1] = y[i]
        end
    end
    return arrOrString(y,onStrings)
end

vigenere_ent = function(k,y)
    local onStrings
    k,y,onStrings = arrOrStringArg(k,y)
    for i=1,#k do
        k[i] = -k[i]
    end
    return arrOrString(vigenere_ver(k,y,1),onStrings)
end

beaufort = function(k,x)
    return vigenere_ver(k,x,-1)
end

autokeyklar_ver = function(k,x)
    return vigenere_ver(k,x,1,1,"klar")
end

autokeyklar_ent = function(k,y)
    return vigenere_ver(k,y,1,-1,"krypto")
end

autokeykrypto_ver = function(k,x)
    return vigenere_ver(k,x,1,1,"krypto")
end

autokeykrypto_ent = function(k,y)
    return vigenere_ver(k,y,1,-1,"klar")
end

--k ist eine Matrix, sodass hill(k,x) = kx (für x der dimension)
hill_ver = function(k,x)
    local onStrings
    k,x,onStrings = arrOrStringArg(k,x)
    k = (getmetatable(k) == Matrix and k) or
        Matrix(k,math.sqrt(#k))--sicherstellen, dass k Matrix ist
   
    if type(x[1]) == "number" then
        for i=1,#x do
            x[i] = RRElement(k.ring,x[i])
        end
    end
    if #x % k.nrow ~= 0 then
        for i=#x+1,#x+(k.nrow-(#x % k.nrow)) do
            x[i] = RRElement(k.ring,0)--padde x, falls zu kurz
        end
    end
    local y = {}
    for b=1,#x/k.nrow,1 do
        local xblock = 
            Matrix(tablerange(x,(b-1)*k.nrow+1,b*k.nrow),k.nrow)
        local yblock = xblock*k
        --Zeile 1 von 1 der Matrix yblock wird angehangen
        settablerange(y,(b-1)*k.nrow+1,yblock[1])
    end
    return arrOrString(y,onStrings)
end

hill_ent = function(k,y)
    local onStrings
    k,y,onStrings = arrOrStringArg(k,y)
    k = (getmetatable(k) == Matrix and k) or
        Matrix(k,math.sqrt(#k))--sicherstellen, dass k Matrix ist
    
    k = k:inv()
    if not k then
        return "ungültiger Schlüssel"
    end
    return arrOrString(hill_ver(k,y),onStrings)
end


blocktrans_ver = function(k,x,ent)
    local onStrings
    k,x,onStrings = arrOrStringArg(k,x)
    local mat = Matrix(#k,#k,k.ring)
    for i,j in pairs(k) do
        if ent then
            mat[i][j.val] = 1
        else
            mat[j.val][i] = 1
        end
    end
    return arrOrString(hill_ver(mat,x),onStrings)
end

blocktrans_ent = function(k,y)
    return blocktrans_ver(k,y,true)
end


sboxHex = function(s)
    local box = {}
    for i=0,15 do
        box[hex[i]] = s:sub(i+1,i+1)
    end
    return function(x)
        x = bitArrayToHex(x)
        x = box[x]
        return hexToBitArray[x]
    end
end

kgenShift = function(k,rund,l)
    local karr = {}
    local letztStartOffset = #k - l --32-16 = 16 in VL
    local offsetShift = letztStartOffset/rund --16/4=4 in VL
    for i=0,rund do
        karr[i+1] = 
            Bitarray(tablerange(k,i*offsetShift+1,i*offsetShift+16))
    end
    return karr
end

spn = function(l,kgen,sbox,sbits,trans,rund)
    return function(k,x)
        local karr = kgen(k,rund,l)
        x = dcopy(x)
        for i=1,rund do
            x = x + karr[i]
            for j=1,l/sbits do
                local sbin = {}
                for m=1,sbits do
                    sbin[m] = x[(j-1)*sbits+m]
                end
                local sbout = sbox(sbin)
                for m=1,sbits do
                    x[(j-1)*sbits+m] = sbout[m]
                end
            end
            if i < rund then
                local xtr = {}
                for m,n in pairs(trans) do
                        xtr[n] = x[m]
                end
                x = xtr
            else
                x = x + karr[rund+1]
            end
        end
        return x
    end
end
