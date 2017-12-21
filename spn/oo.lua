makeclass = function(c)
    setmetatable(c, {
        __call = function (cls, ...)
            return cls.new(...)
        end,
    })
    --Klassen sind später Metatabellen und Fallbackindex
    --normale und Metamethoden können so in derselben
    --Tabelle definiert werden
    if c.__index == nil then
        c.__index = c
    end

    if c.__concat == nil then
        c.__concat = concatWithToString
    end
end

concatWithToString = function(t1,t2)
    return  tostring(t1) ..  tostring(t2)
end

--kopiere Tabelle bis Tiefe l (Schlüssel bleiben Reeferenzen!)
--Rekursionstiefe ~= 1 angeben
dcopy = function(t,l)
    l = l or 1
    if type(t) == 'table' and l > 0 then
        local ret = {}
        for k,v in pairs(t) do
            ret[k] =  dcopy(v,l-1)
        end
        return ret
    else
        return t
    end
end

--nur Debugging, Rekursionstiefe ~= 1 angeben
dump = function(t,l)
    l = l or 1
    if type(t) == 'table' and l > 0 then
        local s = "{"
        for k,v in pairs(t) do
            s = s .. dump(k,l-1) .. ":"  .. dump(v,l-1) .. ", "
        end
        return s .. "}"
    else
        return tostring(t)
    end
end

--nur Debugging, Rekursionstiefe ~= 1 angeben
dumpNoKeys = function(t,l)
    l = l or 1
    if type(t) == 'table' and l > 0 then
        local s = "{"
        for k,v in pairs(t) do
            s = s  .. dumpNoKeys(v,l-1) .. ", "
        end
        return s .. "}"
    else
        return tostring(t)
    end
end

--Iteratoren mit Zustand aus zustandslosen
--praktisch für die Verwendung außerhalb von for
statefull = function(sl,t)
    local it, s, k = sl(t)
    return function()
        local vals = {it(s,k)}
        k = vals[1]
        return unpack(vals)
    end
end

rangemany = function(it,rmin,rmax)
    local i = 0
    while it() and i <= rmax+1 do
        i=i+1
    end
    return i >= rmin and i <= rmax
end

tablerange = function(t,f,l)
    local ret = {}
    for i=f,l do
        ret[#ret+1] = t[i]
    end
    return ret
end

settablerange = function(t,f,r)
    f=f-1
    for i=1,#r do
        t[i+f] = r[i]
    end
end

--Tabelle der Schlüssel
keys   = function(t)
    local ret = {}
    for k,v in pairs(t) do
        ret[k]=k
    end
    return ret
end

--Tabelle der Schlüssel, numerisch geordnet
ikeys   = function(t)
    local ret = {}
    for k,v in pairs(t) do
        ret[#ret+1]=k
    end
    return ret
end

--Tabelle der Schlüssel, numerisch geordnet, sortiert nach Funktion f
--f(k1,k2,t) erhält zu verlgleichende Schlüssel und die Tabelle
--gibt true zurück, falls k1 vor k2 stehen soll
ikeyssort = function(t,f)
    local ret = {}
    for k,v in pairs(t) do
        ret[#ret+1]=k
    end
    local fs = function(k1,k2)
        return f(k1,k2,t)
    end
    table.sort(ret,fs)
    return ret
end

numval = function(how)
    if how == "asc" then
        return function(k1,k2,t)
            return (t[k1] < t[k2]) or
                    (t[k1] == t[k2] and k1 < k2)
        end
    else
        return function(k1,k2,t)
            return (t[k1] > t[k2]) or
                    (t[k1] == t[k2] and k1 < k2)
        end
    end
end

--map übergibt auch den jew. Schlüssel, aber als 2. Argument
--Fkt. f mit nur einem Argument erhalten also nur den Wert
map = function(f,t)
    local ret = {}
    for k,v in pairs(t) do
        ret[k] = f(v,k)
    end
    return ret
end

mapi = function(f,it)
    local i = it()
    return function()
        return f(i())
    end
end

mapmod = function(f,t)
    for k,v in pairs(t) do
        t[k] = f(v,k)
    end
    return t
end

--auch reduce kann die Schlüssel nutzen
reduce = function(f,t,init)
    local ret = init
    for k,v in pairs(t) do
        ret = f(ret,v,k)
    end
    return ret
end

prod = function(t)
    return reduce(function(x,y) return x*y end,t,1)
end

sum = function(t)
    return reduce(function(x,y) return x+y end,t,0)
end

whichmax = function(t)
    return reduce(
        function(maxpair,v,k)
            return (maxpair.v < v and {v=v,k=k}) or maxpair
        end,t,{v=0,k=0}
    )
end

--Transponieren einer 2dim-Tabelle (d.h. Tab. von Tabellen)
--mit jeweils numerischen Schlüsseln
transp = function(t,inplace)
    local ret = {}
    for j=1,#(t[1]) do
        ret[j] = {}
        for i=1,#t do
            ret[j][i] = t[i][j] 
        end
    end
    return ret
end

blocks  = function(t,l)
    local bl = {}
    for i=1,#t do
        if i % l == 1 or l==1 then
            bl[#bl+1] = {}
        end
        bl[#bl][((i-1) % l)+1] = t[i]
    end
    return bl
end

splice  = function(t,l)
    local spl = {}
    for i=1,l do
        spl[i] = {}
    end
    for i=1,#t do
        local part = spl[((i-1) % l)+1]
        part[#part+1] = t[i]
    end
    return spl
end

desplice  = function(spl)
    local t,l,i = {},#spl,0
    local last = ""
    while last do
        i=i+1
        t[i] = spl[((i-1) % l)+1][((i-1)-((i-1)%l))/l+1]
        last = t[i]
    end
    return t
end

spliceStr  = function(s,l)
    local spl = {}
    for i=1,l do
        spl[i] = ""
    end
    for i=1,s:len() do
        local part = spl[((i-1) % l)+1]
        part = part .. s:sub(i,i)
        spl[((i-1) % l)+1] = part
    end
    return spl
end

despliceStr  = function(spl)
    local s,l,i = "",#spl,0
    local last = "-"
    while last ~= "" do
        i=i+1
        local j = ((i-1)-((i-1)%l))/l+1
        last = spl[((i-1) % l)+1]:sub(j,j)--gibt "" zurück,für j>len
        s = s .. last
    end
    return s
end

choose = function(t,n,tmax,tshift,nshift)
    tshift = tshift or 0
    nshift = nshift or 0
    tmax = tamx or #t
    local pos,nlit = tshift,nil
    if n ~= 1 then
        nlit = choose(t,n-1,tmax,tshift+1,nshift+1)
        pos=pos+1
        return function()
            local set = nlit()
            if (not set) and pos < tmax-(n-1) then
                pos = pos + 1
                nlit=choose(t,n-1,tmax,pos,nshift+1)
                set = nlit()
            elseif (not set) and pos >= tmax-(n-1) then
                return nil
            end
            set[nshift+1] = t[pos]
            return set
        end
    else
        return function()
            pos = pos + 1
            return ((pos <= tmax and {[nshift+1]=t[pos]}) or nil)
        end
    end
end

perm = function(t,npos)
    local k = ikeys(t)
    local pos = 0
    npos = npos or 1
    if #k ~= 1 then
        local nlit = nil
        return function()
            local set = nlit and nlit()
            if (not set) and pos < #k then
                pos = pos + 1
                local tc = dcopy(t)
                tc[k[pos]] = nil
                nlit=perm(tc,npos+1)
                set = nlit()
            elseif (not set) and pos >= #k then
                return nil
            end
            set[npos] = t[k[pos]]
            return set
        end
    else
        local once = true
        return function()
            if once then
                once = false
                return {[npos]=t[k[1]]}
            else
                return nil
            end
        end
    end
end

kperm = function(t,k)
    local chooseit = choose(t,k)
    local permit = nil
    return function()
        local kpm = permit and permit()
        if not kpm then
            local pm = chooseit()
            if not pm then
                return nil
            end
            permit = perm(pm)
            kpm = permit()
        end
        return kpm
    end
end


--table.maxn scheint bei texlua zu fehlen
if not table.maxn then
    table.maxn = function(t)
        local max=0
        for i,v in ipairs(t) do
            if i > max then
                max = i
            end
        end
        return max
    end
end
