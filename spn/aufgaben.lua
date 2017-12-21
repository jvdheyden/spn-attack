require "grundl"
require "krypto"
require "analyse"

local hillvl = Matrix(stringToAlphaArray("LNIVYRDZSMXRGPCP"),4)
--sollen Reste modulo 26 als Zahlen dargstellt werden?
--Standard ist Buchstaben
Alpha.printAsNumbers = true

aufgabe9 = function()
    print(affin_ver("BN","DREIEINS"))
    print(affin_ver("RG","DREIEINS"))
    print(vigenere_ver("TIM","DREIEINS"))
    print(hill_ver("LNIVYRDZSMXRGPCP","DREIEINS"))
end

aufgabe11 = function()
    local mat = Matrix(stringToAlphaArray("NCCCNCNCN"),3)
    print(mat:det())
    print(mat:detCRS())
    print(mat:gaussZusGes())
end

aufgabe14BruteForce = function()
    local zaehler = 0
    for a=0,25 do for b=0,25 do for c=0,25 do for d=0,25 do
        local m = Matrix({a,b,c,d},2,26)
        if m*m == Matrix.eins(2,26) then
            zaehler = zaehler + 1
--             print(m)
        end
    end end end end
    print(zaehler)
end

aufgabe15 = function()
    local x = "STEFFENFREUND"
    local k = "FCK"

    print(vigenere_ver(k,x))
    print(beaufort(k,x))
    print(autokeyklar_ver(k,x))
    print(autokeykrypto_ver(k,x))
end

aufgabe16 = function()
    print(hillklartext("CONSPIRACIES","RPETVTZADECM",1))
    print(hillklartext("CONSPIRACIES","RPETVTZADECM",2))
    print(hillklartext("CONSPIRACIES","RPETVTZADECM",3))
    print(hillklartext("CONVERSATION","HIARRTNUYTUS",2))
    print(hillklartext("CONVERSATION","HIARRTNUYTUS",3))
    print(hillklartext("CONVERSATION","HIARRTNUYTUS",4))
    
    
    local krypt="LMQETXYEAGTXCTUIEWNCTXLZEWUAISPZY"..
        "VAPEWLMGQWYAXFTCJMSQCADAGTXLMDXNXSNP"..
        "JQSYVAPRIQSMHNOCVAXFV"
    local bigr = ngramm_anteil(krypt,2,true)--true:blockweise
    print(bigr)
    print(lang.en.bi)
    
    
    local kand,kandx,kandst = hillngrammblock(krypt,2,lang.en)
    for i=1,#kand do
        print(kandst[i])
        print(kand[i])
        print(kandx[i])
    end

end

aufgabe19 = function()
    local y = 
    "KCCPKBGUFDPHQTYAVINRRTMVGRKDNBVFDETDGILTXRGUDDKOTFMBPVG" ..
    "EGLTGCKQRACQCWDNAWCRXIZAKFTLEWRPTYCQKYVXCHKFTPONCQQRHJVAJUWET" ..
    "MCMSPKQDYHJVDAHCTRLSVSKCGCZQQDZXGSFRLSWCWSJTBHAFSIASPRJAHKJRJ" ..
    "UMVGKMITZHFPDISPZLVLGWTFPLKKEBDPGCEBSHCTJRWXBAFSPEZQNRWXCVYCG" ..
    "AONWDDKACKAWBBIKFTIOVKCGGHJVLNHIFFSQESVYCLACNVRWBBIREPBBVFEXO" ..
    "SCDYGZWPFDTKFQIYCWHJVLNHIQIBTKHJVNPIST"
    print(vigenereICmax(y,lang.en,20))--Spalten individ. entschlüsseln
    print(vigenereICmax(y,lang.en,20,true))--gemeins. IC
    print(vigenereICerw(y,lang.en,20))--Spalten individ. entschlüsseln
    print(vigenereICerw(y,lang.en,20,true))--gemeins. IC
    --gemeinsamer IC funktioniert hier bei langen Spalten ebenso gut
    --bei kurzen (d=12 statt 6) schlechter
    --Das ist nicht überraschend, da bei gem. IC und kurzen Spalten
    --der Stichprobenumfang für beide Verteilungen zu klein ist,
    --der Ansatz über die Sprachverteilung liefert zumindest
    --eine gute Verteilung
end

aufgabe26 = function()
    print(Alpha.R)
    print(hillklartext("HAUSBOTVERWALTER","GEGENTORSCHUETZE",2))
    print(hillklartext("NERD","FAKT",2,true))
end

aufgabe33 = function()
    local sbox = sboxHex("8421C63DA5E7FB90")
    print(hextable(sbox,Lab))
-- gibt aus: (Zeilen a, Spalten b, "G" ist 16)
-- G  8  8  8  8  8  8  8  8  8  8  8  8  8  8  8  
-- 8  A  6  8  A  8  8  6  4  6  6  8  A  8  4  A  
-- 8  A  8  A  6  8  6  8  6  8  A  4  4  6  8  A  
-- 8  8  A  A  8  C  A  6  6  6  8  8  A  6  C  8  
-- 8  A  8  6  8  A  8  6  A  4  A  8  6  8  6  4  
-- 8  C  6  6  A  A  8  C  6  A  8  8  8  8  A  6  
-- 8  8  C  8  A  A  6  A  8  8  8  C  6  6  6  A  
-- 8  6  6  8  C  6  A  8  8  6  6  8  4  6  A  8  
-- 8  A  A  8  8  6  6  8  A  8  4  6  A  4  8  6  
-- 8  8  8  C  A  A  6  A  A  6  6  6  8  C  8  8  
-- 8  C  A  A  6  6  C  8  8  8  6  A  6  A  8  8  
-- 8  6  C  6  8  6  8  A  4  6  8  6  8  A  8  6  
-- 8  8  A  A  C  8  A  6  8  C  A  6  8  8  6  6  
-- 8  6  8  6  6  C  A  8  8  A  4  6  6  8  6  8  
-- 8  6  6  C  6  8  8  A  6  8  8  A  8  6  6  4  
-- 8  8  8  8  8  8  C  C  A  6  A  6  A  6  6  A
end


aufgabe41a = function()
    local sbox = sboxHex("E213D906F45A8C7B")
    print(hextable(sbox,Dab))
-- gibt aus: (Zeilen a, Spalten b, "G" ist 16)
-- G  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  
-- 0  0  2  0  4  0  2  0  0  0  0  2  4  0  0  2  
-- 0  2  0  0  0  0  0  2  0  0  2  0  0  2  2  6  
-- 0  2  0  4  0  2  0  0  0  2  0  4  0  2  0  0  
-- 0  4  2  2  0  2  0  2  2  0  0  2  0  0  0  0  
-- 0  0  0  4  0  0  0  4  0  0  0  0  2  2  2  2  
-- 0  0  0  0  2  0  2  0  2  0  2  0  2  2  2  2  
-- 0  0  4  2  2  0  0  0  4  2  0  0  0  0  2  0  
-- 0  2  0  0  2  4  2  2  0  2  0  0  0  2  0  0  
-- 0  6  0  0  0  0  2  0  0  0  2  4  0  2  0  0  
-- 0  0  2  0  0  0  0  2  4  0  4  2  0  0  2  0  
-- 0  0  0  0  2  2  2  2  0  0  0  0  4  0  4  0  
-- 0  0  2  0  0  2  4  0  2  0  0  0  2  2  2  0  
-- 0  0  2  2  2  0  2  0  0  2  6  0  0  0  0  0  
-- 0  0  2  2  0  0  0  0  2  6  0  0  0  0  0  4  
-- 0  0  0  0  2  4  0  2  0  2  0  2  2  2  0  0
end


