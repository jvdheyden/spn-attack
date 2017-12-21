require "grundl"
require "krypto"
require "analyse"

local transvl = 
    {1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12 , 16}
local sboxaufg = sboxHex("8421C63DA5E7FB90")
local spnvl = spn(16,kgenShift,sboxaufg,4,transvl,3)
local kspnvl = hexStringToBitArray("4AA4")
local xspnvl = hexStringToBitArray("26B7")

print(bitArrayToHexString(spnvl(kspnvl,xspnvl)))
file = io.open("encrypted.txt", "w")
for i=1,2000 do
  x=intToBitArray(i,16)
  y=bitArrayToHexString(spnvl(kspnvl,x))
  file:write(y,'\n')
end





