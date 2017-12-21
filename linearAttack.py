permutation_table = [8,4,2,1,12,6,3,13,10,5,14,7,15,11,9,0]

def linearAttack(pairs):
    #generate all possible keys
    l = [(a,b) for a in range(0,16) for b in range(0,16)] 
    count = dict.fromkeys(l,0)
    #go through all plaintext/cryptotext pairs and generate linear
    #approximation table
    for (x,y) in pairs:
        for a in range(0,16):
            for b in range(0,16):
                y_first_block = (y  >> 12)
                y_third_block = (y << 8) >> 4
                v4_first_block = a ^ y_first_block
                v4_third_block = b ^ y_third_block
                u4_first_block = permutation_table.index(v4_first_block)
                u4_third_block = table.index(v4_third_block)
                z = (x & 1) ^ (u4_first_block >> 3) ^ (u4_third_block >> 3)
                if (z == 0):
                    count[(a,b)] += 1
    max_num = -1
    #select key with highest absolute bias
    for a in range(0,16):
      for b in range(0,16):
          count[a,b] = abs(count[a,b] - 8)
          if count[a,b] > max_num:
              max_num = count[a,b]
              max_key = (a,b)
    return max_key
     




                


