permutation_table = {
        0  : 0,
        1  : 4,
        2  : 2,
        3  : 1,
        4  : 12,
        5  : 6,
        6  : 3,
        7  : 13,
        8  : 10,
        9  : 5,
        10 : 14,
        11 : 7,
        12 : 15,
        13 : 11,
        14 : 9,
        15 : 0
        }

inverse_permutation_table = {
        0 : 0,
        4 : 1,
        2 : 2, 
        1 : 3,
        12: 4,
        6 : 5,
        3 : 6,
        13: 7,
        10: 8,
        5 : 9,
        14: 10,
        7 : 11,
        15: 12,
        11: 13,
        9 : 14,
        0 : 15
        }

def linearAttack(pairs, permutationFunc):
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
                u4_first_block = inverse_permutation_table[v4_first_block]
                u4_third_block = inverse_permutation_table[v4_third_block]
                z = (x & 1) ^ (u4_first_block >> 3) ^ (u4_third_block >> 3)
                if (z == 0):
                    count[(a,b)] += 1
    max_num = -1
    for a in range(0,16):
      for b in range(0,16):
          count[a,b] = abs(count[a,b] - 8)
          if count[a,b] > max_num:
              max_num = count[a,b]
              max_key = (a,b)
    return (a,b)
     




                


