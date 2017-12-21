Kryptologie Blatt #8
Aufg. 40

CMS-Namen:

heydenjq
gehrinlu

Aufgabe: Programm, das Angriff auf SPN mittels linearer Kryptoanalyse ausführt.

Wir wissen:


T1 = U1_16 ^ V1_13
T2 = U2_4 ^ V2_1
T3 = U3_1 ^ V3_1 ^ V3_3

|bias(T1)| = |bias(T2)| = |bias(T3)| = 1/4

bias(T1 ^ T2 ^ T3) = 1/16

Daher können wir abschätzen, wie viele Plaintext/Kryptotextpaare benötigt werden:

T = ce^-2 = c * 256

Das Programm befindet sich in linearAttack.py. Die dazugehörigen Plaintext/Krypto-
textpaare wurden mithilfe des mitgelieferten LUA SPNs generiert. Das SPN wurde
von uns angepasst und ist unter spn/spn.lua zu finden. 

Leider ist es uns nicht gelungen, das SPN so anzupassen, dass es auch generiert
werden kann, wenn der Schlüssel nur 2 Bytes hat. Daher konnten wir leider auch
nicht den richtigen Schlüssel finden.



