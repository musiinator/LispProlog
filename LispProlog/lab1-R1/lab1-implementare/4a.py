
def inv(lst):
    aux=lst
    if len(aux)>1:
        x=aux[0]
        del aux[0]
        inv(aux)
        aux.append(x)
    elif len(aux) == 1:
        return aux[0]
    elif len(aux) == 0:
        return 0
lst = [1, 2, 3]
aux_lst = lst
inv(lst)
for y in lst:
    print(y)