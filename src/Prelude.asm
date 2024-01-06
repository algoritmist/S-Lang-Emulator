# takes a0 as size of input and returns a0 as address of string in memory
input:
    swm a0 dr 0 # set size of list to @a0
    add a1 dr zero # a1 is the address of char-list in memory
    _loop:
        jel a0 zero _end
        addI dr dr 4
        lwi dr rin 0
        addI rin rin 4
        subI a0 a0 4
        jumpl _loop
    _end:
        add a0 a1 zero
        addI dr dr 4
        ret

# takes a0 as address of list in memory
output:
    lwm t0 a0 0 # size of list
    _loop:
        jel t0 zero _end
        addI a0 a0 4
        swo a0 rout 0
        addI rout rout 4
        subI t0 t0 4
        jumpl _loop
    _end:
        addI dr dr 4
        ret

# takes (a0, a1) as addresses of (list1, list2)
addLists:
    add t3 dr zero # address of l3
    lwm t0 a0 0 # size of l1
    lwm t1 a1 0 # size of l2
    add t2 t0 t1 # size of |l1|+|l2|
    swm t2 dr 0 # write size of list at the beggining
    _loop1:
        jel t0 zero _loop2
        addI dr dr 4
        addI a0 a0 4
        lwm tr a0 0
        swm tr dr 0
        subI t0 t0 4
        jumpl _loop1
    _loop2:
        jel t1 zero _end
        addI dr dr 4
        addI a1 a1 4
        lwm tr a1 0
        swm tr dr 0
        subI t1 t1 4
        jumpl _loop2
    _end:
        add a0 t3 zero
        addI dr dr 4
        ret

# takes a0 as address of list and returns a0 as first element of list. List should be valid
head:
    lwm a0 a0 4
    ret

# takes a0 as address of list and returns a0 as address of new list
tail:
    add t3 dr zero
    lwm t0 a0 0
    addI a0 a0 4
    subI t0 t0 4
    swm t0 dr 0
    _loop:
        jel t0 zero _end
        addI a0 a0 4
        addI dr dr 4
        lwm tr a0 0
        swm tr dr 0
        subI t0 t0 4
        jumpl _loop
    _end:
        add a0 t3 zero
        addI dr dr 4
        ret