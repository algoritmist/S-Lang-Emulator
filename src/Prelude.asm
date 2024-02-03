# takes a0 as size of input and returns a0 as address of string in memory
input:
    swm a0 dr 0 # set size of list to @a0
    add a1 dr zero # a1 is the address of char-list in memory
    _loop:
        jel a0 zero _end
        addI dr dr 1
        lwi dr 0
        subI a0 a0 1
        jumpl _loop
    _end:
        add a0 a1 zero
        addI dr dr 1
        ret

# takes a0 as address of list in memory
output:
    lwm t0 a0 0 # size of list
    _loop:
        jel t0 zero _end
        addI a0 a0 1
        swo a0 0
        subI t0 t0 1
        jumpl _loop
    _end:
        addI dr dr 1
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
        addI dr dr 1
        addI a0 a0 1
        lwm tr a0 0
        swm tr dr 0
        subI t0 t0 1
        jumpl _loop1
    _loop2:
        jel t1 zero _end
        addI dr dr 1
        addI a1 a1 1
        lwm tr a1 0
        swm tr dr 0
        subI t1 t1 1
        jumpl _loop2
    _end:
        add a0 t3 zero
        addI dr dr 1
        ret

# takes a0 as address of list and returns a0 as first element of list. List should be valid
head:
    lwm a0 a0 1
    ret

# takes a0 as address of list and returns a0 as address of new list
tail:
    add t3 dr zero
    lwm t0 a0 0
    addI a0 a0 1
    subI t0 t0 1
    swm t0 dr 0
    _loop:
        jel t0 zero _end
        addI a0 a0 1
        addI dr dr 1
        lwm tr a0 0
        swm tr dr 0
        subI t0 t0 4
        jumpl _loop
    _end:
        add a0 t3 zero
        addI dr dr 1
        ret

outputChar:
    swm a0 dr 0
    swo dr 0
    ret

outputInt:
    jel a0 zero _output0
    jll a0 zero _wsign
    call outputPositiveInt
    ret
    _wsign:
        swm s0 sp 0
        subI sp sp 1
        add s0 a0 zero
        addI a0 zero 45
        call outputChar
        sub a0 zero s0
        addI sp sp 1
        lwm s0 sp 0
        jumpl outputPositiveInt
    _output0:
        addI a0 zero 48
        call outputChar
        ret
# takes a0 as int and prints it
outputPositiveInt:
    jel a0 zero _end
    swm a0 sp 0
    subI sp sp 1
    divI a0 a0 10
    call outputPositiveInt
    addI sp sp 1
    lwm a0 sp 0
    modI a0 a0 10
    addI a0 a0 48
    call outputChar
    _end:
        ret

# id x = x
id:
    ret