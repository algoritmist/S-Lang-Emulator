main:
    addI a0 zero 16
    call input
    halt
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
        ret