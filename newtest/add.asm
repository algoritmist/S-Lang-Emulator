jel a0 a0 main
halt
main:
addI a0 a0 42
call outputInt
halt

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
        add s0 a0 zero
        addI a0 zero 45
        call outputChar
        sub a0 zero s0
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