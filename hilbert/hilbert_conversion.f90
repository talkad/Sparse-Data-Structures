module hilbert_module
! assumes cubic space of order n

private

    public :: xy2d, d2xy, rotation

contains

    pure function logical2int(logic) result(num)
        implicit none
        logical, intent(in) :: logic
        integer :: num

        num = 0
        if (logic) num = 1
    end function


    function xy2d(n, x, y) result(idx)
        implicit none
        integer, intent(in) :: n
        integer, intent(inout) :: x, y
        integer :: rx, ry, s, idx, d

        d = 0
        s = n/2

        do while (s > 0)
            rx = logical2int(and(x,s) > 0)
            ry = logical2int(and(y,s) > 0)

            d = d + s * s * xor((3*rx), ry)
            call rotation(n, x, y, rx, ry)

            s = s / 2
        end do 

        idx = d
    end function


    subroutine d2xy(n, x, y)
        ! irrelevant 
    end subroutine


    subroutine rotation(n, x, y, rx, ry)
        implicit none
        integer, intent(in) :: n, rx, ry
        integer, intent(inout) ::  x, y

        if (ry == 0) then
            if (rx == 1) then 
                x = n-1 - x
                y = n-1 -y
            end if

            x = xor(x, y)
            y = xor(x, y)
            x = xor(x, y)
        end if
    end subroutine rotation



end module