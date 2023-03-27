program main 
    use hilbert
    use hilbert_module


    integer, dimension(9) :: array
    integer :: x = 3, y = 5
    integer :: i, j, n = 4
    ! integer :: x, y
    ! array = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(array))
    ! array = (/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /)

    ! print*, compact_h_to_p(1, array)

    ! print*, x,y

    ! x = xor(x, y)
    ! y = xor(x, y)
    ! x = xor(x, y)

    ! x = or(x, y)
    ! x = (x > 0)

    ! print*, x


    do i=0, n-1
        do j=0, n-1
            x = i
            y = j
            ! print*, i, j
            print*, '(',x, y,')',  xy2d(n, x, y)
        end do 
    end do 


end program