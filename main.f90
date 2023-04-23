
PROGRAM MAIN  

    ! integer, dimension(:), allocatable :: array
    

    ! allocate(array(1:3))
    ! array(1) = 1
    ! array(2) = 2
    ! array(3) = 3

    ! call func(array)

    integer :: a = 5
    real :: b
    b = a / 2d0
    print*, modulo(17,3)

    
END PROGRAM MAIN

subroutine func(array)
    integer, dimension(:), pointer :: array
    ! integer, dimension(:), pointer :: arr_ptr

    ! arr_ptr => array

    ! print*, array

    ! print*, 'aaa'
end subroutine
