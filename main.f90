
PROGRAM MAIN  

    integer, dimension(:), allocatable :: array
    

    allocate(array(1:3))
    array(1) = 1
    array(2) = 2
    array(3) = 3

    call func(array)

    
END PROGRAM MAIN

subroutine func(array)
    integer, dimension(:), pointer :: array
    ! integer, dimension(:), pointer :: arr_ptr

    ! arr_ptr => array

    ! print*, array

    ! print*, 'aaa'
end subroutine
