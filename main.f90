PROGRAM MAIN  
    integer :: num_materials =5
    real :: nz_ratio
    character(len=32) :: arg
    integer, dimension(:), allocatable :: arr, temp

    ! allocate(arr(4))

    ! arr = 1
    ! print*, arr

    ! allocate(temp(8))            ! enlarge array size by factor of 2
    ! temp(1:8) = 5
    ! temp(1:4) = arr(1:4)   ! copy previous values
    ! call move_alloc(temp, arr)! temp gets deallocated

    ! print*, arr
    ! call get_command_argument(1,arg)
    ! read(arg,*)  num_materials

    ! call get_command_argument(2,arg)
    ! read(arg,*)  nz_ratio

    ! print*, num_materials, nz_ratio

    print*, int(num_materials*1.5)

    
END PROGRAM MAIN