program main
    use omp_lib
    use data_abstract_module
    use mat_list_module
    use algorithm_module

    class(data_abstract_t), pointer :: data_struct
    integer :: nx, ny, num_materials, i,j,m
    real(8), dimension(:,:,:), allocatable :: p
    real*8 :: time

    nx = 4000
    ny = 4000
    num_materials = 20

    data_struct => mat_list_constructor(nx, ny)

    allocate(p(0:num_materials,0:nx,0:ny))
    call init_3d(p, 0.1, num_materials, nx, ny)

    ! init data structure
    do j=0, ny
        do i=0, nx
            do m =0, num_materials
                call data_struct%add_item(m,i,j, p(m,i,j))
            end do
        end do
    end do

    time = omp_get_wtime()
    call intensive_algorithm(data_struct, nx, ny, num_materials)
    print*, 'func time', omp_get_wtime() - time
    deallocate(p)
end program

! nx * ny * 8 + (nx * ny * num_materials * nz_ratio * (sizeof(link)~25))
! 128,064,008
! 934,845,968 bytes


! intensinve
! O0 5.9454691084101796 
! O2  3.2541971812024713 

! intensinve neighbors
! O0 24.258125972002745
! O2 12.253767005167902