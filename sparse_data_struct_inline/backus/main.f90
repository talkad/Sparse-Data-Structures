program main
    use omp_lib

    ! TAL !         
    use demo_algorithms_module
    ! TAL !
    implicit none


    ! TAL !

    real(8), dimension(:,:,:,:), allocatable :: A, B, C
    integer :: nx=220, ny=220, nz=220, num_mats=8
    integer, dimension(:), allocatable :: ms, is, js, ks
    real(8), dimension(:), allocatable :: vals
    real(8) :: nz_ratio=0.3, time
    integer :: size
    integer :: idx
    character(len=32) :: arg
    integer :: i, j, k, m

    call get_command_argument(1,arg)
    read(arg,*)  num_mats

    call get_command_argument(2,arg)
    read(arg,*)  nz_ratio

    size = nz_ratio*nx*ny*nz*num_mats

    ! allocate
    allocate(ms(0:size-1))
    allocate(is(0:size-1))
    allocate(js(0:size-1))
    allocate(ks(0:size-1))
    allocate(vals(0:size-1))

    ! write(*,*) '--- main ---'
    allocate(A(1:num_mats,0:nx,0:ny,0:nz))
    allocate(B(1:num_mats,0:nx,0:ny,0:nz))
    allocate(C(1:num_mats,0:nx,0:ny,0:nz))

    ! init data
    call advection(nx, ny, nz, num_mats, ms, is, js, ks, vals, nz_ratio)

    do idx=0, size
        if (vals(idx) == 0d0) exit
        A(ms(idx), is(idx), js(idx), ks(idx)) = vals(idx)
    end do

    do idx=0, size
        if (vals(idx) == 0d0) exit
        B(ms(idx), is(idx), js(idx), ks(idx)) = vals(idx)
    end do

    print*, nz_ratio
    time = omp_get_wtime()
    do k = 0, nz
        do j = 0, ny
            do i = 0, nx
                do m = 1, num_mats
                    C(m, i, j, k) = A(m, i, j, k)*A(m, i, j, k)*1.2 + 0.5*B(m, i, j, k)
                end do
            end do
        end do
    end do
    print*, 'Result intensive_algorithm'
    print*, 'func time', omp_get_wtime() - time


    time = omp_get_wtime()
    do m = 1, num_mats
        do k = 0, nz
            do j = 0, ny
                do i = 0, nx
                    C(m, i, j, k) = A(m, i, j, k)*A(m, i, j, k)*1.2 + 0.5*B(m, i, j, k)
                end do
            end do
        end do
    end do
    print*, 'Result intensive_algorithm_mat'
    print*, 'func time', omp_get_wtime() - time

    time = omp_get_wtime()
    do k=1, nz-1
        do j = 1, ny-1
            do i = 1, nx-1
                do m = 1, num_mats
                    C(m, i, j, k) = A(m, i, j, k) + A(m, i-1, j, k) + A(m, i+1, j, k) +&
                     A(m, i, j-1, k) + A(m, i, j+1, k) + A(m, i, j, k-1) + A(m, i, j, k+1)
                end do
            end do
        end do
    end do
    print*, 'Result intensive_algorithm_neighbors'
    print*, 'func time', omp_get_wtime() - time



end program main

! ./exe 5 0.3
! 0.29999999999999999     
! Result intensive_algorithm
! func time   9.6988182514905930E-002
! Result intensive_algorithm_mat
! func time  0.31211333628743887     
! Result intensive_algorithm_neighbors
! func time   8.8446769863367081E-002
