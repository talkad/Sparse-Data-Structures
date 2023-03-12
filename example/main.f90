program main
    use sparse_matrix_module

    real, dimension(:,:), allocatable :: M
    type(coo_matrix_t) :: csr_matrix

    allocate(M(3,3))
    M = reshape((/0,2,3,4,0,6,7,8,0/), (/3,3/))

    csr_matrix = csr_matrix_t(M)

    print*, csr_matrix%get(2,2)

end program