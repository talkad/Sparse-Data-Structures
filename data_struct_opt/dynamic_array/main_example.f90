program main
    use dynamic_array_module

    type(dynamic_array_t), pointer:: array
    real(8), dimension(:), allocatable :: p

    allocate(p(1))

    p = [1.0]

    array => dynamic_array_constructor(p)

end program