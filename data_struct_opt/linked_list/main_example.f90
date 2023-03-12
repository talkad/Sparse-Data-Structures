program main
    use data_abstract_module
    use linked_list_module

    ! type(mat_list_t) :: mat_list
    type(linked_list_t) :: list
    type(link_t), pointer :: mat1, mat2, mat3



    ! list example
    allocate(mat1, mat2, mat3)

    list = list_constructor()
    call mat1%link_init(1, 1d0)
    call mat2%link_init(2, 2d0)
    call mat3%link_init(3, 3d0)

    call list%add(mat1)
    call list%add(mat2)
    call list%add(mat3)

    print*, list%head%val
    print*, list%head%next%val

    


    ! matrix of lists example
    ! mat_list = mat_list_constructor(1000, 1000)

    ! call mat_list%add_material(10, 10, 5, 5d0)
    ! print*, mat_list%get_material(10,10,5)
    ! print*, mat_list%get_material(10,10,3)
    ! print*, mat_list%get_material(2,2,5)


end program