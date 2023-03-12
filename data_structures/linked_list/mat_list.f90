module mat_list_module
    use data_abstract_module
    use linked_list_module

    type, extends(data_abstract_t) :: mat_list_t
        private 
        type(linked_list_t), dimension(:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item

    end type

    interface mat_list_t
        module procedure mat_list_constructor
    end interface

    contains


    function mat_list_constructor(nx, ny)
        type(mat_list_t), pointer :: mat_list_constructor
        integer :: i, j

        allocate(mat_list_constructor)
        allocate(mat_list_constructor%mat(0:nx,0:ny))

        do j = 0, ny
            do i = 0, nx
                mat_list_constructor%mat(i,j) = list_constructor()
            end do
        end do
    end function


    subroutine add_item(this, material_type, i, j, val)
        class(mat_list_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val
        type(link_t), pointer :: material

        if (val == 0) return

        allocate(material)
        call material%link_init(material_type, val)

        call this%mat(i,j)%add(material)
    end subroutine


    function get_item(this, material_type, i, j)
        class(mat_list_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item

        get_item = this%mat(i,j)%get(material_type)  ! inline?
    end function get_item

end module