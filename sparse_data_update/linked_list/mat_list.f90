module mat_list_module
    use data_abstract_module
    use linked_list_module

    type, extends(data_abstract_t) :: mat_list_t
        private 
        type(linked_list_t), dimension(:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: get_neighbors => get_neighbors
            
            procedure :: update_struct => update_struct

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

        get_item = this%mat(i,j)%get(material_type)
    end function get_item


    subroutine update_struct(this, ms, is, js, vals)
        class(mat_list_t) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: idx

        do idx=1, size(is)
            if (vals(idx) == 0) then
                call this%mat(is(idx), js(idx))%remove_mat(ms(idx))
            else
                call this%mat(is(idx), js(idx))%update_mat(ms(idx), vals(idx))
            end if
        end do

    end subroutine

    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(mat_list_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val

        integer :: nx, ny

        nx = size(this%mat, dim=1) - 1
        ny = size(this%mat, dim=2) - 1

        curr_val= 0d0
        upper_val=0d0
        lower_val=0d0
        left_val= 0d0
        right_val=0d0

        curr_val= this%mat(i,j)%get(material_type)
        if (i-1 >= 0) upper_val=this%mat(i-1,j)%get(material_type)
        if (i+1 <= nx) lower_val=this%mat(i+1,j)%get(material_type)
        if (j-1 >= 0) left_val= this%mat(i,j-1)%get(material_type)
        if (j+1 <= ny) right_val=this%mat(i,j+1)%get(material_type)

    end subroutine

end module