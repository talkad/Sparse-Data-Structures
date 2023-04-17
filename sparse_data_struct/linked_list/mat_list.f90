module mat_list_module
    use sparse_struct_base_module
    use linked_list_module

    type, extends(sparse_struct_base_t) :: mat_list_t
        private 
        type(linked_list_t), dimension(:,:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: update_struct => update_struct
    end type

    interface mat_list_t
        module procedure mat_list_constructor
    end interface

    contains


    function mat_list_constructor(nx, ny, nz)
        type(mat_list_t), pointer :: mat_list_constructor
        integer, intent(in) :: nx, ny, nz
        integer :: i, j, k

        allocate(mat_list_constructor)
        allocate(mat_list_constructor%mat(0:nx,0:ny,0:nz))

        do k=0, nz
            do j=0, ny
                do i=0, nx
                    mat_list_constructor%mat(i,j,k) = list_constructor()
                end do
            end do
        end do

    end function


    subroutine add_item(this, material_type, i, j, k, val)
        implicit none
        class(mat_list_t), intent(inout) :: this
        integer, intent(in) :: material_type, i, j, k
        real(8) :: val
        class(link_t), pointer :: material

        if (val == 0) return

        allocate(material)
        call material%link_init(material_type, val)
        call this%mat(i,j,k)%add(material)
    end subroutine


    function get_item(this, material_type, i, j, k)
        implicit none
        class(mat_list_t), intent(inout) :: this
        integer, intent(in) :: i, j, k, material_type
        real(8) :: get_item

        get_item = this%mat(i,j,k)%get(material_type)
    end function get_item


    subroutine update_struct(this, ms, is, js, ks, vals)
        implicit none
        class(mat_list_t), intent(inout) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ks, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: idx
        real(8) :: curr_val

        do idx=0, size(is)
            curr_val = vals(idx)

            if (curr_val == 0) then
                call this%mat(is(idx), js(idx), ks(idx))%remove_mat(ms(idx))
            else
                call this%mat(is(idx), js(idx), ks(idx))%update_mat(ms(idx), curr_val)
            end if
        end do

    end subroutine

end module