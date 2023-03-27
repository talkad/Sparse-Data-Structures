module mat_list_module
    use data_abstract_module
    use linked_list_module

    type, extends(data_abstract_t) :: mat_list_t
        private 
        type(linked_list_t), dimension(:,:), allocatable :: mat
        integer :: curr_i, curr_j

        type(link_t), pointer :: curr, up, down, left, right

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

        mat_list_constructor%curr_i = -1
        mat_list_constructor%curr_j = -1

        mat_list_constructor%curr => null()
        mat_list_constructor%up => null()
        mat_list_constructor%down => null()
        mat_list_constructor%left => null()
        mat_list_constructor%right => null()

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

        get_item = 0.0

        if (i /= this%curr_i .or. j /= this%curr_j) then
            this%curr_i = i
            this%curr_j = j

            this%curr => this%mat(i, j)%head
        end if

        if (associated(this%curr)) then
            if  (this%curr%material_type == material_type) then
            get_item = this%curr%val
            this%curr => this%curr%next
            end if
        end if
    end function get_item


    subroutine update_struct(this, ms, is, js, vals)
        class(mat_list_t) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: idx

        do idx=1, size(is)
            if ms(idx) == 0 then
                ! pass
            else
                
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

        curr_val=0.0
        upper_val=0.0
        lower_val=0.0
        left_val=0.0
        right_val=0.0

        if (i /= this%curr_i .or. j /= this%curr_j) then
            this%curr_i = i
            this%curr_j = j

            this%up => null()
            this%down => null()
            this%left => null()
            this%right => null()

            this%curr => this%mat(i, j)%head
            if (i > 0) this%up => this%mat(i - 1, j)%head
            if (i < ny) this%down => this%mat(i + 1, j)%head
            if (j > 0) this%left => this%mat(i, j - 1)%head
            if (j < nx) this%right => this%mat(i, j + 1)%head
        end if

        if (associated(this%curr) .and. this%curr%material_type == material_type) then
            curr_val = this%curr%val
            this%curr => this%curr%next
        end if

        if (associated(this%up) .and. this%up%material_type == material_type) then
            upper_val = this%up%val
            this%up => this%up%next
        end if

        if (associated(this%down) .and. this%down%material_type == material_type) then
            lower_val = this%down%val
            this%down => this%down%next
        end if

        if (associated(this%left) .and. this%left%material_type == material_type) then
            left_val = this%left%val
            this%left => this%left%next
        end if

        if (associated(this%right) .and. this%right%material_type == material_type) then
            right_val = this%right%val
            this%right => this%right%next
        end if

    end subroutine

end module