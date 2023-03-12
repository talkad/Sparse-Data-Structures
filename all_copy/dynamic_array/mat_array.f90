module mat_array_module
    use data_abstract_module
    use dynamic_array_module

    type, extends(data_abstract_t) :: mat_array_t
        private 
        type(dynamic_array_t), dimension(:,:), allocatable :: mat

        integer :: curr_i, curr_j
        integer :: curr, up, down, left, right

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: get_neighbors => get_neighbors

    end type

    interface mat_array_t
        module procedure mat_array_constructor
    end interface

    contains

    function mat_array_constructor(matrix)
        implicit none
        type(mat_array_t), pointer :: mat_array_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer :: i, j, nx, ny, materials

        nx = size(matrix, dim=2)-1
        ny = size(matrix, dim=3)-1

        allocate(mat_array_constructor)
        allocate(mat_array_constructor%mat(0:nx,0:ny))

        mat_array_constructor%curr_i = 0
        mat_array_constructor%curr_j = 0
        mat_array_constructor%curr = 0
        mat_array_constructor%up = 0
        mat_array_constructor%down = 0
        mat_array_constructor%left = 0
        mat_array_constructor%right = 0

        do j = 0, ny
            do i = 0, nx
                mat_array_constructor%mat(i,j) =  dynamic_array_constructor(matrix(:,i,j))
            end do
        end do
    end function


    subroutine add_item(this, material_type, i, j, val)
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! irrelevant
    end subroutine


    function get_item(this, material_type, i, j)
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item
        
        get_item = 0.0

        if (.not. allocated(this%mat(i, j)%types)) return 
    
        if (this%curr_i /= i .or. this%curr_j /= j) then
            this%curr_i = i
            this%curr_j = j

            this%curr = 0
        end if

        if (this%mat(i, j)%types(this%curr) == material_type) then
            get_item = this%mat(i, j)%values(this%curr)
            this%curr = this%curr + 1
        end if
    end function get_item


    subroutine get_neighbors(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(mat_array_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val

        curr_val=0.0
        upper_val=0.0
        lower_val=0.0
        left_val=0.0
        right_val=0.0

        if (i /= this%curr_i .or. j /= this%curr_j) then
            this%curr_i = i
            this%curr_j = j

            this%curr = 0
            this%up = 0
            this%down = 0
            this%left = 0
            this%right = 0
        end if

        if (allocated(this%mat(i, j)%types) .and. this%mat(i, j)%types(this%curr) == material_type) then
            curr_val = this%mat(i, j)%values(this%curr)
            this%curr = this%curr + 1
        end if

        if (allocated(this%mat(i-1, j)%types) .and. this%mat(i-1, j)%types(this%up) == material_type) then
            upper_val = this%mat(i-1, j)%values(this%up)
            this%up = this%up + 1
        end if

        if (allocated(this%mat(i+1, j)%types) .and. this%mat(i+1, j)%types(this%down) == material_type) then
            lower_val = this%mat(i+1, j)%values(this%down)
            this%down = this%down + 1
        end if

        if (allocated(this%mat(i, j-1)%types) .and. this%mat(i, j-1)%types(this%left) == material_type) then
            left_val = this%mat(i, j-1)%values(this%left)
            this%left = this%left + 1
        end if

        if (allocated(this%mat(i, j+1)%types) .and. this%mat(i, j+1)%types(this%right) == material_type) then
            right_val = this%mat(i, j+1)%values(this%right)
            this%right = this%right + 1
        end if

    end subroutine

end module