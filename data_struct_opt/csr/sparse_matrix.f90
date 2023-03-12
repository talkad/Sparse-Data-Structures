module sparse_matrix_module
    use data_abstract_module

    type, extends(data_abstract_t) :: coo_matrix_t
    ! private
        real(8), dimension(:), allocatable :: values
        integer, dimension(:), allocatable :: row
        integer, dimension(:), allocatable :: col
        integer, dimension(:), allocatable :: materials
        integer :: upper_bound

        integer :: curr_idx, left_idx, right_idx, lower_idx, upper_idx

    contains

        procedure :: get_item => get_item
        procedure :: add_item => add_item

        procedure, private, nopass :: append_real
        procedure, private, nopass :: append_int
        procedure, private, nopass :: idx_exists
        procedure, private, nopass :: get_boundries


        procedure, private :: next_iter
        procedure :: get_neighborhood
    end type

    interface coo_matrix_t
        module procedure sparse_constructor
    end interface


    contains

    subroutine add_item(this, material_type, i, j, val)
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: val

        ! maybe someday

    end subroutine


    function get_item(this, material_type, i, j)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8) :: get_item
        integer :: val_idx, range_min, range_max
    
        if ((material_type == this%materials(this%curr_idx)) &
                .and. (i == this%row(this%curr_idx)) &
                .and. (j == this%col(this%curr_idx))) then
            get_item = this%values(this%curr_idx)
            this%curr_idx = this%curr_idx + 1
        else
            get_item = 0.0
        end if

    end function get_item


    subroutine get_neighborhood(this, material_type, i, j, curr_val, upper_val, lower_val, left_val, right_val)
        implicit none
        class(coo_matrix_t) :: this
        integer, intent(in) :: i, j, material_type
        real(8), intent(out) :: curr_val, upper_val, lower_val, left_val, right_val

        curr_val=0.0
        upper_val=0.0
        lower_val=0.0
        left_val=0.0
        right_val=0.0

        ! print*, 'indexes a', this%col(this%curr_idx), this%row(this%curr_idx), this%materials(this%curr_idx)
        ! print*, 'current index', j, i, material_type
        call this%next_iter(i,j, this%curr_idx)
        ! print*, 'left index', j-1, i, material_type
        call this%next_iter(i,j-1, this%left_idx)
        ! print*, 'right index', j+1, i, material_type
        call this%next_iter(i,j+1, this%right_idx)

        call this%next_iter(i-1,j, this%upper_idx)
        call this%next_iter(i+1,j, this%lower_idx)

        ! print*, 'indexes b', this%col(this%curr_idx), this%row(this%curr_idx), this%materials(this%curr_idx)
            

        ! look fo neighbor values
        ! print*, j,i,material_type
        ! if (this%materials(this%curr_idx) == material_type .and. &
        !     this%row(this%curr_idx-1) == i - 1 .and. &
        !     this%col(this%curr_idx-1) == j) then 
        !     upper_val = this%values(this%curr_idx-1)
        ! end if

        ! if (this%materials(this%curr_idx+1) == material_type .and. &
        !     this%row(this%curr_idx + 1) == i + 1 .and. &
        !     this%col(this%curr_idx+1) == j) then 
        !     lower_val = this%values(this%curr_idx+1)
        ! end if
        
        if (this%materials(this%upper_idx) == material_type .and. &
            this%row(this%upper_idx) == i-1 .and. &
            this%col(this%upper_idx) == j) then
            upper_val = this%values(this%upper_idx)
            this%upper_idx = this%upper_idx + 1
        end if

        if (this%materials(this%lower_idx) == material_type .and. &
            this%row(this%lower_idx) == i+1 .and. &
            this%col(this%lower_idx) == j) then
            lower_val = this%values(this%lower_idx)
            this%lower_idx = this%lower_idx + 1
        end if

        if (this%materials(this%left_idx) == material_type .and. &
            this%row(this%left_idx) == i .and. &
            this%col(this%left_idx) == j-1) then
            left_val = this%values(this%left_idx)
            this%left_idx = this%left_idx + 1
        end if

        if (this%materials(this%right_idx) == material_type .and. &
            this%row(this%right_idx) == i .and. &
            this%col(this%right_idx) == j+1) then
            right_val = this%values(this%right_idx)
            this%right_idx = this%right_idx + 1
        end if
        
        if (this%materials(this%curr_idx) == material_type .and. &
            this%row(this%curr_idx) == i .and. &
            this%col(this%curr_idx) == j) then
            curr_val = this%values(this%curr_idx)
            this%curr_idx = this%curr_idx + 1
        end if

    end subroutine


    subroutine next_iter(this, row_idx, col_idx, idx)
        class(coo_matrix_t) :: this
        integer, intent(inout) :: idx
        integer, intent(in) :: row_idx, col_idx
        integer :: i 
        
        ! print*, 'aaaaaaaaaaaaa', idx
        ! print*, row_idx, col_idx
        do i=idx, size(this%row, dim=1)
            ! print*, this%col(i), this%row(i)
            if (this%row(i) >= row_idx .and. this%col(i) >= col_idx) exit 
        end do

        idx = i
        ! print*, 'bbbbbbbbbbbbb', idx
    end subroutine 


    function sparse_constructor(matrix)
        implicit none
        type(coo_matrix_t), pointer :: sparse_constructor
        real(8), dimension(:,:,:), intent(inout), allocatable :: matrix
        integer :: i, j, m, idx
        logical :: start_col = .False.
        integer :: mapping_length, nx, ny, materials
        real(8), parameter :: EPSILON = 0.00001
        integer :: col_init_val

        allocate(sparse_constructor)
        sparse_constructor%curr_idx = 0
        sparse_constructor%left_idx = 0
        sparse_constructor%right_idx = 0  

        sparse_constructor%lower_idx = 0  
        sparse_constructor%upper_idx = 0  

        ! print*, 'hello'
        idx = 0 
        
        do j = 0, size(matrix, dim=3)-1
            do i = 0, size(matrix, dim=2)-1
                do m = 0, size(matrix, dim=1)-1
                    if (matrix(m, i, j) > EPSILON) then

                        ! print*, j, i, m, matrix(m, i, j)
                        call sparse_constructor%append_real(sparse_constructor%values, matrix(m, i, j), idx)
                        call sparse_constructor%append_int(sparse_constructor%row, i, idx)
                        call sparse_constructor%append_int(sparse_constructor%col, j, idx)
                        call sparse_constructor%append_int(sparse_constructor%materials, m, idx)

                        idx = idx + 1
                    end if
                end do
            end do
        end do

        sparse_constructor%upper_bound = idx - 1

        ! print*, '-----------------'
        ! set right neightbor index
        ! do j = 0, size(sparse_constructor%col)-1
        !     if (sparse_constructor%col(j) >= 1 .and. &
        !         sparse_constructor%row(j) >= 1 .and. &
        !         sparse_constructor%materials(j) >= 1) exit
        ! end do
        ! sparse_constructor%curr_idx = j

        ! print*, 'initial value', sparse_constructor%curr_idx, sparse_constructor%col(j),&
        !                             sparse_constructor%row(j), sparse_constructor%materials(j)

        ! ! set right neightbor index
        ! col_init_val = sparse_constructor%col(0)
        ! do j = 1, size(sparse_constructor%col)-1
        !     if (sparse_constructor%col(j) /= col_init_val) exit
        ! end do
        ! sparse_constructor%right_idx = j

        ! print*, 'col', sparse_constructor%col
        ! print*, 'row', sparse_constructor%row
        ! print*, 'materials', sparse_constructor%materials


    end function



    ! helper function - append val to array if it is not full, otherwise enlarge the array and append
    subroutine append_real(array, val, idx)
        implicit none
        real(8), dimension(:), allocatable, intent(inout) :: array
        real(8), intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        real(8), dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:16))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)                ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated

            ! allocate(temp(0:prev_size))            ! enlarge array size by factor of 2
            ! temp(0:prev_size) = array
            ! deallocate(array)
            ! allocate(array(0:prev_size*2))
            ! array = temp
            ! deallocate(temp)
        end if

        ! print*, allocated(array), size(array, dim=1), idx
        ! print*, 'a', idx, val
        array(idx) = val
        ! print*, array
    end subroutine


    subroutine append_int(array, val, idx)
        ! temporal solution
        implicit none
        integer, dimension(:), allocatable, intent(inout) :: array
        integer, intent(in) :: val
        integer, intent(in) :: idx

        integer :: prev_size
        integer, dimension(:), allocatable :: temp

        if (allocated(array)) then
            prev_size = size(array, dim=1)
        end if

        if (.not. allocated(array)) then
            allocate(array(0:16))
        else if (prev_size <= idx) then
            allocate(temp(0:prev_size*2))            ! enlarge array size by factor of 2
            temp(0:prev_size*2) = 0d0
            temp(0:prev_size) = array(0:prev_size)   ! copy previous values
            call move_alloc(temp, array)             ! temp gets deallocated
        end if

        array(idx) = val
    end subroutine


    integer function idx_exists(array, i, lower, upper)
        implicit none
        integer, dimension(:), allocatable :: array
        integer, intent(in) :: i
        integer :: min_limit, max_limit, mid_idx, range
        integer :: lower, upper

        min_limit = lower
        max_limit = upper

        ! if (present(lower)) min_limit = lower
        ! if (present(upper)) max_limit = upper

        ! write(*,*) 'tryna find', i
        ! write(*,*) 'find', lower, upper, array(lower), array(upper)

        range = max_limit - min_limit
        mid_idx = (min_limit + max_limit) / 2
        ! write(*,*) 'eeeeeeeeeee', mid_idx


        do while (array(mid_idx) /= i .and. range>0)            
            ! print*, min_limit, max_limit, mid_idx
            ! print*, array(mid_idx), i

            if (array(mid_idx) < i) then
                min_limit = mid_idx + 1
            else
                max_limit = mid_idx - 1
            end if

            range = max_limit - min_limit
            mid_idx = (min_limit + max_limit) / 2
        end do 

        if (array(mid_idx) /= i) then
            idx_exists = -1
        else 
            idx_exists = mid_idx
        end if 
        
    end function


    subroutine get_boundries(lower, upper, array, idx)
        implicit none
        integer, dimension(:), allocatable :: array
        integer, intent(in) :: idx
        integer, intent(inout) :: lower, upper
        integer :: val, i

        val = array(idx)
        ! write(*,*) 'bbbbbbb', idx, val, array(idx)

        do i = idx, 0, -1
            if (array(i) /= val) exit
            lower = i
        end do

        do i = idx, size(array, dim=1)
            if (array(i) /= val) exit
            upper = i
        end do

        ! write(*,*) 'done'


    end subroutine




end module