module mat_array_module
    use sparse_struct_base_module

    type :: materials_t
        real(8), dimension(:), allocatable :: materials
    end type

    type, extends(sparse_struct_base_t) :: mat_array_t
        private 
        integer :: num_mats
        type(materials_t), dimension(:,:,:), allocatable :: mat

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: update_struct => update_struct
            procedure :: total_sum => total
    end type

    interface mat_array_t
        module procedure mat_array_constructor
    end interface

    contains


    function mat_array_constructor(num_materials, nx, ny, nz)
        type(mat_array_t), pointer :: mat_array_constructor
        integer, intent(in) :: nx, ny, nz, num_materials
        integer :: i, j, k

        allocate(mat_array_constructor)
        allocate(mat_array_constructor%mat(0:nx,0:ny,0:nz))
        mat_array_constructor%num_mats = num_materials
        

    end function


    subroutine add_item(this, material_type, i, j, k, val)
        implicit none
        class(mat_array_t), intent(inout) :: this
        integer, intent(in) :: material_type, i, j, k
        real(8):: val

        if (.not. allocated(this%mat(i,j,k)%materials)) allocate(this%mat(i,j,k)%materials(1:this%num_mats))

        this%mat(i,j,k)%materials(material_type) = val
    end subroutine


    pure function get_item(this, material_type, i, j, k)
        implicit none
        class(mat_array_t), intent(in) :: this
        integer, intent(in) :: i, j, k, material_type
        real(8) :: get_item

        if (.not. allocated(this%mat(i,j,k)%materials)) then 
            get_item = 0d0
            return
        end if

        get_item = this%mat(i,j,k)%materials(material_type)
    end function get_item


    function total(this)
        implicit none
        class(mat_array_t), intent(inout) :: this
        integer :: m, i, j, k
        real(8) :: total
        total = 0d0

        do k=0, size(this%mat, dim=3)-1
            do j=0, size(this%mat, dim=2)-1
                do i=0, size(this%mat, dim=1)-1
                    do m=1, this%num_mats
                        total = total + this%get_item(m,j,i,k)
                    end do
                end do
            end do
        end do
    end function


    subroutine update_struct(this, ms, is, js, ks, vals)
        implicit none
        class(mat_array_t), intent(inout) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ks, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: idx
        real(8) :: curr_val
        
        do idx=0, size(is)-1
            curr_val = vals(idx)

            if (is(idx) == -1) EXIT

            if (.not. allocated(this%mat(is(idx), js(idx), ks(idx))%materials)) then
                allocate(this%mat(is(idx), js(idx), ks(idx))%materials(1:this%num_mats))
            end if

            this%mat(is(idx), js(idx), ks(idx))%materials(ms(idx)) = vals(idx)
        end do

    end subroutine

end module