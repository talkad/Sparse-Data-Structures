module naive_amr_module
    use sparse_struct_base_module

    type :: sub_space_t
        real(8), dimension(:,:,:), allocatable :: sub_space
    end type

    type, extends(sparse_struct_base_t) :: naive_amr_t
        private 
        integer :: block_size
        type(sub_space_t), dimension(:,:,:,:), allocatable :: grid

        contains

            procedure :: get_item => get_item
            procedure :: add_item => add_item
            procedure :: update_struct => update_struct
    end type

    interface naive_amr_t
        module procedure naive_amr_constructor
    end interface

    contains


    function naive_amr_constructor(block_size, num_materials, nx, ny, nz)
        type(naive_amr_t), pointer :: naive_amr_constructor
        integer, intent(in) :: nx, ny, nz, num_materials, block_size
        integer :: i, j, k, dim

        allocate(naive_amr_constructor)
        allocate(naive_amr_constructor%grid(1:num_materials,0:nx/block_size + 1,0:ny/block_size + 1,0:nz/block_size + 1))
        naive_amr_constructor%block_size = block_size      

    end function


    subroutine add_item(this, material_type, i, j, k, val)
        implicit none
        class(naive_amr_t), intent(inout) :: this
        integer, intent(in) :: material_type, i, j, k
        integer :: i_new, j_new, k_new, i_sub, j_sub, k_sub
        real(8):: val

        i_new = i / this%block_size
        j_new = j / this%block_size
        k_new = k / this%block_size

        i_sub = modulo(i, this%block_size)
        j_sub = modulo(j, this%block_size)
        k_sub = modulo(k, this%block_size)

        if (.not. allocated(this%grid(material_type,i_new,j_new,k_new)%sub_space)) then
            allocate(this%grid(material_type,i_new,j_new,k_new)% &
                sub_space(0:this%block_size-1,0:this%block_size-1,0:this%block_size-1))
        end if

        this%grid(material_type,i_new,j_new,k_new)%sub_space(i_sub,j_sub,k_sub) = val
    end subroutine


    function get_item(this, material_type, i, j, k)
        implicit none
        class(naive_amr_t), intent(inout) :: this
        integer, intent(in) :: i, j, k, material_type
        real(8) :: get_item
        integer :: i_new, j_new, k_new, i_sub, j_sub, k_sub

        i_new = i / this%block_size
        j_new = j / this%block_size
        k_new = k / this%block_size

        if (.not. allocated(this%grid(material_type,i_new,j_new,k_new)%sub_space)) then 
            get_item = 0d0
            return
        end if

        i_sub = modulo(i, this%block_size)
        j_sub = modulo(j, this%block_size)
        k_sub = modulo(k, this%block_size)

        get_item = this%grid(material_type,i_new,j_new,k_new)%sub_space(i_sub,j_sub,k_sub)
    end function get_item


    subroutine update_struct(this, ms, is, js, ks, vals)
        implicit none
        class(naive_amr_t), intent(inout) :: this
        integer, dimension(:), allocatable, intent(in) :: is, js, ks, ms
        real(8), dimension(:), allocatable, intent(in) :: vals
        integer :: idx
        real(8) :: curr_val
        integer :: i_new, j_new, k_new, i_sub, j_sub, k_sub
        
        do idx=0, size(is)-1
            curr_val = vals(idx)

            i_new = is(idx) / this%block_size
            j_new = js(idx) / this%block_size
            k_new = ks(idx) / this%block_size

            i_sub = modulo(is(idx), this%block_size)
            j_sub = modulo(js(idx), this%block_size)
            k_sub = modulo(ks(idx), this%block_size)

            if (.not. allocated(this%grid(ms(idx),i_new,j_new,k_new)%sub_space)) then
                allocate(this%grid(ms(idx),i_new,j_new,k_new)% & 
                    sub_space(0:this%block_size-1,0:this%block_size-1,0:this%block_size-1))
                        this%grid(ms(idx),i_new,j_new,k_new)% &
                            sub_space(0:this%block_size-1,0:this%block_size-1,0:this%block_size-1) = 0
            end if

            this%grid(ms(idx),i_new,j_new,k_new)%sub_space(i_sub,j_sub,k_sub) = curr_val
        end do


    end subroutine

end module