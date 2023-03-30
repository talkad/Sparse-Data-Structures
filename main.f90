PROGRAM MAIN  
    use omp_lib

    integer :: a, idx
    real(8) :: time

    a = 0

    time = omp_get_wtime()
    do idx=0, 500000
        a = a + func(a)
    end do

    write(*,*), 'result ', a, 'execution time ', omp_get_wtime()-time
    
END PROGRAM MAIN

function func(a) result(b)
    integer, intent(in) :: a

    b = a +1
end function
