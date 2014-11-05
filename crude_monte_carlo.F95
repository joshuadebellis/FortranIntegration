!     
! File:   crude_monte_carlo.F95
! Author: josh
!
! Created on November 5, 2014, 5:22 PM
!

program crude_monte_carlo
    ! The crude Monte Carlo approximation uses a discretisation of the average 
    !value of a function formula from first year calculus.  Pseudo-random
    ! numbers are selected from a uniform distribution. For more information see
    ! http://en.wikipedia.org/wiki/Monte_Carlo_integration. a and b are the bounds of the integral and n is the integer
    ! number of samples.
    implicit none
    ! ----- variables for portable seed setting -----
    integer :: i_seed
    integer, dimension(:), allocatable :: a_seed
    integer, dimension(1:8) :: dt_seed
  ! ----- end of variables for seed setting -----
    real(8) :: r
    real (8) :: a,b
    real (8) :: answer, total
    integer :: n,k
    answer = 0.0
    total = 0.0

  ! ----- Set up random seed portably -----
    call random_seed(size=i_seed)
    allocate (a_seed(1:i_seed))
    call random_seed(get=a_seed)
    call date_and_time(values=dt_seed)
    a_seed(i_seed)=dt_seed(8); a_seed(1)=dt_seed(8)*dt_seed(7)*dt_seed(6)
    call random_seed(put=a_seed)
    deallocate (a_seed)
  ! ----- Done setting up random seed -----
    write(*,*), 'pick integral bounds a and b'
    read(*, *), a,b
    write(*,*), 'pick no of random numbers n'
    read(*, *), n
    do k = 1, n
        call random_number(r)
        total = total + f(r)
    end do
    answer = ( total / n) * (b - a)
    write(*, *), 'the answer is ' , answer
contains
    function f(x)
        real (8) :: f
        real (8) :: x
        f = x ** 2
    end function f
end program crude_monte_carlo
